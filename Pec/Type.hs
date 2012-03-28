-- NOTE: This module is not currently in use.

module Pec.Type where

import Control.Monad.State
import Data.Generics.Uniplate.Data hiding (contexts)
import Data.List
import Grm.Prims
import Language.Pds.Abs

data St = St
  { env :: [(String,Type)]
  , constraints :: [Constraint]
  , contexts :: [Cxt]
  , knowns :: [(String,Type)]
  } deriving Show
  
initSt = St [] [] [] []

foo e = runState (typeOf e >>= \t -> solve >> return t) initSt

letE a b c = LetE a undefined b c
charE = LitE . CharL
stringE = LitE . StringL
nmbrE = LitE . NmbrL

solve :: M ()
solve = do
  mc <- popConstraint
  case mc of
    Nothing -> return ()
    Just (a, b) -> do
      case (a,b) of
        (TyCxt cxts c, _) -> do
          pushContexts cxts
          pushConstraint (c, b)
        (_, TyCxt cxts c) -> do
          pushContexts cxts
          pushConstraint (a,c)
        (TyFun c d, TyFun e f) -> do
          pushConstraint (c, e)
          pushConstraint (d, f)
        (TyVoid, TyVoid) -> return ()
        (TyConstr c ds, TyConstr e fs) | c == e ->
          mapM_ pushConstraint $ zip ds fs
        (TyVarT c, TyVarT d) | c == d -> return ()
        (_, TyVarT v) -> pushKnown v a -- BAL: one of these should be less powerful than the other
        (TyVarT v, _) -> pushKnown v b -- BAL: one of these should be less powerful than the other
        _ -> error $ "unable to resolve constraints:" ++ ppShow a ++ " ? " ++ ppShow b
      solve

subst :: String -> Type -> Constraint -> Constraint
subst v0 t = transformBi f
  where f x = case x of
          TyVarT v | v == v0 -> t
          _ -> x
          
pushKnown v0 t | v0 `elem` [ v | TyVarT v <- universe t ] = error $ "infinite type:" ++ v0 ++ " in " ++ ppShow t
pushKnown v t = do
  modify $ \st -> st{ knowns = (v,t) : knowns st
                    , constraints = map (subst v t) $ constraints st }
  
type M a = State St a

(..=..) :: Type -> Type -> M ()
ta ..=.. tb = do
  pushConstraint (ta, tb)
  pushConstraint (tb, ta)
  
(.=..) :: M Type -> Type -> M ()
f .=.. tb = do
  ta <- f
  ta ..=.. tb
  
(.=.) :: M Type -> M Type -> M ()
f .=. g = do
  tb <- g
  f .=.. tb
  
pushConstraint :: Constraint -> M ()
pushConstraint x = modify $ \st -> st{ constraints = x : constraints st }

pushContexts xs = modify $ \st -> st{ contexts = nub (xs ++ contexts st) } -- BAL: this nub is pretty slow, can just do it once at the end

(.<=..) :: M Type -> Type -> M ()
f .<=.. tb = do
  ta <- f
  pushConstraint (ta, tb)

popConstraint = do
  xs0 <- gets constraints
  case xs0 of
    [] -> return Nothing
    (x:xs) -> do
      modify $ \st -> st{ constraints = xs }
      return $ Just x
      
type Constraint = (Type,Type)
           
type Class = String

declare :: String -> M Type
declare a = do
  ta <- freshType a
  pushEnv a ta
  return ta
  
typeOfList :: [Exp] -> M Type
typeOfList (x:xs) = do
  t <- typeOf x
  mapM_ (\e -> typeOf e .=.. t) xs
  return t
typeOfList [] = error "typeOfList:empty list"  

typeOf :: Exp -> M Type
typeOf x = case x of  
  SwitchE a b cs -> do
    typeOf a .=. typeOfList [ d | SwitchAlt d _ <- cs ]
    typeOfList (fromDefault b ++ [ e | SwitchAlt _ e <- cs ]) >>= myType
  LetE a _ b c -> do
    declare a .=. typeOf b
    typeOf c >>= myType
  LamE a b -> do
    ta <- freshType a
    tb <- freshType b
    declare a .=.. ta
    typeOf b .=.. tb
    myType $ TyFun ta tb
  AppE a b -> do
    ta <- freshType a
    tb <- freshType b
    typeOf a .=.. TyFun tb ta
    typeOf b .=.. tb
    myType ta
  AscribeE a b -> do
    typeOf a .<=.. b
    myType b
  VarE a -> typeOfVar a >>= myType
  LitE a -> typeOfLit a >>= myType
  where
    myType tx = do
      return tx
      
typeOfVar :: String -> M Type
typeOfVar v = do
  env <- getEnv
  case lookup v env of
    Just t -> return t
    Nothing -> error $ "unknown name:" ++ v
    
typeOfLit :: Lit -> M Type
typeOfLit x = case x of
  CharL _ -> return $ TyConstr "Char" []
  StringL _ -> return $ TyConstr "IString" []
  NmbrL _ -> do
    ta <- freshTyVar x
    return $ TyCxt [Cxt "Nmbr" [Var ta]] $ TyVarT ta -- BAL: make contexts just hold 1 type, not a list

fromDefault :: Default -> [Exp]
fromDefault x = case x of
  DefaultNone -> []
  DefaultSome a -> [a]

freshTyVar :: a -> M String
freshTyVar a = return $ uId a "a"

freshType :: a -> M Type
freshType a = liftM TyVarT (freshTyVar a)

getEnv :: M [(String,Type)]
getEnv = gets env

pushEnv :: String -> Type -> M ()
pushEnv v t = modify $ \st -> st{ env = (v,t) : env st }
