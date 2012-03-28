{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.Base
( module Pec.Base
, module Language.Pir.Abs
, unused
)

where

import Control.Concurrent
import Control.Monad.State
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Development.Shake.FilePath
import Distribution.Text
import Grm.Prims
import Language.Pir.Abs hiding (Exp(..))
import Paths_pec
import Pec.C
import Pec.IUtil (vtvar, gTyDecls)
import Pec.PUtil
import Prelude hiding (exp)
import System.Console.CmdArgs hiding (atom)
import System.IO.Unsafe
import qualified Language.Pir.Abs as I
import qualified Pec.LLVM as L

data Args = Args
  { march :: Arch
  , readable :: Bool
  } deriving (Show, Data, Typeable)

argsDesc :: Args
argsDesc = Args
  { march = def &= help "arch to build (C or LLVM)"
  , readable = def &= help "generate human readable C (experimental)"
  } &= summary summry &= program prog

summry :: String
summry = prog ++ " v" ++ display version ++ ", " ++ copyright

prog :: String
prog = "pecgen"

data E a = E Exp deriving Show

unE :: Typed a => E a -> Exp
unE = f (error "unused:unE")
  where
    f :: Typed a => a -> E a -> Exp
    f a (E x) = seq (addGTyDecls $ tydecls a) x
    
setE :: Typed a => Exp -> E a
setE = f (error "unused:setE")
  where
    f :: Typed a => a -> Exp -> E a
    f a x = seq (addGTyDecls $ tydecls a) $ E x

data Exp
  = VarE TVar
  | AppE Exp Exp
  | SwitchE Exp Exp [(Exp,Exp)]
  | LitE TLit
  | LetE TVar Exp Exp
  | LamE TVar (Exp -> Exp)
  | DefE TVar Exp
        
instance Show Exp where -- for debugging
  show x = case x of
    VarE a -> unwords ["VarE", show a]
    AppE a b -> unwords ["AppE", show a, show b]
    SwitchE a b c -> unwords ["SwitchE", show a, show b, show c]
    LitE a -> unwords ["LitE", show a]
    LetE a b c -> unwords ["LetE", show a, show b, show c]
    LamE a _ -> unwords ["LamE", show a]
    DefE a b -> unwords ["DefE", show a, show b]
           
apps :: [Exp] -> Exp
apps = foldl1 AppE

type M a = State St a
  
data St = St
  { stmts :: [Stmt]
  }

tatom :: Atom -> Type
tatom x = case x of
  VarA (TVar _ a) -> a
  LitA (TLit _ a) -> a

isVoidA :: Atom -> Bool
isVoidA = isVoidTy . tatom

isVoidE :: I.Exp -> Bool
isVoidE = isVoidTy . texp

isVoidV :: TVar -> Bool
isVoidV = isVoidTy . ttvar

fNoOpS :: [Stmt] -> Maybe [Stmt]
fNoOpS xs | any ((==) NoOpS) xs = Just $ filter ((/=) NoOpS) xs
fNoOpS _ = Nothing

fVoidS :: Stmt -> Maybe Stmt
fVoidS (ReturnS (LitA (TLit VoidL _))) = Nothing
fVoidS (ReturnS a) | isVoidA a = Just $ ReturnS voidA
fVoidS (LetS _ b) | isVoidE b = case b of
  I.CallE a bs -> Just $ CallS a bs
  _ -> Just NoOpS
fVoidS (CallS a bs) | any isVoidA bs =
  Just $ CallS a $ filter (not . isVoidA) bs
  -- ^ type of a is no longer correct (it may contain void types)
fVoidS (StoreS _ b) | isVoidA b = Just NoOpS
fVoidS _ = Nothing

fVoidE :: I.Exp -> Maybe I.Exp
fVoidE (I.CallE a bs) | any isVoidA bs =
  Just $ I.CallE a $ filter (not . isVoidA) bs
  -- ^ type of a is no longer correct (it may contain void types)
fVoidE _ = Nothing

fVoidD :: Define -> Maybe Define
fVoidD (Define a b cs ds) | any isVoidV cs =
  Just $ Define a b (filter (not . isVoidV) cs) ds
fVoidD _ = Nothing

texp :: I.Exp -> Type
texp x = case x of
  I.CastE _  b -> b
  I.AllocaE a -> tyPtr a
  I.AtomE a -> tatom a
  I.LoadE a -> unTyPtr $ ttvar a
  I.CallE a bs -> tcall (ttvar a) $ map tatom bs

tcall :: Type -> [Type] -> Type
tcall a bs = case splitAt (length ts) bs of
  (_,[]) -> t
  (_,cs) -> tcall t cs
  where (t,ts) = unFunTy a

tyRecord :: [(String,Type)] -> TyDecl
tyRecord xs = TyRecord [ FieldT a b | (a,b) <- xs]

tyEnum :: [String] -> TyDecl
tyEnum bs = TyEnum $ map EnumC bs

initSt :: St
initSt = St{ stmts = [] }

stmt :: Stmt -> M ()
stmt x = modify $ \st -> st{ stmts = x : stmts st }

pop_block :: M [Stmt]
pop_block = do
  ss0 <- gets stmts
  modify $ \st -> st{ stmts = [] }
  return $ reverse ss0

push_block :: [Stmt] -> M ()
push_block x = modify $ \st -> st{ stmts = reverse x ++ stmts st }

block :: (Exp -> M a) -> Exp -> M (a,[Stmt])
block f a = do
  ss0 <- pop_block
  x <- f a
  ss1 <- pop_block
  push_block ss0
  return (x,ss1)
 
block_ :: (Exp -> M Atom) -> Exp -> M [Stmt]
block_ f a = liftM snd $ block f a

assignAtom :: I.Exp -> M Atom
assignAtom x = do
  v <- fresh (texp x)
  stmt $ LetS v x
  return $ VarA v

ifSwitchS :: Atom -> [Stmt] -> [SwitchAlt] -> M [Stmt]
ifSwitchS _ ys [] = return ys
ifSwitchS x ys (SwitchAlt a bs : zs) = do
  ss <- ifSwitchS x ys zs
  v <- assignAtom $ I.CallE strEqE [x, LitA a]
  return [IfS v bs ss]

strEqE :: TVar
strEqE = TVar "eq" $ tyFun tyIString (tyFun tyIString tyBool)

ttvar :: TVar -> Type
ttvar (TVar _ b) = b

atom :: Exp -> M Atom
atom x = case x of
  VarE a -> return $ VarA a
  LitE a -> return $ LitA a
  DefE a _ -> return $ VarA a
  _ -> expr x >>= assignAtom

tvar :: Exp -> M TVar
tvar x = do
  a <- atom x
  case a of
    VarA v -> return v
    _ -> error $ "expected variable:" ++ ppShow a

exprFun :: Type -> Exp -> [Exp] -> M I.Exp
exprFun t y ys = do
  v <- tvar y
  case (v,ys) of
    (TVar "load" _, [a]) -> liftM I.LoadE $ tvar a
    (TVar "then" _, [a, b]) -> do
      ss <- block_ atom a
      push_block ss
      expr b
    (TVar "if" _, [a, b, c]) -> do
      r <- fresh (tyPtr t)
      stmt $ LetS r $ I.AllocaE t
      e <- atom a
      bb <- block_ (store r) b
      bc <- block_ (store r) c
      stmt $ IfS e bb bc
      return $ I.LoadE r
    (TVar "unsafe_cast" _, [a]) -> do
      e <- atom a
      case e of
        LitA (TLit l _) -> return $ I.AtomE $ LitA $ TLit l t
        VarA b -> return $ I.CastE b t
    (TVar "when" _, [a, b]) -> do
      e <- atom a
      bb <- block_ atom b
      stmt $ WhenS e bb
      return voidE
    (TVar "while" _, [a, b]) -> do
      (e,aa) <- block atom a
      bb <- block_ atom b
      stmt $ WhileS aa e bb
      return voidE
    (TVar "store" _, [a, b]) -> do
      r <- tvar a
      e <- atom b
      stmt $ StoreS r e
      return voidE
    _ -> do
      es <- mapM atom ys
      return $ I.CallE v es

unApp :: Exp -> [Exp]
unApp x = case x of
  AppE a b -> unApp a ++ [b]
  _ -> [x]

unFunTy :: Type -> (Type, [Type])
unFunTy x = case x of
  Type "Fun_" ts -> (last ts, init ts)
  _ -> error $ "function type expected:" ++ ppShow x
  
expr :: Exp -> M I.Exp
expr x = case x of
  DefE a _ -> case a of
    TVar "unsafe_alloca" t -> return $ I.AllocaE (unTyPtr t)
    _ -> return $ I.AtomE $ VarA a
  VarE a -> return $ I.AtomE $ VarA a
  LitE{} -> liftM I.AtomE $ atom x
  AppE{} -> do
    let (y:ys) = unApp x
    let (_,ts) = unFunTy $ tof y
    case splitAt (length ts) ys of
      (bs,[])
        | length bs < length ts ->
          error $ "no partial application:" ++ show x
        | otherwise -> exprFun (tof x) y ys
      (bs,cs) -> do
        let e = apps (y:bs)
        v <- fresh (tof e)
        expr $ LetE v e (apps (VarE v : cs))
  SwitchE a b cs -> do
    let t = tof b
    v <- fresh $ tyPtr t
    stmt $ LetS v $ I.AllocaE t
    e <- atom a
    dflt <- block_ (store v) b
    alts <- mapM (alt v) cs
    if tof a == tyIString
      then ifSwitchS e dflt alts >>= mapM_ stmt -- will have void type
      else stmt $ SwitchS e dflt alts
    return $ I.LoadE v
  LetE a b c -> do
    e <- expr b
    stmt $ LetS a e
    expr c
  LamE{} -> error "unapplied lamda expression"

alt :: TVar -> (Exp,Exp) -> M SwitchAlt
alt a (LitE b, c) = do
  ss <- block_ (store a) c
  return $ SwitchAlt b ss
alt a (AppE (b@LitE{}) _, c) = alt a (b,c)
alt a (b,c) = error $ "alt:pattern match failed:" ++ show (a,b,c)

store :: TVar -> Exp -> M Atom
store a b = do
  e <- atom b
  stmt $ StoreS a e
  return voidA

fresh :: Type -> M TVar
fresh a = return $ TVar (uId a "v") a

fExitS :: [Stmt] -> Maybe [Stmt]
fExitS ss = case break isExitS ss of
  (_,[]) -> Nothing
  (_,[_]) -> Nothing
  (bs,c:_) -> Just $ bs ++ [c]
  where
  isExitS (CallS a _) = vtvar a == "exit"
  isExitS _ = False

fVoidT :: Type -> Maybe Type
fVoidT (Type "Fun_" xs0) | any isVoidTy xs =
  Just $ Type "Fun_" $ filter (not . isVoidTy) xs ++ [x]
  where
    xs = init xs0
    x = last xs0
fVoidT _ = Nothing

fSynT :: Type -> Maybe Type
fSynT (Type a _) = case a of
  "Idx_" -> Just $ I.Type "W_" [I.Type "Cnt32" []]
  "IString_" -> Just $ I.Type "Ptr_" [I.Type "Char_" []]
  _ -> Nothing

fCastE :: I.Exp -> Maybe I.Exp
fCastE (I.CastE a b) | ttvar a == b = Just $ I.AtomE $ VarA a
fCastE _ = Nothing

dModule :: FilePath -> String -> [String] -> [Define] -> IO ()
dModule outdir a bs cs = do
  let m = Module a (map Import bs) cs
  let m1 = 
        rewriteBi fCastE $
        rewriteT $
        rewriteBi fExitS $
        rewriteBi fNoOpS $
        rewriteBi fVoidD $
        rewriteBi fVoidE $
        rewriteBi fVoidS m
  x <- cmdArgs argsDesc
  case march x of
    C -> cModules outdir (readable x) m1
    LLVM -> L.dModule outdir m1

rewriteT :: Data a => a -> a
rewriteT x = rewriteBi fVoidT $ rewriteBi fSynT x

addGTyDecls :: [(Type,TyDecl)] -> ()
{-# NOINLINE addGTyDecls #-}
addGTyDecls xs = unsafePerformIO $ modifyMVar_ gTyDecls $ \ys ->
  return $ union (rewriteT xs) ys

defn :: Typed a => E a -> Define
defn x = case unE x of
  DefE (TVar a0 t) b -> flip evalState initSt $ do
    let a = if a0 == "main_" then "main" else a0
    let (vs,c) = unLam b
    e <- atom c
    ss <- pop_block
    return $ Define (fst $ unFunTy t) a vs $ ss ++ [ReturnS e]
  _ -> error "defn"

unLam :: Exp -> ([TVar],Exp)
unLam x = case x of
  LetE a b c -> let (vs,e) = unLam c in (vs, LetE a b e)
  LamE (TVar a b) f -> let (vs,e) = unLam $ f $ VarE v in (v:vs, e)
    where v = TVar (a ++ "_") b
  _ -> ([],x)
    
appE :: (Typed a, Typed b) => E (a -> b) -> E a -> E b
appE a b = case unE a of
  LamE _ f -> setE (f $ unE b)
  _ -> setE (AppE (unE a) (unE b))
  
data Array_ cnt a
data Pointer_ p a

class Load_ a
class Store_ a

data IString_

data I_ a
data W_ a
data Idx_ a

instance Count a => Arith_ (I_ a)
instance Count a => Arith_ (W_ a)
instance Arith_ Double_
instance Arith_ Float_

instance Floating_ Double_
instance Floating_ Float_

instance Count a => Nmbr (I_ a)
instance Count a => Nmbr (W_ a)
instance Count a => Nmbr (Idx_ a)
instance Nmbr Double_
instance Nmbr Float_

class Ord_ a
class Eq_ a

instance Eq_ Char_
instance Eq_ IString_

instance Count a => Ord_ (I_ a)
instance Count a => Ord_ (W_ a)
instance Ord_ Char_
instance Ord_ Double_
instance Ord_ Float_

instance Count a => Eq_ (W_ a)
instance Count a => Eq_ (I_ a)
instance Count a => Eq_ (Idx_ a)

count_ :: (Count ca, Count cb, Typed a, Typed p) =>
  E (Pointer_ p (Array_ ca a) -> W_ cb)
count_ = lamE "" f
  where
  f :: (Count ca, Count cb, Typed a, Typed p) =>
       E (Pointer_ p (Array_ ca a)) -> E (W_ cb)
  f (_ :: E (Pointer_ p (Array_ cnt a))) =
    nmbrE (show $ countof (unused :: cnt))

data Char_
data Double_
data Float_

data Tag a

class Typed a => Count a where
  countof :: a -> Integer
  idx_max_ :: E (Idx_ a)
  idx_max_ = nmbrE (show $ pred $ countof (unused :: a))

instance Tagged a => Tagged (Tag a) where
  tags (_ :: Tag a) = tags (unused :: a)

class StorePtr a
class Typed a => Nmbr a
class Nmbr a => Arith_ a
class Floating_ a

tyPair :: Type -> Type -> Type
tyPair a b = Type "Pair_" [a,b]

unTyPtr :: Type -> Type
unTyPtr (Type _ [a]) = a
unTyPtr _ = error "unTyPtr"

tyPtr :: Type -> Type
tyPtr a = Type "Ptr_" [a]

tyBool :: Type
tyBool = tyPrim "Bool_"

enumTyDecls :: Typed a => [String] -> a -> [(Type, TyDecl)]
enumTyDecls ss a = [(ty a, tyEnum ss)]

class Tagged a where
  tags :: a -> [String]
  
taggedTyDecls :: Typed a =>
  [[(Type,TyDecl)]] -> [(String,Type)] -> a -> [(Type, TyDecl)]
taggedTyDecls xs ys z = nub $ concat xs ++
  [ (Type (s ++ "tag") [], tyEnum $ map fst ys)
  , (t, TyTagged [ ConC a b | (a,b) <- ys ])
  ]
  where
    t@(Type s _) = ty z

recordTyDecls :: Typed a =>
  [[(Type,TyDecl)]] -> [(String,Type)] -> a -> [(Type, TyDecl)]
recordTyDecls xs ys z =
  nub $ concat xs ++ [(ty z, TyRecord [ FieldT a b | (a,b) <- ys ])]

class Typed a where
  ty :: a -> Type
  tydecls :: a -> [(Type, TyDecl)]
  tydecls _ = []

tydecls_ :: (Typed a, Typed b) => a -> b -> [(Type, TyDecl)]
tydecls_ a _ = tydecls a

instance Count a => Typed (I_ a) where
  ty _ = Type "I_" [ty (unused :: a)]
  
instance Count a => Typed (W_ a) where
  ty _ = Type "W_" [ty (unused :: a)]

instance Count a => Typed (Idx_ a) where
  ty _ = Type "Idx_" [ty (unused :: a)]

instance Typed a => Typed (Tag a) where
  ty _ = Type (s ++ "tag") []
    where I.Type s _ = ty (unused :: a)

instance Typed () where
  ty _ = tyVoid
  
instance (Typed a, Typed b) => Typed (a -> b) where
  ty _ = tyFun (ty (unused :: a)) (ty (unused :: b))
  tydecls _ =
    tydecls (unused :: a) ++ tydecls (unused :: b)

instance (Count cnt, Typed a) => Typed (Array_ cnt a) where
  ty _ = tyArray (countof (unused :: cnt)) (ty (unused :: a))
  tydecls _ = tydecls (unused :: a)
  
tyArray :: Integer -> Type -> Type
tyArray a b = Type "Array_" [tyCnt a, b]

instance Typed Char_ where
  ty _ = tyChar
  
instance Typed Double_ where
  ty _ = tyDouble
  
instance Typed Float_ where
  ty _ = tyFloat
  
instance Typed IString_ where
  ty _ = tyIString
  
instance (Typed p, Typed a) => Typed (Pointer_ p a) where
  ty _ = tyPtr (ty (unused :: a))
  tydecls _ = tydecls (unused :: a)

isVoidTy :: Type -> Bool
isVoidTy = (==) tyVoid

tyVoid :: Type
tyVoid = tyPrim "Void_"

tyPrim :: String -> Type
tyPrim a = Type a []

letE :: (Typed a, Typed b) => String -> E a -> (E a -> E b) -> E b
letE a0 b f =
  let a = uId a0 a0 in
    setE (LetE (TVar a (tof $ unE b)) (unE b) $ unE $ f $ varE a)

tyFun :: Type -> Type -> Type
tyFun a b = case b of
  Type "Fun_" cs -> Type "Fun_" (a:cs)
  _ -> Type "Fun_" [a,b]

tof :: Exp -> Type
tof x = case x of
  VarE a -> ttvar a
  DefE a _ -> ttvar a
  LamE (TVar a b) f -> tyFun b (tof $ f $ VarE $ TVar a b)
  AppE a _ -> case tail ts of
    [] -> t
    bs -> Type "Fun_" $ bs ++ [t]
    where (t,ts) = unFunTy $ tof a
  SwitchE _ b _ -> tof b
  LitE (TLit _ b) -> b
  LetE _ _ c -> tof c

fixArity :: Int -> Type -> Type
fixArity 0 x = x
fixArity n x = case splitAt n ts of
  (_,[]) -> x
  (bs,cs) -> Type "Fun_" $ bs ++ [Type "Fun_" $ cs ++ [t]]
  where
    (t,ts) = unFunTy x
    
arityDefE :: Typed a => Int -> String -> E a -> E a
arityDefE n a b = setE (DefE (TVar a (fixArity n $ tof e)) e)
  where e = unE b

defE :: Typed a => String -> E a -> E a
defE a b = arityDefE (arityDef $ unE b) a b

arityDef :: Exp -> Int
arityDef = length . fst . unLam

extern :: Typed a => E (IString_ -> IString_ -> a)
extern = lamE "" $ \x -> lamE "" $ \y ->
  let v = unStringE x in arityDefE (read $ unStringE y) v (varE v)

unStringE :: E IString_ -> String
unStringE x = case unE x of
  LitE (TLit (StringL s) _) -> s
  _ -> error "unStringE"

varE :: Typed a => String -> E a
varE = f (error "unused:varE")
  where
  f :: Typed a => a -> String -> E a
  f a s = setE (VarE $ TVar s (ty a))

lamE :: (Typed a, Typed b) => String -> (E a -> E b) -> E (a -> b)
lamE s (f :: (E a -> E b)) =
  setE (LamE (TVar s (ty (unused :: a))) (\e -> unE (f (setE e))))

switchE :: (Typed a, Typed b) => E a -> E b -> [(E a, E b)] -> E b
switchE a b cs = setE (SwitchE (unE a) (unE b)
                    [ (unE x, unE y) | (x,y) <- cs ])

switchE_ :: (Tagged a, Typed a, Typed b) => E a -> [(E a, E b)] -> E b
switchE_ (a :: E a) bs = case tags (unused :: a) \\ xs of
  [] -> switchE a (snd $ last bs) (init bs)
  ts -> error $ "unmatched tag(s):" ++ unwords (take 10 ts)
  where
    xs = map (get_tag . unE . fst) bs

get_tag :: Exp -> String
get_tag x = case x of
  AppE a _ -> get_tag a
  LitE (TLit (EnumL a) _) -> a
  _ -> error "unused:get_tag"
  
litE :: Lit -> Type -> Exp
litE a b = LitE (TLit a b)

charE :: Char -> E Char_
charE x = setE $ litE (CharL x) tyChar

stringE :: String -> E IString_
stringE x = setE $ litE (StringL x) tyIString

nmbrE :: Nmbr a => String -> E a
nmbrE = f (error "unused:nmbrE")
  where
  f :: Nmbr a => a -> String -> E a
  f a i = setE $ litE (NmbrL i) (ty a)

tyChar :: Type
tyChar = tyPrim "Char_"

tyDouble :: Type
tyDouble = tyPrim "Double_"

tyFloat :: Type
tyFloat = tyPrim "Float_"

tyIString :: Type
tyIString = tyPrim "IString_"

tyCnt :: Integer -> Type
tyCnt x = tyPrim ("Cnt" ++ show x)

un :: (Typed a, Typed b, Typed p, Load_ p) =>
      E (IString_ -> Pointer_ p a -> b)
un = varE "un"

tg :: (Typed a) => E (IString_ -> a)
tg = f (error "unused:tg")
  where
    f :: (Typed a) => a -> E (IString_ -> a)
    f a = lamE "" $ \x -> setE $ litE (EnumL $ unStringE x) (ty a)

storeE :: (Typed a, Typed p, Store_ p) => E (Pointer_ p a -> a -> ())
storeE = varE "store"

fld :: (Typed a, Typed b, Typed p) =>
       E (IString_ -> Pointer_ p a -> Pointer_ p b)
fld = lamE "" $ \x -> lamE "" $ \y -> appE (varE $ unStringE x ++ "fld") y
         
unwrap_ :: (Typed a, Typed b) => E (IString_ -> a -> b)
unwrap_ = lamE "" $ \_ -> lamE "" $ \a -> setE (unE a)

unwrapptr_ :: (Typed a, Typed b, Typed p) =>
              E (IString_ -> Pointer_ p a -> Pointer_ p b)
unwrapptr_ = lamE "" $ \_ -> lamE "" $ \a -> setE (unE a)

mk :: (Typed a) => E (IString_ -> a)
mk = varE "mk"

uni :: (Typed a) => E (IString_ -> a)
uni = lamE "" $ \_ -> setE (LitE voidL)

voidL :: TLit
voidL = TLit VoidL tyVoid

voidA :: Atom
voidA = LitA voidL

voidE :: I.Exp
voidE = I.AtomE voidA

nt :: (Typed a, Typed b) => E (IString_ -> a -> b)
nt = lamE "" $ \_ -> lamE "" $ \a -> setE (unE a)

tagv :: (Typed a, Typed b, Typed p, Load_ p) => E (Pointer_ p a -> b)
tagv = varE "tagv"

unsafe_cast_ :: (Typed a, Typed b) => E (a -> b)
unsafe_cast_ = varE "unsafe_cast"
