{-# OPTIONS -Wall #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.C (cModules) where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Development.Shake.FilePath
import Grm.Prims
import Language.C.Abs
import Pec.IUtil
import qualified Language.Pir.Abs as I

cModules :: FilePath -> Bool -> I.Module -> IO ()
cModules outdir is_readable (I.Module a _bs cs) = do
  let defs = map cDefine cs
  writeFileBinary (joinPath [outdir, n ++ ".c"]) $ ppShow $
    cleanup $ optimize $ dModule is_readable $
    CModule [Import hnfn] defs
  xs <- liftM (nub . map cTypeD) $ readMVar gTyDecls
  let ifvs = nub [ ifv | ifv@(I.TVar v _) <- concatMap fvsIDefine cs
                       , v `notElem` cBuiltins ]
  writeFileBinary (joinPath [outdir, hnfn]) $ ppShow $
    cleanup $
    transformBi tArrayArgTy $
    HModule hn hn imps $ xs ++ map (Declare . cFunDecl) ifvs
  where
  hn = n ++ "_H"
  hnfn = n ++ ".h"
  n = case a of
    "" -> error "unused:cModules"
    _ -> init a
  imps = map GImport
    [ "stdio.h"
    , "stdint.h"
    , "stdlib.h" 
    , "string.h"
    ]

cBuiltins :: [String]
cBuiltins =  [ "puts", "putchar", "strlen", "strncpy", "strcmp"] -- BAL: figure out how to handle this generically

cTypeD :: (I.Type, I.TyDecl) -> Declare
cTypeD (x@(I.Type s _),y) = Typedef $ Decl t (tyName x)
  where
  t = case y of
    I.TyEnum bs -> TyEnum [ EnumC b | I.EnumC b <- bs ]
    I.TyRecord bs -> TyStruct [ cDecl $ I.TVar c d | I.FieldT c d <- bs ]
    I.TyTagged bs -> TyStruct
      [ Decl (TyName $ s ++ "tag") "tag"
      , Decl (TyUnion [ cDecl $ I.TVar c d | I.ConC c d <- bs
                                           , d /= tyVoid ]) "data"
      ]

cDecl :: I.TVar -> Decl
cDecl (I.TVar a b) = Decl (cType b) a

cType :: I.Type -> Type
cType x@(I.Type a bs) = case a of
  "Fun_" -> TyFun (cType $ last bs) (map cType $ init bs)
  "Ptr_" -> case bs of
    [t] -> TyPtr (cType t)
    _ -> error "cType:TyPtr"
  "Array_" -> case bs of
    [c,d] -> TyArray (cType d) (nCnt [c])
    _ -> error "cType:TyArray"
  _ -> cTyName x

cTyName :: I.Type -> Type
cTyName = TyName . tyName

tyName :: I.Type -> String
tyName (I.Type a bs) = case (a,bs) of
  ("Void_",[]) -> "void"
  ("W_",_) -> "uint" ++ (promote $ nCnt bs) ++ "_t"
  ("I_",_) -> "int" ++ (promote $ nCnt bs) ++ "_t"
  ("Double_",[]) -> "double"
  ("Float_",[]) -> "float"
  ("Char_",[]) -> "char"
  ("Fun_",_) -> tyName (I.Type (a ++ show (length bs)) bs)
  _ -> mkTyConstr (a : map tyName bs)

mkTyConstr :: [String] -> String
mkTyConstr ss = concat $ intersperse "_" $ map strip_underscore ss

cDefine :: I.Define -> Define
cDefine x@(I.Define a b cs d) =
  Define (funDecl a b cs) $ map (DeclS . cDecl) (lvsIDefine x) ++ cBlock d

funDecl :: I.Type -> Lident -> [I.TVar] -> FunDecl
funDecl a b cs = case cType a of
  TyFun t ts -> RetFunFD t b vs ts
  t -> FunFD t b vs
  where vs = map cDecl cs
  
cFunDecl :: I.TVar -> FunDecl
cFunDecl tv@(I.TVar a b) = case b of
  I.Type "Fun_" xs -> funDecl (last xs) a (map (I.TVar "") $ init xs)
  _ -> error $ "unused:cFunDecl:not TyFun:" ++ ppShow tv

cExp :: I.Exp -> Exp
cExp x = case x of
  I.CallE f [b] | has_suffix "_fld" $ vtvar f ->
    AddrE $ ArrowE (cAtom b) $ VarE $ drop_suffix "fld" (vtvar f)
  I.CallE f [b] | vtvar f == "tagv" -> ArrowE (cAtom b) (enum "tag")
  I.CallE f [b,c] | vtvar f == "un" ->
    AddrE $ ArrowE (cAtom c) $ DotE (VarE "data") (cTag b)
  I.CallE f [b,c] | vtvar f == "idx" ->
    IdxE (cAtom b) (cAtom c)
  I.CallE a [b,c] | isBinOp a -> BinOpE (cAtom b) (cBinOp a) (cAtom c)
  I.CallE a bs -> CallE (cVarE a) $ map cAtom bs
  I.AtomE a -> cAtom a
  I.CastE a@(I.TVar _ t) b
    | isTypeEquiv t b -> cVarE a
    | otherwise -> CastE (cType b) (cVarE a)
  I.AllocaE a -> AllocaE $ cType a
  I.LoadE a -> LoadE $ cVarE a

cTag :: I.Atom -> Exp
cTag x = case x of
  I.LitA (I.TLit (I.EnumL a) _) -> enum a
  I.LitA (I.TLit (I.StringL a) _) -> enum a -- BAL: should be EnumL...
  _ -> error $ "cTag:" ++ ppShow x

enum :: Uident -> Exp
enum = LitE . EnumL

cVarE :: I.TVar -> Exp
cVarE (I.TVar a _) = VarE a

cStmt :: I.Stmt -> [Stmt]
cStmt x = case x of
  I.LetS a b -> case b of
    I.CallE f [c] | vtvar f == "mk" ->
      [ AssignS (DotE (cVarE a) (enum "tag")) $ cTag c ]
    I.CallE f [c,d] | vtvar f == "mk" ->
      [ AssignS (DotE (cVarE a) (enum "tag")) $ cTag c
      , AssignS (DotE (DotE (cVarE a) (enum "data")) (cTag c)) $
        cAtom d
      ]
    I.CallE f [c] | vtvar f == "mk" ->
      [AssignS (DotE (cVarE a) (enum "tag")) (cAtom c)]
    _ -> [AssignS (cVarE a) (cExp b)]
  I.StoreS a b -> [AssignS (LoadE $ cVarE a) (cAtom b)]
  I.CallS a bs -> [CallS (cVarE a) $ map cAtom bs]
  I.SwitchS a bs cs ->
    [SwitchS (cAtom a) $ map cSwitchAlt cs ++
     [DefaultAlt $ cBlock bs ++ [BreakS]]]
  I.IfS a b c -> [IfS (cAtom a) (cBlock b) (cBlock c)]
  I.WhenS a b -> [WhenS (cAtom a) (cBlock b)]
  I.WhileS a b c -> ss ++ [WhileS (cAtom b) (cBlock c ++ ss)]
    where ss = cBlock a
  I.ReturnS (I.LitA (I.TLit I.VoidL _)) -> [RetVoidS]
  I.ReturnS a -> [ReturnS (cAtom a)]
  I.NoOpS -> error "unused:cStmt:NoOpS"
  
cAtom :: I.Atom -> Exp
cAtom x = case x of
  I.VarA a -> cVarE a
  I.LitA (I.TLit I.VoidL _) -> error "void atom not removed"
  I.LitA a -> cTLitE a

cTLitE :: I.TLit -> Exp
cTLitE = LitE . cTLit

cBlock :: I.StmtList -> [Stmt]
cBlock = concatMap cStmt

cSwitchAlt :: I.SwitchAlt -> SwitchAlt
cSwitchAlt (I.SwitchAlt a b) =
  SwitchAlt (cTLit a) (cBlock b ++ [BreakS])

cTLit :: I.TLit -> Lit
cTLit (I.TLit x y) = case x of
  I.StringL a -> StringL a
  I.CharL a -> CharL a
  -- BAL add int and float suffixes
  I.NmbrL a -> NmbrL $ case y of
    I.Type "Float_" []
      | isFloat a -> a
      | otherwise -> show (readNumber a :: Double) ++ "f"
    I.Type "Double_" []
      | isFloat a -> a
      | otherwise -> show (readNumber a :: Double)
    _ | isFloat a -> error $ "integral type with float syntax:" ++ a ++
                     ":" ++ ppShow y
      | isBinary a -> show (readBinary a)
      | isOctal a -> '0' : drop 2 a
      | otherwise -> a
  I.EnumL a -> EnumL a
  I.VoidL -> error "unused:cLTit:VoidL"

cleanup :: Module -> Module
cleanup x =
  rewriteBi tTypeD $
  transformBi tVarName $
  transformBi tName $
  x

tMath :: Stmt -> Maybe Stmt
tMath (AssignS a (BinOpE b "+" (LitE (NmbrL s)))) -- BAL: Don't do if float/double?
  | a == b && (readNumber s :: Integer) == 1 = Just $ IncS a
tMath (AssignS a (BinOpE b "-" (LitE (NmbrL s)))) -- BAL: Don't do if float/double?
  | a == b && (readNumber s :: Integer) == 1 = Just $ DecS a
tMath _ = Nothing

tVarName :: Define -> Define
tVarName x = transformBi k $ transformBi h x
  where
    tbl = concatMap g $ groupBy (\a b -> f a == f b) $
          sort [ v | Decl _ v <- universeBi x ]
    f s =
      case reverse $ dropWhile (\c -> isDigit c || c == '_') $ reverse s of
        "" -> "_"
        s1 -> s1
    g ss = case ss of
      [s] -> [(s, f s)]
      _ -> [ (s, f s ++ show i) | (s,i) <- zip ss [0 :: Int .. ]]
    h (VarE v) = case lookup v tbl of
      Nothing -> VarE v
      Just v1 -> VarE v1
    h a = a
    k (Decl t v) = case lookup v tbl of
      Nothing -> error "unused:tVarName"
      Just v1 -> Decl t v1
    k a = a
    
optimize :: Module -> Module
optimize x =
  rewriteBi canon $
  rewriteBi tNoOpS $
  rewriteBi canonSS $
  rewriteBi opt $
  x

opt :: Stmt -> Maybe Stmt
opt (IfS e a _) | isTrue e = Just $ BlockS a
opt (IfS e _ b) | isFalse e = Just $ BlockS b
opt (WhenS e a) | isTrue e = Just $ BlockS a
opt (WhenS e _) | isFalse e = Just NoOpS
opt (WhileS e _) | isFalse e = Just NoOpS
opt _ = Nothing

canon :: Stmt -> Maybe Stmt
canon (IfS e a []) = Just $ WhenS e a
canon (IfS e [] b) = Just $ WhenS (NotE e) b
canon _ = Nothing

canonSS :: [Stmt] -> Maybe [Stmt]
canonSS xs | any isBlockS xs = Just $ concatMap f xs
  where f (BlockS ss) = ss
        f s = [s]
canonSS _ = Nothing

isBlockS :: Stmt -> Bool
isBlockS BlockS{} = True
isBlockS _ = False

isTrue :: Exp -> Bool
isTrue (LitE (EnumL "True_")) = True
isTrue _ = False

isFalse :: Exp -> Bool
isFalse (LitE (EnumL "False_")) = True
isFalse _ = False

dModule :: Bool -> Module -> Module
dModule is_readable x = 
  rewriteBi canon $
  rewriteBi tNoOpS $
  rewriteBi canonSS $
  rewriteBi tMath $
  transformBi tSort $
  transformBi reParen $
  transformBi tArray $
  transformBi tArrayArgTy $
  transformBi tUnused $
  (if is_readable then rewriteBi tLive else id) $
  transformBi tOnlyAssigned $
  transformBi tBlock $
  rewriteBi tNoOpS $
  rewriteBi tNoOpE $
  transformBi tPtr $
  rewriteBi tNoOpS $
  rewriteBi tNoOpE $
  transformBi tRHS $
  rewriteBi tNoOpS $
  rewriteBi tNoOpE $
  transformBi tLHS $
  transformBi tAlloca $
  transformBi tLit $
  x

tTypeD :: Decl -> Maybe Decl
tTypeD (Decl (TyArray a b) c) = Just $ Decl a $ c ++ "[" ++ b ++ "]"
tTypeD (Decl (TyPtr a) b) = Just $ Decl a $ "(*" ++ b ++ ")"
tTypeD (Decl (TyFun t ts) x) = Just $ FunD t ("(*" ++ x ++ ")") ts
tTypeD _ = Nothing

tArray :: Define -> Define
tArray x = transformBi i $ transformBi h $ transformBi g x
  where
  vs = [ v | Decl (TyArray{}) v <- universeBi x ]

  f (AddrE (VarE v)) | v `elem` vs = VarE v -- C passes arrays by reference
  f a = a

  g (CallE a bs) = CallE a $ map f bs
  g a = a

  h (CallS a bs) = CallS a $ map f bs
  h a = a

  i (AssignS a@(VarE v) b) | v `elem` vs =
    CallS (VarE "memcpy") [a, b, CallE (VarE "sizeof") [a]]
  i a = a

tSort :: Define -> Define
tSort (Define x ys) = Define x $ sortBy f cs ++ ds
  where
  (cs,ds) = partition isDeclS ys
  f (DeclS a) (DeclS b) = compare (declNm a) (declNm b)
  f _ _ = error "unused:tSort"
  
isDeclS :: Stmt -> Bool
isDeclS DeclS{} = True
isDeclS _ = False

tName :: Module -> Module
tName x = transformBi j $ transformBi i $ transformBi h $ transformBi g $
          transformBi k $ transformBi f x
  where
    f (Decl a b) = Decl a $ rName b
    f _ = error "unused:tName"
    g (VarE a) = VarE $ rName a
    g a = a
    h (FunFD a b cs) = FunFD a (rName b) cs
    h (RetFunFD a b cs ds) = RetFunFD a (rName b) cs ds
    i (EnumC a) = EnumC $ rName a
    j (EnumL a) = EnumL $ rName a
    j a = a
    k (TyName a) = TyName $ rName a
    k a = a

rName :: String -> String
rName x = strip_underscore $ map f $ filter ((/=) '~') x
  where
  f '.' = '_'
  f c = c

tBlock :: Define -> Define
tBlock x = transformBi f x
  where
  tbl :: [(String,Stmt)]
  tbl = catMaybes $ map h $ universeBi x
  f :: Stmt -> Stmt
  f s0@(AssignS (VarE v) e) = case lookup v tbl of
    Nothing -> s0
    Just s -> transformBi g s
      where
        g (VarE v1) | v1 == v = e
        g a = a
  f s | s `elem` map snd tbl = NoOpS
  f s = s

  h :: Stmt -> Maybe (String,Stmt)
  h s = case [ v | VarE v <- universeBi s, isFreshVar v ] \\ vs of
    [v] -> Just (v,s)
    _ -> Nothing
    where
    vs = case s of
      AssignS e _ -> [basename e]
      ReturnS e -> [basename e]
      _ -> []

-- make sure arithmetic expressions are fully parenthesized
reParen :: Exp -> Exp
reParen (ArrowE (AddrE a) b) = DotE a b
reParen (DotE (LoadE a) b) = ArrowE a b
reParen (CallE a@LoadE{} bs) = CallE (ParenE a) bs
reParen x = x

tUnused :: Define -> Define
tUnused (Define a bs) = Define a (filter f bs)
  where
  f (DeclS x) = declNm x `elem` vs
  f _ = True
  vs = nub [ v | VarE v <- universeBi bs ]

tOnlyAssigned :: Define -> Define
tOnlyAssigned x = rewriteBi f x
  where
    vs = concat [ [v,v] | AssignS (VarE v) (CallE{}) <- universeBi x ] \\
         [ v | VarE v <- universeBi x ]
    f (AssignS (VarE v) (CallE a bs)) | v `elem` vs = Just $ CallS a bs
    f _ = Nothing
      
tNoOpE :: Exp -> Maybe Exp
tNoOpE (LoadE (AddrE e)) = Just e
tNoOpE (ArrowE (AddrE (ArrowE a b)) c) = Just $ DotE (ArrowE a b) c
tNoOpE (IdxE (AddrE a) b) = Just $ AddrE (IdxE a b)
tNoOpE _ = Nothing

tNoOpS :: [Stmt] -> Maybe [Stmt]
tNoOpS xs
  | any isNoOpS xs = Just $ filter (not . isNoOpS) xs
  | otherwise = Nothing

isNoOpS :: Stmt -> Bool
isNoOpS NoOpS = True
isNoOpS (AssignS a b) = a == b
isNoOpS (CallS (VarE "memcpy") [a, b, _]) = a == b
isNoOpS _ = False

declNm :: Decl -> String
declNm (Decl _ v) = v
declNm _ = error "unused:declNm"

tLit :: Define -> Define
tLit x = rewriteBi g $ rewriteBi f x
  where
  tbl = [ (v,e) | AssignS (VarE v) e@LitE{} <- universeBi x ]

  f (AssignS (VarE v) _) | isJust (lookup v tbl) = Just NoOpS
  f _ = Nothing

  g (VarE v) = lookup v tbl
  g _ = Nothing

tAlloca :: Define -> Define
tAlloca x@(Define fd _) =
  transformBi g $ transformBi f $ rewriteBi h x
  where
  vs = [ v | AssignS (VarE v) AllocaE{} <- universeBi x ] ++
       [ v | Decl (TyPtr TyArray{}) v <- universeBi fd ]

  f :: Exp -> Exp
  f (ArrowE (AddrE (VarE v)) e) | v `elem` vs = DotE (VarE v) e
  f (VarE v) | v `elem` vs = AddrE (VarE v)
  f e = e

  g (Decl (TyPtr t) v) | v `elem` vs = Decl t v
  g a = a

  h :: Stmt -> Maybe Stmt
  h s | isAllocaS s = Just NoOpS
      | otherwise = Nothing

tArrayArgTy :: FunDecl -> FunDecl
tArrayArgTy = transformBi f
  where
  f :: Type -> Type
  f (TyPtr t@TyArray{}) = t
  f x = x

isAllocaS :: Stmt -> Bool
isAllocaS (AssignS VarE{} AllocaE{}) = True
isAllocaS _ = False

tPtr :: Define -> Define
tPtr x = transformBi h $ transformBi g $ rewriteBi f x
  where
  tbl = concat [ let e = VarE ('$':a) in [(a, AddrE e) ,(b, e)]
                 | AssignS (VarE a) (AddrE (VarE b)) <- universeBi x ]

  f (VarE v) = lookup v tbl
  f _ = Nothing

  g (VarE ('$':v)) = VarE v
  g e = e

  h (Decl (TyPtr t) v)  | v `elem` map fst tbl = Decl t v
  h p = p

tRHS :: Define -> Define
tRHS x = rewriteBi f x
  where
  tbl = [ (v,e) | AssignS e (VarE v) <- universeBi x, isFreshVar v ]

  f (VarE v) = lookup v tbl
  f _ = Nothing

tLHS :: Define -> Define
tLHS x = rewriteBi f x
  where
  tbl = [ (v,e) | AssignS (VarE v) e <- universeBi x, isFreshVar v ]

  f (VarE v) = lookup v tbl
  f _ = Nothing

isFreshVar :: String -> Bool
isFreshVar v = '_' `notElem` v && isDigit (last v)

-- Liveness
tLive :: Define -> Maybe Define -- this is hacky and inefficient, but seems to work
tLive x = case live_tbl x of
  [] -> Nothing
  (y:_) -> Just $ transformBi (f y) x
  where
    f (a,b) (VarE v) | v == a = VarE b
    f _ a = a  
  
is_reuse :: (String,String) -> Bool
is_reuse (a,b) = a /= b

live_tbl :: Define -> [(Nm, Nm)]
live_tbl (Define _ ss) =
  filter is_reuse $ reuse $ liveSS [ d | DeclS d <- ss ] initSt $ sStmts ss
    
sStmts :: [Stmt] -> [S]
sStmts = concatMap sStmt . reverse

basename :: Exp -> String
basename x = case x of
  VarE a -> a
  DotE a _ -> basename a
  AddrE a -> basename a
  ArrowE a _ -> basename a
  IdxE a _ -> basename a
  CastE _ b -> basename b
  LoadE a -> basename a
  ParenE a -> basename a
  _ -> error $ "unused:basename:" ++ ppShow x
  
sStmt :: Stmt -> [S]
sStmt x = case x of
  AssignS a b -> sExp b ++ [Init $ basename a]
  
  SwitchS a bs -> [Branch $
                   [ sStmts cs | DefaultAlt cs <- bs ] ++
                   [ sStmts cs | SwitchAlt _ cs <- bs ]
                  ] ++ sExp a
  IfS a bs cs -> [Branch [sStmts bs, sStmts cs]] ++ sExp a
  WhenS a bs -> [Branch [sStmts bs]] ++ sExp a
  WhileS a bs -> [Loop $ sStmts bs ++ sExp a]
  
  CallS a bs -> sExp a ++ concatMap sExp bs
  ReturnS a -> sExp a
  
  DeclS{} -> []
  BreakS -> []
  RetVoidS -> []
  NoOpS -> []
  
  BlockS ss -> sStmts ss
  DecS{} -> error $ "unused:sStmt:DecS"
  IncS{} -> error $ "unused:sStmt:IncS"
  
sExp :: Exp -> [S]
sExp x = [ Use v | VarE v <- universeBi x ]

type Nm = String

data S
  = Use Nm
  | Init Nm
  | Branch [[S]]
  | Loop [S]
  deriving Show

data St = St
  { in_use :: [Nm]
  , reuse :: [(Nm, Nm)]
  } deriving Show

initSt :: St
initSt = St [] []

liveSS :: [Decl] -> St -> [S] -> St
liveSS vs = loop
  where
  loop st [] = st
  loop st (x:xs) = case x of
    Use a -> loop st1 xs
      where st1 = st{ in_use = nub $ union [a] $ in_use st }
    Init a | isJust $ lookup a $ reuse st -> loop st xs
    Init a -> loop st1 xs
      where
        st1 = St{ in_use = in_use st \\ [a]
                , reuse = nub ((a, a1) : reuse st)
                }
        a1 = reuse_nm a vs $ in_use st
    Branch bs -> loop st1 xs
      where
        sts = map (loop st) bs
        st1 = St{ in_use = nub $ foldr1 union $ map in_use sts
                , reuse = nub (concatMap reuse sts)
                }
    Loop bs -> loop st $ concatMap no_inits bs ++ bs ++ xs
 
reuse_nm :: Nm -> [Decl] -> [Nm] -> Nm
reuse_nm a bs cs =
  head $ sort $ a : ((map declNm $ filter (equiv_decl b0) bs) \\ cs)
  where
    b0 = head $ filter ((==) a . declNm) bs

equiv_decl :: Decl -> Decl -> Bool
equiv_decl (Decl a _) (Decl b _) = a == b
equiv_decl _ _ = error "unused:equiv_decl"

no_inits :: S -> [S]
no_inits x = case x of
  Init{} -> []
  Use{} -> [x]
  Branch bs -> concatMap (concatMap no_inits) bs
  Loop bs -> concatMap no_inits bs
