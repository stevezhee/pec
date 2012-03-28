{-# OPTIONS -Wall #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.Desugar (desugar) where

import Data.Char
import Data.Either
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Grm.Prims
import Language.Pec.Abs
import Text.PrettyPrint.Leijen
import qualified Language.Pds.Abs as D
import Pec.PUtil

desugar :: Module Point -> D.Module
desugar = rewriteBi dTyCxt . dModule . tModule

tModule :: Module Point -> Module Point
tModule m =
  rewriteBi dBlockE $ -- BAL: make it so we don't have to call these twice
  rewriteBi dConstructedE $
  rewriteBi dTupleE $
  rewriteBi dPatE $
  rewriteBi dBlockE $
  rewriteBi dConstructedE $
  rewriteBi unProcD $
  transformBi (sortBy cmpFieldD) $ 
  transformBi (sortBy cmpFieldT) $ 
  transformBi (sortBy cmpConC) $ 
  transformBi (\(Con p s) -> Con (p :: Point) $ usern s) $
  transformBi (\(Field p s) -> Field (p :: Point) $ usern s) $
  transformBi (\(Var p s) -> Var (p :: Point) $ usern s) m

unCxt :: D.Type -> D.Type
unCxt x = case x of
  D.TyCxt _ b -> b
  _ -> x

isTyCxt :: D.Type -> Bool
isTyCxt x = case x of
  D.TyCxt{} -> True
  _ -> False

cxtOf :: D.Type -> D.CxtList
cxtOf x = case x of
  D.TyCxt a _ -> a
  _ -> []

tyCxt :: [D.Cxt] -> D.Type -> D.Type
tyCxt xs y = D.TyCxt (sort $ nub xs) y

dTyCxt :: D.Type -> Maybe D.Type
dTyCxt x = case x of
  D.TyCxt a b | isTyCxt b -> Just $ tyCxt (a ++ cxtOf b) (unCxt b)
  D.TyFun a b | any isTyCxt [a,b] ->
    Just $ tyCxt (cxtOf a ++ cxtOf b) $ D.TyFun (unCxt a) (unCxt b)
  D.TyConstr a bs | any isTyCxt bs ->
    Just $ tyCxt (concatMap cxtOf bs) $ D.TyConstr a $ map unCxt bs
  _ -> Nothing

dModule :: Module Point -> D.Module
dModule (Module _ a b c ds) = D.Module (dModid a)
  (dExportDecls ys xs b) (dImportDecls c) ys zs
  (concatMap constrs ys ++ concatMap deconstrs ys ++ xs)
  where
  (ws,xs) = partitionEithers $ map dTopDecl $ unAscribeDs ds
  (ys,zs) = partitionEithers ws
    
usern :: String -> String
usern s = case a of
  "" -> b ++ "_"
  _ -> a ++ "." ++ b ++ "_"
  where
  (a,b) = breakQual s

breakQual :: String -> (String,String)
breakQual s = (vModid $ qual $ init ss, last ss)
  where ss = unqual s
  
vModid :: String -> String
vModid s
  | null s = s
  | '_' `elem` s =
    error $ "underscore not allowed in module identifier:" ++ s
  | otherwise = qual_to_und s ++ "_"

dModid :: Modid Point -> String
dModid = vModid . ppShow

cmpFieldD :: FieldD Point -> FieldD Point -> Ordering
cmpFieldD (FieldD _ a _) (FieldD _ b _) = compare a b

cmpFieldT :: FieldT Point -> FieldT Point -> Ordering
cmpFieldT (FieldT _ a _) (FieldT _ b _) = compare a b

cmpConC :: ConC Point -> ConC Point -> Ordering
cmpConC (ConC _ a _) (ConC _ b _) = compare a b

dExportDecls :: [D.TypeD] -> [D.VarD] -> ExportDecls Point -> [D.ExportD]
dExportDecls xs ys z = case z of
  ExpAllD p -> concatMap (dSpec (Both p)) xs ++ map exVarD ys
  ExpListD _ es -> concatMap (dExport xs) es

dExport :: [D.TypeD] -> Export Point -> [D.ExportD]
dExport xs y = case y of
  VarEx{} -> [D.VarEx $ ppShow y]
  TypeEx _ a b -> case [ d | d@(D.TypeD c _ _) <- xs, c == ppShow a ] of
    [] -> error $ "unknown type in export list:" ++ ppShow a
    (z:_) -> dSpec b z

exVarD :: D.VarD -> D.ExportD
exVarD (D.VarD a _ _) = D.VarEx a

tTypeD :: D.TypeD -> D.Type
tTypeD (D.TypeD a bs c) =
  D.TyCxt cxt $ D.TyConstr a $ map (D.TyVarT . ppShow) bs
  where cxt = nub $ universeBi c

tTag :: D.Type -> D.Type
tTag t = D.TyConstr "Tag" [t]

constrs :: D.TypeD -> [D.VarD]
constrs d@(D.TypeD _ _ x) = case x of
  D.TySyn{} -> []
  D.TyNewtype a b -> [ fun2 "mk" "nt" a $ f b t ]
  D.TyUnit b -> [ fun2 "mk" "uni" b t ]
  D.TyEnum bs -> [ fun2 "mk" "tg" b t | b <- bs ]
  D.TyRecord{} -> []
  D.TyTagged ys -> [ fun "mk" a $ f b t | D.ConC a b <- ys ]
  where
  t = tTypeD d
  f ta tb = if ta == D.TyVoid then tb else D.TyFun ta tb

deconstrs :: D.TypeD -> [D.VarD]
deconstrs d@(D.TypeD _ _ x) = case x of
  D.TySyn{} -> []
  D.TyNewtype a b ->
    [ fun "unwrap_" a (D.TyFun t b)
    , let p = uId a "p" in
       fun "unwrapptr_" a (D.TyFun (aptr p t) (aptr p b)) ]
  D.TyUnit b -> [ fun2 "tg" "uni" b t ]
  D.TyEnum bs -> [ fun "tg" b t | b <- bs ]
  D.TyRecord ys ->
    [ let p = uId a "p" in fun "fld" a (D.TyFun (aptr p t) (aptr p b))
    | D.FieldT a b <- ys ]
  D.TyTagged ys ->
    [ fun "tg" a $ D.TyFun (rptr_ t) (tTag t)  | D.ConC a _ <- ys ] ++
    [ let p = uId a "p" in fun "un" a $
        D.TyFun (rptr p t) (if b == D.TyVoid then b else rptr p b)
    | D.ConC a b <- ys
    ]
  where
  t = tTypeD d

rptr :: String -> D.Type -> D.Type
rptr p a =
  D.TyCxt [ D.Cxt "Load_" [D.Var p]] $ aptr p a

aptr :: String -> D.Type -> D.Type
aptr p a = D.TyConstr "Pointer_" [D.TyVarT p ,a]

rptr_ :: D.Type -> D.Type
rptr_ a = let p = uId a "p" in rptr p a

fun2 :: Pretty a => String -> String -> a -> D.Type -> D.VarD
fun2 a x b c = D.VarD (lowercase s ++ a) D.Macro $
  D.AscribeE (D.AppE (D.VarE x) (dExp $ stringE s)) c
  where s = ppShow b

fun :: Pretty a => String -> a -> D.Type -> D.VarD
fun a = fun2 a a

dSpec :: Spec Point -> D.TypeD -> [D.ExportD]
dSpec a b@(D.TypeD c _ _) = case a of
  Neither _ -> [D.TypeEx c]
  Decon p -> dSpec (Neither p) b ++ map exVarD (deconstrs b)
  Both p -> dSpec (Decon p) b ++ map exVarD (constrs b)

un :: Pretty a => a -> String
un x = lowercase (ppShow x) ++ "un"

fld :: Pretty a => a -> String
fld x = ppShow x ++ "fld"

dImportDecls :: ImportDecls Point -> [D.ImportD]
dImportDecls x = case x of
  ImpListD _ a -> map f a
  ImpNoneD _ -> []
  where
  f y = case y of
    Import _ a (AsAS _ b) -> D.ImportD (dModid a) (D.AsAS $ dModid b)
    Import _ a (EmptyAS _) -> D.ImportD (dModid a) D.EmptyAS
        
dVar :: Pretty a => a -> D.Var
dVar = D.Var . ppShow

dTyArity :: D.Type -> Int
dTyArity x = case x of
  D.TyCxt _ b -> dTyArity b
  D.TyFun _ b -> 1 + dTyArity b
  _ -> 0

dTopDecl :: TopDecl Point -> Either (Either D.TypeD D.InstD) D.VarD
dTopDecl (VarD _ a b c) = Right $ D.VarD (ppShow a) (dDeclSym b) (dExp c)
dTopDecl (ExternD p a b c) = Right $ D.VarD (ppShow b) D.Macro
  (dExp $ AscribeE p (AppE p (AppE p (varE "extern") (stringE v))
                    (stringE $ show $ dTyArity $ dType c)) c)
  where
  v = case a of
    SomeNm _ x -> x
    NoneNm _ -> init $ ppShow b
dTopDecl (TypeD0 p a bs) =
  dTopDecl (TypeD p a bs $ TyTagged p [ConC p (Con p c) []]) -- BAL: should also use module name to avoid clashes.
  where c = uId a (ppShow a)
dTopDecl (TypeD _ a bs c) =
  Left $ Left $ D.TypeD (ppShow a) (map dVar bs) $
  case c of
    TySyn _ x -> D.TySyn $ dType x
    TyRecord _ xs -> D.TyRecord [ D.FieldT (ppShow y) (dType z)
                                | FieldT _ y z <- xs ]
    TyTagged _ xs -> case xs of
      [] -> error "unused:dTopDecl:[]" -- BAL: support?
      [ConC _ y []] -> D.TyUnit (ppShow y)
      [ConC p y zs] -> D.TyNewtype (ppShow y) $ dType $ TyTuple p zs
      _ | all isEnumC xs ->
            D.TyEnum [ D.EnumC (ppShow y) | ConC _ y [] <- xs ]
      _ -> D.TyTagged [ D.ConC (ppShow y) (dType $ TyTuple p zs)
                      | ConC p y zs <- xs ]
dTopDecl (InstD _ a b) = Left $ Right $ D.InstD (ppShow a) (dType b)
dTopDecl _ = error "unused:dTopDecl"

unAscribeDs :: [TopDecl Point] -> [TopDecl Point]
unAscribeDs xs = foldr unAscribeD zs ys
  where
  (ys,zs) = partition isAscribeD xs

unProcD :: TopDecl Point -> Maybe (TopDecl Point)
unProcD (ProcD p a bs c d) = Just $ VarD p a c $ LamE p bs d
unProcD _ = Nothing

unAscribeD :: TopDecl Point -> [TopDecl Point] -> [TopDecl Point]
unAscribeD (AscribeD _ x y) xs = case ys of
  [] -> error $ "unknown ascription:" ++ ppShow x
  _ -> [ VarD p a b $ AscribeE p c y | VarD p a b c <- ys ] ++ zs
  where
  (ys,zs) = partition f xs
  f (VarD _ a _ _) = ppShow a == ppShow x
  f _ = False
unAscribeD _ _ = error "unused:unAscribeD"

isAscribeD :: TopDecl Point -> Bool
isAscribeD (AscribeD{}) = True
isAscribeD _ = False

isEnumC :: ConC Point -> Bool
isEnumC (ConC _ _ xs) = null xs

dTyVar :: TyVar Point -> D.Type
dTyVar x = case x of
  VarTV _ a -> D.TyVarT $ usern a
  CntTV _ a -> D.TyCxt [D.Cxt "Count" [D.Var s]] $ D.TyVarT s
    where s = usern a

dDeclSym :: DeclSym Point -> D.DeclSym
dDeclSym x = case x of
  Macro _ -> D.Macro
  Define _ -> D.Define

dType :: Type Point -> D.Type
dType x = case x of
  TyCxt _ a b -> D.TyCxt (map dCxt a) (dType b)
  TyFun _ a b -> D.TyFun (dType a) (dType b)
  TyConstr _ a b -> D.TyConstr (ppShow a) $ map dType b
  TyTuple p ts -> case ts of
    [] -> D.TyVoid
    [t] -> dType t
    [a,b] -> dType $ TyConstr p (Con p "Pair_") [a,b]
    (a:bs) -> dType $ TyTuple p [a, TyTuple p bs]
  TyVarT _ a -> dTyVar a
  TyConstr0 p a -> dType $ TyConstr p a []
  TyArray p a b -> dType $ TyConstr p (Con p "Array_") [a,b]
  TyCount p (Count _ a)
    | all isDigit a -> dType $ TyConstr0 p (Con p $ "Cnt" ++ ppShow a)
    | otherwise -> error "literal count types must be in decimal form"
                   
dCxt :: Cxt Point -> D.Cxt
dCxt (Cxt _ a bs) = D.Cxt (ppShow a) $ map dVar bs

varE :: Lident -> Exp Point
varE = VarE p . Var p
  where p = noPoint
    
stringE :: String -> Exp Point
stringE = LitE p . StringL p
  where p = noPoint
        
nmbrE :: Integer -> Exp Point
nmbrE = LitE p . NmbrL p . show
  where p = noPoint
        
apps :: [Exp Point] -> Exp Point
apps = foldl1 (AppE noPoint)

dSwitchAlt :: SwitchAlt Point -> D.SwitchAlt
dSwitchAlt (SwitchAlt _ a b) = D.SwitchAlt (dLit "tg" a) (dExp b)

dCaseAlt :: Exp Point -> CaseAlt Point -> D.SwitchAlt
dCaseAlt e (CaseAlt p a bs c) = D.SwitchAlt
  (D.AppE (dLit "tg" $ EnumL p a) (dExp e))
  (dExp $ rewriteBi dPatE $ LetE p (TupleE p $ map (VarE p) bs) (Macro p)
   (AppE p (varE $ un a) e) c)

tagv :: Exp Point -> D.Exp
tagv a = dExp $ AppE (point a) (varE "tagv") a

defE :: Exp Point -> Var Point -> Exp Point -> D.Exp
defE a b c = dExp $ AppE (point a) (LamE (point c) [VarE (point b) b] c) a

dBlockE :: Exp Point -> Maybe (Exp Point)
dBlockE x = case x of
  BlockE p0 ys -> case ys of
    [] -> error "unused:dBlockE"
    [y] -> Just y
    (e:es) -> case e of
      LetS p1 a b c -> Just $ LetE p1 a b c $ BlockE p0 es
      _ -> Just $ apps [varE "then_", e, BlockE p0 es]
  _ -> Nothing
  
dPatE :: Exp Point -> Maybe (Exp Point)
dPatE x = case x of
  LamE p bs c -> case bs of
    [] -> error "unused:dPatE"
    [b] -> case unPat b c of
      Nothing -> Nothing
      Just (b1,c1) -> Just $ LamE p [b1] c1
    (d:ds) -> Just $ LamE p [d] $ LamE p ds c
  LetE p a b c d -> case unPat a d of
    Nothing -> Nothing
    Just (a1,d1) -> Just $ LetE p a1 b c d1
  _ -> Nothing

unPat :: Exp Point -> Exp Point -> Maybe (Exp Point, Exp Point)
unPat x y = case x of
  TupleE p xs -> case xs of
    [] -> Just (AscribeE p (varE "_u") (TyTuple p []), y)
    [a] -> Just (a, y)
    [a,b] -> Just (v, LetE p a (Macro p) (apps [varE "fst_fld", v])
                      (LetE p b (Macro p) (apps [varE "snd_fld", v]) y))
      where
        v = varE $ uId x "p"
    (e:es) -> Just (TupleE p [e, TupleE p es], y)
  AscribeE _ a _ -> Just (a, apps [varE "ignore_", x, y])
  VarE{} -> Nothing
  _ -> error $ "unexpected pattern:" ++ ppShow x
  
dExp :: Exp Point -> D.Exp
dExp x = case x of
  BlockE{} -> error "unused:dExp:BlockE"
  LetS{} ->
    error "malformed let statement (must not be the last expr in a block)"
  LetE _ a b c d -> case a of
    VarE{} -> D.LetE (ppShow a) (dDeclSym b) (dExp c) (dExp d)
    _ -> error $ "unused:dExp:LetE:" ++ ppShow a
  LamE _ bs c -> case bs of
    [VarE _ (Var _ a)] -> D.LamE a (dExp c)
    _ -> error "unused:dExp:LamE"
  SwitchE _ a bs (DefaultNone{}) ->
    D.SwitchE (dExp a) D.DefaultNone $ map dSwitchAlt bs
  SwitchE _ a@(VarE{}) bs (DefaultAlt _ c d) ->
    D.SwitchE (dExp a) (D.DefaultSome $ defE a c d) (map dSwitchAlt bs)
  SwitchE p a bs c -> dExp $ LetE p v (Define p) a $ SwitchE p v bs c
    where v = varE $ uId x "s"
  CaseE _ a@(VarE{}) bs (DefaultNone{}) ->
    D.SwitchE (tagv a) D.DefaultNone (map (dCaseAlt a) bs)
  CaseE _ a@(VarE{}) bs (DefaultAlt _ c d) ->
    D.SwitchE (tagv a) (D.DefaultSome $ defE a c d) (map (dCaseAlt a) bs)
  CaseE p a bs c -> dExp $ LetE p v (Define p) a $ CaseE p v bs c
    where v = varE $ uId x "c"
  BranchE _ ys0 z -> case ys0 of
    [] -> dExp z
    (BranchAlt p a b : ys) ->
      dExp $ apps [varE "if_", a, b, BranchE p ys z]
  StoreE _ a b -> dExp $ apps [varE "store_", a, b]
  BinOpE _ a b c -> dExp $ apps [opE b, a, c]
  UnOpE p a b -> dExp $ AppE p (opE a) b
  IdxE _ a b -> dExp $ apps [varE "idx_", a, b]
  FldE p a b -> dExp $ AppE p (varE $ fld b) a
  CountE _ (Count _ a) -> dCnt $ ppShow a
  AppE _ a b -> D.AppE (dExp a) (dExp b)
  ArrayE{} -> error "unused:dExp:ArrayE"
  RecordE{} -> error "unused:dExp:RecordE"
  TupleE{} -> error "unused:dExp:TupleE"
  AscribeE _ a b -> D.AscribeE (dExp a) (dType b)
  VarE _ a -> D.VarE $ ppShow a
  LitE _ a -> dLit "mk" a

dTupleE :: Exp Point -> Maybe (Exp Point)
dTupleE (TupleE p xs) = case xs of
  [] -> Just $ VarE p (Var p "void_")
  [a] -> Just a
  [a,b] -> Just $ RecordE p [ fieldD "fst_" a, fieldD "snd_" b ]
  (a:bs) -> Just $ TupleE p [a, TupleE p bs]
dTupleE _ = Nothing

fieldD :: String -> Exp Point -> FieldD Point
fieldD a e = FieldD p (Field p a) e
  where p = point e
        
countE :: Int -> Exp Point
countE = CountE noPoint . Count noPoint . show

dConstructedE :: Exp Point -> Maybe (Exp Point)
dConstructedE (ArrayE p xs) = Just $ BlockE p $
  [ LetS p v (Define p) $ AppE p (varE "unsafe_alloca_array_")
    (countE $ length xs) ] ++
  [ StoreE p (IdxE p v $ nmbrE i) x | (i,x) <- zip [0 .. ] xs ] ++
  [ AppE p (varE "load_") v ]
  where
    v = varE $ uId xs "a"
dConstructedE (RecordE p0 xs) = Just $ BlockE p0 $
  [ LetS p0 v (Define p0) $ varE "unsafe_alloca_" ] ++
  [ StoreE p (FldE p v a) b | FieldD p a b <- xs ] ++
  [ AppE p0 (varE "load_") v ]
  where
    v = varE $ uId xs "r"
dConstructedE _ = Nothing

dCnt :: String -> D.Exp
dCnt x = dExp $ varE $ "cnt" ++ x

dLit :: String -> Lit Point -> D.Exp
dLit suf x = case x of
  CharL _ a -> D.LitE $ D.CharL a
  StringL _ a -> D.LitE $ D.StringL a
  NmbrL _ a -> D.LitE $ D.NmbrL a
  EnumL{} -> dExp $ varE $ lowercase $ ppShow x ++ suf

opE :: Pretty a => a -> Exp Point
opE a = varE $ concatMap f $ ppShow a
  where
  f x = fromMaybe err $ lookup x
    [ ('!', "bang_"), ('"', "dquote_"), ('#', "hash_"), ('$', "dollar_")
    , ('%', "rem_"), ('&', "band_"), ('\'', "squote_"), ('(', "lparen_")
    , (')', "rparen_"), ('*', "mul_"), ('+', "add_"), (',', "comma_")
    , ('-', "sub_"), ('.', "dot_"), ('/', "div_"), (':', "colon_")
    , (';', "semi_"), ('<', "lt_"), ('=', "eq_"), ('>', "gt_")
    , ('?', "ques_"), ('@', "load_"), ('[', "lbrack_"), ('\\', "bslash_")
    , (']', "rbrack_"), ('^', "bxor_"), ('`', "grave_"), ('{', "lbrace_")
    , ('|', "bor_"), ('}', "rbrace_"), ('~', "bnot_")
    ]
    where err = error $ "unknown character in operator:" ++ show x
