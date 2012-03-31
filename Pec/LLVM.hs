{-# OPTIONS -Wall #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.LLVM (dModule) where

import Control.Concurrent
import Data.Char
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Development.Shake.FilePath
import Grm.Prims
import Language.LLVM.Abs
import Numeric
import Pec.IUtil
import qualified Language.Pir.Abs as I

data St = St
  { strings :: [(String,String)]
  , free_vars :: [String]
  , enums :: [(String,Integer)]
  , fields :: [(String,Integer)]
  , tydecls :: [(String,I.TyDecl)]
  , defines :: [Define]
  }

dModule :: FilePath -> I.Module -> IO ()
dModule outdir m@(I.Module a _ _) = do
  xs <- readMVar gTyDecls
  let st0 = St{ strings = ss
              , enums = concatMap tyEnums $ universeBi xs
              , free_vars = map vtvar ifvs
              , fields = concatMap tyFields $ universeBi xs
              , tydecls = [ (dTypeVar y, z) | (y, z) <- xs ]
              , defines = []
              }
  let ds = map (dTypeD st0) xs
  let st = st0{ defines = ds }
  writeFileBinary (joinPath [outdir, fn]) $
    ppShow $
      transformBi elimNoOpS $
      transformBi allocasAtStart $
      Module $
        map dStringD ss ++
        ds ++
        map dBuiltin builtinTbl ++
        map (dDeclare st) ifvs ++
        map (dDefine st) cs
  where
    I.Module _ _ cs = transformBi inlineAtoms m
    ifvs = nub $ concatMap fvsIDefine cs
    fn = n ++ ".ll"
    n = case a of
      "" -> error "unused:dModule"
      _ -> init a
    ss = [ (s, "@.str" ++ show i)
           | (I.StringL s ,i) <- zip (nub $ sort $ universeBi m)
                                 [ 0 :: Int .. ]]

dDeclare :: St -> I.TVar -> Define
dDeclare st x = case ty of
  PtrT (FunT a bs) -> Declare a v bs
  _ -> error $ "declare not a function type:" ++ ppShow x
  where
    TVar ty v = dTVar st x

dTypeD :: St -> (I.Type, I.TyDecl) -> Define
dTypeD st (x,y) = TypeD (dTypeVar x) $ dTyDecl st y

dTypeVar :: I.Type -> String
dTypeVar x0 = '%' : loop x0
  where
    loop x = case x of
      I.Type a [] -> a
      I.Type a xs ->
        a ++ "$" ++ concat (intersperse "." $ map loop xs) ++ "$"

dTyDecl :: St -> I.TyDecl -> Type
dTyDecl st x = case x of
  I.TyEnum bs -> lengthT bs
  I.TyRecord bs -> StructT $ map dFieldT bs
  I.TyTagged bs -> StructT
    [ lengthT bs
    , maximumBy (\a b -> compare (sizeT st a) (sizeT st b))
      [ dType t | I.ConC _ t <- bs ]
    ]

lengthT :: [a] -> Type
lengthT = IntT . show . bitsToEncode . genericLength

sizeT :: St -> Type -> Integer
sizeT st x = case x of
  VoidT -> 0
  CharT -> 8
  FloatT -> 32
  DoubleT -> 64
  PtrT{} -> sizeofptr
  FunT{} -> sizeofptr
  IntT a -> read a
  StructT bs -> sum $ map (sizeT st) bs
  ArrayT a b -> read a * (sizeT st b)
  UserT a -> case lookup a $ tydecls st of
    Just b -> sizeT st $ dTyDecl st b
    Nothing -> error $ "unused:sizeT:UserT:" ++ ppShow x
  VarArgsT -> error $ "unused:sizeT:VarArgsT:" ++ ppShow x

sizeofptr :: Integer
sizeofptr = 32

dFieldT :: I.FieldT -> Type
dFieldT (I.FieldT _ b) = dType b

dVar :: Bool -> String -> String
dVar is_free v = (if is_free then '@' else '%') : map f v
  where
    f c = case c of
      '~' -> '$'
      _ -> c
    
dDefine :: St -> I.Define -> Define
dDefine st (I.Define a b cs ds) =
  Define (dType a) (dVar True b) (map (dTVar st) cs)
    (concatMap (dStmt st) ds)

dStmt :: St -> I.Stmt -> [Stmt]
dStmt st x = case x of
  I.LetS a b -> dExp st a b
  I.StoreS a b -> [ StoreS (dAtom st b) (dTVar st a) ]
  I.CallS a b -> [ CallS (dTVar st a) (map (dAtom st) b) ]
  I.SwitchS a b cs -> concat
    [ [ SwitchS (dAtom st a) l1 $ map (dSwitchAlt st) lcs ]
    , [ LabelS l1 ]
    , concatMap (dStmt st) b
    , [ Br0S l0 ]
    , concatMap (dSwitchAltBody st l0) lcs
    , [ LabelS l0 ]
    ] 
    where
      l0 = uLbl a
      l1 = uLbl b
      lcs = [ (uLbl c, c) | c <- cs ]
  I.IfS a b c -> concat
    [ [ BrS (duAtom st a) l1 l2 ]
    , [ LabelS l1 ]
    , concatMap (dStmt st) b
    , [ Br0S l3 ]
    , [ LabelS l2 ]
    , concatMap (dStmt st) c
    , [ Br0S l3 ]
    , [ LabelS l3 ]
    ]
    where
      l1 = uLbl a
      l2 = uLbl b
      l3 = uLbl c
  I.WhenS a b -> concat
    [ [ BrS (duAtom st a) l1 l2 ]
    , [ LabelS l1 ]
    , concatMap (dStmt st) b
    , [ Br0S l2 ]
    , [ LabelS l2 ]
    ]
    where
      l1 = uLbl a
      l2 = uLbl b
  I.WhileS a b c -> concat
    [ [ Br0S l0 ]
    , [ LabelS l0 ]
    , concatMap (dStmt st) a
    , [ BrS (duAtom st b) l1 l2 ]
    , [ LabelS l1 ]
    , concatMap (dStmt st) c
    , [ Br0S l0 ]
    , [ LabelS l2 ]
    ]
    where
      l0 = uLbl a
      l1 = uLbl b
      l2 = uLbl c
  I.ReturnS a -> [ ReturnS $ dAtom st a ]
  I.NoOpS -> []

uLbl :: a -> String
uLbl a = uId a "Lbl"

dSwitchAlt :: St -> (String, I.SwitchAlt) -> SwitchAlt
dSwitchAlt st (lbl, I.SwitchAlt a _) = SwitchAlt tl lbl
  where
    tl = case dTLit st a of
      TLit (PtrT (FunT b _)) c -> TLit b c -- BAL: Shouldn't base report the correct type here without the need for this fixup?
      b -> b

dSwitchAltBody :: St -> String -> (String, I.SwitchAlt) -> [Stmt]
dSwitchAltBody st lbl0 (lbl, I.SwitchAlt _ b) = concat
  [ [ LabelS lbl ]
  , concatMap (dStmt st) b
  , [ Br0S lbl0 ]
  ]

variantTypes :: St -> Exp -> (Type,Type)
variantTypes st x = case [ (a, b) | TypeD v (StructT [a,b]) <- defines st, v == v0 ] of
  [y] -> y
  _ -> error $ "unused:variantTypes:" ++ ppShow x
  where
  IdxE (TVar (PtrT (UserT v0)) _) _ = x

fldE :: TVar -> Integer -> Exp
fldE a i = IdxE a $ LitA $ TLit (IntT "32") $ NmbrL $ show i

bitcastE :: TVar -> Type -> Exp
bitcastE = CastE Bitcast

dExp :: St -> I.TVar -> I.Exp -> [Stmt]
dExp st tv@(I.TVar v t) x = case x of
  I.CallE (I.TVar "tagv" _) [I.VarA b] ->
    [ LetS v1 $ fldE (dTVar st b) 0, letS $ LoadE $ TVar (PtrT $ dType t) v1 ]
    where
      v1 = uId b "%.tag"
  I.CallE (I.TVar "un" _) [_, I.VarA c] ->
    [ LetS v1 e, letS $ bitcastE (TVar (PtrT ta) v1) tb ]
    where
      v1 = uId c "%.data"
      e = fldE (dTVar st c) 1
      (_,ta) = variantTypes st e
      tb = dType t
  I.CallE (I.TVar "mk" a) [I.LitA (I.TLit (I.StringL b) _)] -> fst $ dTag st tv a b
  I.CallE (I.TVar "mk" a) [I.LitA (I.TLit (I.StringL b) _), c] ->
    ss0 ++
    [ LetS datap0 $ fldE tv1 1
    , LetS datap1 $ bitcastE (TVar (PtrT tb) datap0) (PtrT tc)
    , StoreS atomc (TVar (PtrT tc) datap1)
    , s
    ]
    where
      (ss,(tv1,tb)) = dTag st tv a b
      (ss0, s) = (init ss, last ss)
      datap0 = uId c "%.data"
      datap1 = uId datap0 "%.data"
      atomc = dAtom st c
      tc = tyAtom atomc
  I.CallE a [I.VarA b] | isJust mi -> [ letS $ fldE (dTVar st b) $ fromJust mi ]
    where mi = lookup (vtvar a) $ fields st
  I.CallE (I.TVar "idx" _) [I.VarA b, c] -> [ letS $ IdxE (dTVar st b) $ dAtom st c ]
  I.CallE a [b,c] | isBinOp a -> [ letS $ llvmBinOp st a b c ]
  I.CallE a b -> [ letS $ CallE (dTVar st a) (map (dAtom st) b) ]
  I.CastE a b -> [ letS $ CastE cast tva tb ]
    where
    tva@(TVar ta _) = dTVar st a
    tb = dType b
    y = ttvar a
    sa = sizeT st ta
    sb = sizeT st tb
    cast
      | isSigned y && isFloating b = Sitofp
      | isUnsigned y && isFloating b = Uitofp
      | isFloating y && isSigned b = Fptosi
      | isFloating y && isUnsigned b = Fptoui
      | isFloating y && isFloating b && sa < sb = Fpext
      | isFloating y && isFloating b && sa > sb = Fptrunc
      | isSigned y && isSigned b && sa < sb = Sext
      | isSigned y && isSigned b && sa > sb = Trunc
      | isUnsigned y && isUnsigned b && sa < sb = Zext
      | isUnsigned y && isUnsigned b && sa > sb = Trunc
      | otherwise = Bitcast
  I.AllocaE a -> [ letS $ AllocaE $ dType a ]
  I.LoadE a -> [ letS $ LoadE $ dTVar st a ]
  I.AtomE a -> [ letS $ AtomE $ dAtom st a ]
  where
    letS = LetS (dVar False v)

dTag :: St -> I.TVar -> a -> String -> ([Stmt], (TVar, Type))
dTag st tv a b =
    ([ LetS v1 $ AllocaE t
     , LetS tagp tagfld
     , StoreS (LitA $ TLit ta $ dEnum st b) (TVar (PtrT ta) tagp)
     , LetS v0 $ LoadE tv1
     ], (tv1,tb))
    where
      TVar t v0 = dTVar st tv
      v1 = uId a "%.v"
      tv1 = TVar (PtrT t) v1
      tagp = uId b "%.tag"
      tagfld = fldE tv1 0
      (ta,tb) = variantTypes st tagfld

duAtom :: St -> I.Atom -> UAtom
duAtom st = uAtom . dAtom st

llvmBinOp :: St -> I.TVar -> I.Atom -> I.Atom -> Exp
llvmBinOp st a b c =
  BinOpE (f ty) (dType ty) (duAtom st b) (duAtom st c)
  where
    f = fromJust $ lookup (vtvar a) binOpTbl
    ty = tatom b

tyAtom :: Atom -> Type
tyAtom x = case x of
  LitA (TLit a _) -> a
  VarA (TVar a _) -> a

uAtom :: Atom -> UAtom
uAtom x = case x of
  LitA (TLit _ b) -> LitUA b
  VarA (TVar _ b) -> VarUA b
  
dAtom :: St -> I.Atom -> Atom
dAtom st x = case x of
  I.LitA a -> LitA $ dTLit st a
  I.VarA a -> VarA $ dTVar st a

dLit :: St -> I.Type -> I.Lit -> Lit
dLit st t x = case x of
  I.StringL a -> case lookup a $ strings st of
    Just v -> StringL (show $ length a + 1) v
    Nothing -> error $ "unused:dLit:string"
  I.NmbrL a
    | isFloating t -> NmbrL $ show (readNumber a :: Double)
    | isFloat a -> error $ "non-integral literal:" ++ a
    | otherwise -> NmbrL $ show (readNumber a :: Integer)
  I.CharL a -> NmbrL $ show $ ord a
  I.EnumL a -> dEnum st a
  I.VoidL -> VoidL

dEnum :: St -> String -> Lit
dEnum st x = case x of
  "False_" -> FalseL
  "True_" -> TrueL
  _ -> case lookup x $ enums st of
    Nothing -> error $ "unused:dEnum:" ++ ppShow (enums st, x)
    Just i -> NmbrL $ show i

dTVar :: St -> I.TVar -> TVar
dTVar st (I.TVar a b) = case lookup a builtinTbl of
  Just t -> TVar t (dVar True a)
  Nothing -> TVar (dType b) (dVar (a `elem` (free_vars st ++ builtins)) a)

dBuiltin :: (String, Type) -> Define
dBuiltin (s, PtrT (FunT a bs)) = Declare a ('@':s) bs
dBuiltin x = error $ "unused:dBuiltin:" ++ ppShow x

builtinTbl :: [(String, Type)]
builtinTbl =
  [ ("printf", PtrT (FunT VoidT [PtrT CharT, VarArgsT])) ]
  
dTLit :: St -> I.TLit -> TLit
dTLit st (I.TLit a b) = TLit (dType b) (dLit st b a)

dType :: I.Type -> Type
dType t@(I.Type a b) = case (a,b) of
  ("Ptr_", [c]) -> PtrT (dType c)
  ("Void_", []) -> VoidT
  ("I_",_) -> IntT $ nCnt b
  ("W_",_) -> IntT $ nCnt b
  ("Fun_", ts) -> PtrT (FunT (dType $ last ts) (map dType $ init ts))
  ("Array_", [c,d]) -> ArrayT (nCnt [c]) (dType d)
  ("Float_", []) -> FloatT
  ("Double_", []) -> DoubleT
  ("Char_", []) -> CharT
  _ -> UserT $ dTypeVar t

fSOrU :: a -> a -> a -> I.Type -> a
fSOrU a b c t
  | isFloating t = a
  | isSigned t = b
  | otherwise = c

fOrN :: a -> a -> I.Type -> a
fOrN a b t
  | isFloating t = a
  | otherwise = b

binOpTbl :: [(String, I.Type -> BinOp)]
binOpTbl =
  [ ("eq", \_ -> Icmp Equ)
  , ("ne", \_ -> Icmp Neq)
  , ("gt", fSOrU (Fcmp Ogt) (Icmp Sgt) (Icmp Ugt))
  , ("gte", fSOrU (Fcmp Oge) (Icmp Sge) (Icmp Uge))
  , ("lt", fSOrU (Fcmp Olt) (Icmp Slt) (Icmp Ult))
  , ("lte", fSOrU (Fcmp Ole) (Icmp Sle) (Icmp Ule))
  , ("add", fOrN Fadd Add)
  , ("sub", fOrN Fsub Sub)
  , ("mul", fOrN Fmul Mul)
  , ("div", fSOrU Fdiv Sdiv Udiv)
  , ("rem", fSOrU Frem Srem Urem)
  , ("shl", \_ -> Shl)
  , ("shr", \_ -> Lshr)
  , ("band", \_ -> And)
  , ("bor", \_ -> Or)
  , ("bxor", \_ -> Xor)
  , ("bnot", \_ -> error $ "todo:implement binary not in LLVM") -- BAL
  , ("and", \_ -> error $ "todo:implement boolean and in LLVM") -- BAL:doesn't this get desugared?
  , ("or", \_ -> error $ "todo:implement boolean or in LLVM") -- BAL:doesn't this get desugared?
  ]

dStringD :: (String, Lident) -> Define
dStringD (s,v) = StringD v (show $ 1 + length s) $ concatMap const_char s

const_char :: Char -> String
const_char c
  | c < ' ' || c > '~' || c == '\\' = encode_char c
  | otherwise = [c]

encode_char :: Enum a => a -> String
encode_char c =
  '\\' : (if i <= 0xf then "0" else "") ++ map toUpper (showHex i "")
  where i = fromEnum c

allocasAtStart :: Define -> Define -- also removes unused allocas
allocasAtStart (Define a b cs ds) = Define a b cs $
  [ s | s@(LetS v AllocaE{}) <- universeBi ds, v `elem` universeBi ds1 ]
  ++ ds1
  where
    ds1 = transformBi f ds
    f :: Stmt -> Stmt
    f s
      | isAllocaS s = NoOpS
      | otherwise = s
allocasAtStart x = x

isAllocaS :: Stmt -> Bool
isAllocaS (LetS _ AllocaE{}) = True
isAllocaS _ = False

elimNoOpS :: Module -> Module
elimNoOpS = transformBi (filter ((/=) NoOpS))
