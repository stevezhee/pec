{-# OPTIONS -Wall #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.IUtil where

import Data.List
import Grm.Prims
-- import Text.PrettyPrint.Leijen
-- import qualified Language.Haskell as H
import Language.Pir.Abs
import Control.Concurrent
import Data.Char
import Data.Generics.Uniplate.Data
import System.IO.Unsafe
import Data.Maybe

gTyDecls :: MVar [(Type,TyDecl)]
gTyDecls = unsafePerformIO $ newMVar []

fvsIDefine :: Define -> [TVar]
fvsIDefine x@(Define _ _ _ ds) =
  (nub $ filter f $ universeBi ds) \\ bvsIDefine x
  where
    f v@(TVar a _) = not (a `elem` builtins || isBinOp v || isFldOp a)

isFldOp :: String -> Bool
isFldOp = has_suffix "_fld"

bvsIDefine :: Define -> [TVar]
bvsIDefine x@(Define _ _ cs _) = nub (cs ++ lvsIDefine x)

lvsIDefine :: Define -> [TVar]
lvsIDefine (Define _ _ _ ds) = nub [ v | LetS v _ <- universeBi ds ]

builtins :: [String]
builtins = ["mk", "tagv", "idx", "un", "printf"]

isTypeEquiv :: Type -> Type -> Bool
isTypeEquiv a b = case (a,b) of
  (Type "Char_" [], Type "W_" [Type "Cnt8" []]) -> True
  (Type "W_" [Type "Cnt8" []], Type "Char_" []) -> True
  _ -> False

tyVoid :: Type
tyVoid = Type "Void_" []
  
vtvar :: TVar -> String
vtvar (TVar a _) = a

ttvar :: TVar -> Type
ttvar (TVar _ b) = b

ttlit :: TLit -> Type
ttlit (TLit _ b) = b

tatom :: Atom -> Type
tatom x = case x of
  LitA a -> ttlit a
  VarA a -> ttvar a

isBinOp :: TVar -> Bool
isBinOp = isJust . lookupBinOp

cBinOp :: TVar -> String
cBinOp = fromJust . lookupBinOp

lookupBinOp :: TVar -> Maybe String
lookupBinOp v = lookup (vtvar v)
  [ ("add","+")
  , ("sub","-")
  , ("mul","*")
  , ("div","/")
  , ("rem","%")
  , ("gt",">")
  , ("gte",">=")
  , ("lt","<")
  , ("lte","<=")
  , ("eq","==")
  , ("ne","!=")
  , ("shl","<<")
  , ("shr",">>")
  , ("band","&")
  , ("bor","|")
  , ("bxor","^")
  , ("bnot","~")
  , ("and","&&")
  , ("or","||")
  ]

nCnt :: [Type] -> Number
nCnt [t@(Type cs [])] | isCnt t = filter isDigit cs
nCnt _ = error "nCnt"

isCnt :: Type -> Bool
isCnt (Type ('C':'n':'t':cs) []) = not (null cs) && all isDigit cs
isCnt _ = False

has_suffix :: Eq a => [a] -> [a] -> Bool
has_suffix a b = drop (length b - length a) b == a

drop_suffix :: [a] -> [a] -> [a]
drop_suffix a b = take (length b - length a) b

promote :: Number -> Number
promote s
  | x <= 8 = "8"
  | x <= 16 = "16"
  | x <= 32 = "32"
  | x <= 64 = "64"
  | x <= 128 = "128"
  | otherwise = error $ "too many bits required to represent number:" ++ show x
  where
    x = readNumber s :: Integer
    
strip_underscore :: String -> String
strip_underscore "" = ""
strip_underscore s
  | last s == '_' = case reverse $ dropWhile ((==) '_') $ reverse s of
      [] -> "_"
      s1 -> s1
  | otherwise = s

isSigned :: Type -> Bool
isSigned x = case x of
  Type "I_" [_] -> True
  _ -> False

isUnsigned :: Type -> Bool
isUnsigned x = case x of
  Type "W_" [_] -> True
  _ -> False

isFloating :: Type -> Bool
isFloating x = case x of
  Type "Double_" [] -> True
  Type "Float_" [] -> True
  _ -> False

inlineAtoms :: Define -> Define
inlineAtoms x = transformBi h $ rewriteBi g $ rewriteBi f x
  where
    tbl = [ (a,b) | LetS a (AtomE b) <- universeBi x ]
    f (VarA v) = lookup v tbl
    f _ = Nothing
    g v = case lookup v tbl of
      Just (VarA v1) -> Just v1
      _ -> Nothing
    h (LetS _ AtomE{}) = NoOpS
    h s = s

tyEnums :: TyDecl -> [(String,Integer)]
tyEnums x = zip (sort ss) [ 0 .. ]
  where
  ss = [ a | EnumC a <- universeBi x ] ++ [ a | ConC a _ <- universeBi x ]

tyFields :: TyDecl -> [(String,Integer)]
tyFields x = zip [ a ++ "fld" | FieldT a _ <- universeBi x ] [ 0 .. ]
