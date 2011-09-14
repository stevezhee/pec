{-# OPTIONS -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, GADTs #-}

-- The pec embedded compiler
-- Copyright 2011, Brett Letner

module Pec.Base

where

import Control.Monad
import Control.Monad.State hiding (lift)
import Data.Char
import Data.List
import Data.Unique
import Numeric
import System.IO
import qualified Control.Monad.State as S

data V a = V String
data Decl a = Decl Id (Term a)
data Ptr a
data Tag a cnt
data Idx cnt
data Array cnt a
data I cnt
data IString
data W cnt
data Cnt256 = Cnt256
data Cnt4294967296 = Cnt4294967296
data SuccCnt cnt

data Term a where
  Arg :: (Typed a, Typed b) => Id -> (Term a -> Term b) -> Term (a -> b)
  Val :: Typed a => V a -> Term a
  App :: (Typed a, Typed b) => Term (a -> b) -> Term a -> Term b
  Lift :: Typed a => M (Term a) -> Term a

data Ty
  = TyUnit
  | TyEnum Integer
  | TyPtr Ty
  | TyArray Integer Ty
  | TyPair Ty Ty
  | TySum [Ty]
  | TyFun Ty Ty
  | TyDouble
  | TyFloat
  deriving Eq

data St = St
  { istrings_tbl :: [String]
  , outH :: Handle
  , last_label :: Label
  }

type CntW32 = Cnt4294967296
type W32 = W CntW32
type Label = String
type M a = StateT St IO a
type Id = String
type CString = Ptr Char
type W_ a = W a
type I_ a = I a
type SuccCnt_ a = SuccCnt a

class Count cnt where countof :: cnt -> Integer

instance Count cnt => Count (SuccCnt cnt) where
  countof _ = succ $ countof (unused :: cnt)

class (Count cnt, Typed a) => Tagged a cnt | a -> cnt where
  tagof :: Term a -> Term (Tag a cnt)

class Typed a where
  typeof :: a -> Ty
  callt :: Id -> [(Id,Ty)] -> Term a
  callt n bs = with_local $ \(v :: V a) -> do
    let t = typeof (unused :: a)
    let pre = if is_tyunit t then [] else [vof v ++ " ="]
    out 2 $ pre ++ ["call", tyof t, n ++ args (reverse bs)]

class (Typed a, Typed b) => Newtype a b | a -> b where
  unwrap_ :: Term (a -> b)
  unwrap_ = wrap

  unwrap_ptr_ :: Term (Ptr a -> Ptr b)
  unwrap_ptr_ = lam_ cast

class INT a where int :: Integer -> Term a

instance Count Cnt256 where countof _ = 256
instance Count Cnt4294967296 where countof _ = 4294967296
instance Count cnt => INT (I cnt) where int = sint unused
instance Count cnt => INT (Idx cnt) where int = uint unused
instance Count cnt => INT (W cnt) where int = uint unused
instance Count cnt => Tagged (I cnt) cnt where tagof = cast
instance Count cnt => Tagged (W cnt) cnt where tagof = cast
instance Tagged Char Cnt256 where tagof = cast
instance Tagged IString CntW32 where tagof = cast
instance Typed () where typeof _ = TyUnit
instance Typed Char where typeof _ = TyEnum 256
instance Typed Double where typeof _ = TyDouble
instance Typed Float where typeof _ = TyFloat
instance Typed IString where typeof _ = TyEnum $ countof (unused :: CntW32)

instance Count cnt => Typed (SuccCnt cnt) where typeof _ = TyUnit

instance Typed a => Typed (Ptr a) where
  typeof (_ :: Ptr a) = TyPtr $ typeof (unused :: a)

instance (Typed a, Count cnt) => Typed (Array cnt a) where
  typeof (_ :: Array cnt a) =
    TyArray (countof (unused :: cnt)) (typeof (unused :: a))

instance (Typed a, Typed b) => Typed (a -> b) where
  typeof (_ :: a -> b) =
    TyFun (typeof (unused :: a)) (typeof (unused :: b))

  callt n bs = Arg n $ \a -> Lift $ do
      V s <- evalv a
      return $ callt n ((s, typeof (unused :: a)) : bs)

instance (Typed a, Typed b) => Typed (a, b) where
  typeof (_ :: (a, b)) =
    TyPair (typeof (unused :: a)) (typeof (unused :: b))

instance Count cnt => Typed (I cnt) where
  typeof (_ :: I cnt) = TyEnum (countof (unused :: cnt))

instance Count cnt => Typed (W cnt) where
  typeof (_ :: W cnt) = TyEnum (countof (unused :: cnt))

instance Count cnt => Typed (Idx cnt) where
  typeof (_ :: Idx cnt) = TyEnum (countof (unused :: cnt))

instance (Typed a, Count cnt) => Typed (Tag a cnt) where
  typeof (_ :: Tag a cnt) = TyEnum $ countof (unused :: cnt)

uint :: (Count cnt, Typed (f cnt)) => f cnt -> Integer -> Term (f cnt)
uint (_ :: f cnt) x
  | x >= 0 && x < countof (unused :: cnt) = tag x
  | otherwise = error $ "unsigned integer out of range:" ++ show x

sint :: (Count cnt, Typed (f cnt)) => f cnt -> Integer -> Term (f cnt)
sint (_ :: f cnt) x
  | x >= -y && x < y = tag x
  | otherwise = error $ "signed integer out of range:" ++ show x
  where
  y = countof (unused :: cnt) `div` 2

wrap :: (Typed a, Typed b) => Term (a -> b)
wrap = lam_ cast

unwrap2 :: (Newtype a b, Typed c) =>
  (Term b -> Term b -> Term c) -> Term a -> Term a -> Term c
unwrap2 f = \a b -> f (app unwrap_ a) (app unwrap_ b)

unused :: a
unused = error "unused"

cast :: (Typed a, Typed b) => Term a -> Term b
cast f = Lift $ do
  V a <- evalv f
  return $ val a

tagofp :: (Typed a, Count cnt) => Term (Ptr a) -> Term (Tag (Ptr a) cnt)
tagofp = load . tagp

alt0 :: (Typed a, Typed b) => (Term () -> Term b) -> Term a -> Term b
alt0 f _ = f unit

alt :: (Typed a, Typed b, Typed c) =>
  (Term (Ptr b) -> Term c) -> Term (Ptr a) -> Term c
alt f = f . datap

constr :: (Typed a, Count cnt, Typed b) =>
  Term (Tag (Ptr a) cnt) -> Term (b -> a)
constr tg = lam_ $ \f -> Lift $ do
  p <- eval $ alloca unused
  eval_ $ store (tagp p) tg
  eval_ $ store (datap p) f
  return $ load p

constr0 :: (Typed a, Count cnt) => Term (Tag (Ptr a) cnt) -> Term a
constr0 tg = Lift $ do
  p <- eval $ alloca unused
  eval_ $ store (tagp p) tg
  return $ load p

tagp :: (Typed a, Count cnt) =>
  Term (Ptr a) -> Term (Ptr (Tag (Ptr a) cnt))
tagp = gep (tag 0 :: Term W32)

datap :: (Typed a, Typed b) => Term (Ptr a) -> Term (Ptr b)
datap (f :: Term (Ptr a)) = with_local $ \r -> do
  p <- evalv f
  q <- new_local
  let TySum ts = typeof (unused :: a)
  out 2 [ vof q, "= getelementptr", vtof p ++ ", i32 0, i32 1" ]
  out 2 [ vof r, "= bitcast", tyof $ TyPtr $ max_tysum ts, vof q
        , "to", tof r ]

bitcast :: (Typed a, Typed b) => Term (Ptr a) -> Term (Ptr b)
bitcast x = with_local $ \q -> do
  p <- evalv x
  out 2 [ vof q, "= bitcast", vtof p, "to", tof q ]

gep :: (Typed a, Typed b) => Term i -> Term (Ptr a) -> Term (Ptr b)
gep f g = with_local $ \q -> do
  i <- evalv f
  p <- evalv g
  out 2 [ vof q, "= getelementptr", vtof p ++ ", i32 0, i32", vof i ]

load :: Typed a => Term (Ptr a) -> Term a
load f = with_local $ \v -> do
  p <- evalv f
  out 2 [ vof v, "= load", vtof p ]

prim2 :: (Typed a, Typed b, Typed c) =>
  String -> Term a -> Term b -> Term c
prim2 s f g = with_local $ \c -> do
  a <- evalv f
  b <- evalv g
  out 2 [ vof c, "=", s, vtof a ++ ",", vof b ]

tag :: Typed a => Integer -> Term a
tag = val . show

store :: Typed a => Term (Ptr a) -> Term a -> Term ()
store f g = Lift $ do
  p <- evalv f
  a <- evalv g
  out 2 [ "store", vtof a ++ ",", vtof p ]
  return unit

pair :: (Typed a, Typed b) => Term a -> Term b -> Term (a,b)
pair f g = Lift $ do
  p <- eval $ alloca unused
  eval_ $ store (fst_get p) f
  eval_ $ store (snd_get p) g
  return $ load p

fst_get :: (Typed a, Typed b) => Term (Ptr (a,b)) -> Term (Ptr a)
fst_get = gep (tag 0 :: Term W32)

snd_get :: (Typed a, Typed b) => Term (Ptr (a,b)) -> Term (Ptr b)
snd_get = gep (tag 1 :: Term W32)

fst_ :: (Typed a, Typed b) => Term (Ptr (a,b) -> Ptr a)
fst_ = lam_ fst_get

snd_ :: (Typed a, Typed b) => Term (Ptr (a,b) -> Ptr b)
snd_ = lam_ snd_get

array :: (Count cnt, Typed a) => Term cnt -> [Term a] -> Term (Array cnt a)
array _ xs = Lift $ do
  p <- eval $ alloca unused
  sequence_ [ eval $ store (idx p (int i)) x | (i,x) <- zip [0 .. ] xs ]
  return $ load p

new_ :: Typed a => Term (a -> Ptr a)
new_ = lam_ new

alloca :: Typed a => Term a -> Term (Ptr a)
alloca (_ :: Term a) = with_local $ \p -> do
  out 2 [ vof p, "= alloca", tof (unused :: V a) ]

alloca'_ :: Typed a => Term (Ptr a)
alloca'_= alloca unused

new :: Typed a => Term a -> Term (Ptr a)
new f = Lift $ do
  p <- eval $ alloca f
  eval_ $ store p f
  return p

lam3_ :: (Typed a, Typed b, Typed c, Typed d) =>
  (Term a -> Term b -> Term c -> Term d) -> Term (a -> b -> c -> d)
lam3_ f = lam_ $ \x -> lam_ $ \y -> lam_ $ \z -> f x y z

lam2_ :: (Typed a, Typed b, Typed c) =>
  (Term a -> Term b -> Term c) -> Term (a -> b -> c)
lam2_ f = lam_ $ \x -> lam_ $ \y -> f x y

app3 :: (Typed a, Typed b, Typed c, Typed d) =>
  Term (a -> b -> c -> d) -> Term a -> Term b -> Term c -> Term d
app3 f a b = app (app2 f a b)

app2 :: (Typed a, Typed b, Typed c) =>
  Term (a -> b -> c) -> Term a -> Term b -> Term c
app2 f = app . app f

arg :: (Typed a, Typed b) => Id -> (Term a -> Term b) -> Term (a -> b)
arg s = Arg ("%" ++ s)

unitarg :: Typed a => Term a -> Term (() -> a)
unitarg x = arg (error "UNITLAM") (\_ -> x)

args :: [(Id,Ty)] -> String
args xs = parens $ commaSep $ map (\(s,t) -> tyof t ++ " " ++ s) $
            filter (not . is_tyunit . snd) xs

parens :: String -> String
parens s = "(" ++ s ++ ")"

argsof :: Typed a => Term a -> [(Id,Ty)]
argsof (x :: Term a) = case x of
  Arg s (f :: Term b -> Term c) ->
    (s, typeof (unused :: b)) : argsof (f (unused :: Term b))
  _ -> [(error "ARGSOF", typeof (unused :: a))]

define :: Typed a => Decl a -> M ()
define (Decl n a) = do
  out 0 ["define", tyof $ snd $ last xs, n ++ args (init xs) ]
  out 2 ["{"]
  loop a
  out 2 ["}"]
  where
  xs = argsof a
  loop :: Typed a => Term a -> M ()
  loop (x :: Term a) = case x of
    Arg s f -> loop (f $ val s)
    _ -> do
      v <- evalv x
      out 2 [ "ret"
            , if is_tyunit (typeof (unused :: a)) then "void" else vtof v
            ]

switch :: (Tagged a cnt, Typed b) =>
  Term a -> [(Term (Tag a cnt), Term a -> Term b)] -> Term b
switch a bs = Lift $ do
  v <- eval a
  tg <- evalv $ tagof v
  ls <- sequence $ replicate (length bs) new_label
  out 2 [ "switch", vtof tg ++ ",", lblof (last ls) ]
  out 4 ["["]
  let zs = zip bs ls
  sequence_
    [ do u <- evalv t
         out 4 [commaSep [vtof u, lblof l]]
      | ((t,_),l) <- init zs
    ]
  out 4 ["]"]
  done <- new_label
  cs <- mapM (eval_alt v done) zs
  lblout done
  return $ phi cs

phi :: Typed a => [(V a, Label)] -> Term a
phi xs = with_local $ \(v :: V a) ->
  when (not $ is_tyunit $ typeof (unused :: a)) $
    out 2 [ vof v, "= phi", tof v, commaSep $ map phi_arg xs ]

phi_arg :: Typed a => (V a, Label) -> String
phi_arg (x,l) = brackets (vof x ++ ", %" ++ l)

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

eval_alt :: (Typed a, Typed b) =>
  Term a -> Label -> ((z, Term a -> Term b), Label) -> M (V b, Label)
eval_alt a done ((_,f),l)  = do
  lblout l
  v <- evalv $ f a
  out 2 [ "br", lblof done ]
  m <- gets last_label
  return (v,m)

lblout :: Label -> M ()
lblout l = do
  out 0 [l ++ ":"]
  modify $ \st -> st{ last_label = l }

lblof :: Label -> String
lblof l = "label %" ++ l

from_istring_ :: Term (IString -> CString) -- todo:should return immutable cstring
from_istring_ = lam_ $ \f -> with_local $ \s -> do
  i <- evalv f
  p :: V (Ptr CString) <- new_local
  xs <- gets istrings_tbl
  out 2 [ vof p, "= getelementptr [" ++ show (length xs)
        , " x i8*]* @.istrings, i32 0,", vtof i
        ]
  out 2 [ vof s, "= load", vtof p]

app :: (Typed a, Typed b) => Term (a -> b) -> Term a -> Term b
app = App

evalv :: Term a -> M (V a)
evalv x = case x of
  Arg a _ -> return $ V a
  Val a -> return a
  _ -> reduce x >>= evalv

reduce :: Term a -> M (Term a)
reduce x = case x of
  App f a -> case f of
    Arg _ g -> return $ g a
    Val (V n) -> reduce $ App (callt n []) a
    _ -> reduce f >>= \v -> return (App v a)
  Lift f -> f
  _ -> return x

val :: Typed a => String -> Term a
val = Val . V

is_tyunit :: Ty -> Bool
is_tyunit x = x == TyUnit

tof :: Typed a => V a -> String
tof (_ :: V a) = tyof $ typeof (unused :: a)

proc :: Typed a => Id -> Term a -> Decl a
proc a = Decl ("@" ++ a)

lift :: Typed a => M (Term a) -> Term a
lift = Lift

extern :: Typed a => Id -> Decl a
extern n = Decl ("@" ++ n) unused

gen_files :: FilePath -> M () -> [String] -> IO ()
gen_files fn f ss = evalStateT (f >> gen_istrings fn) (St ss (error "gen_files:outH") "")

gen_file :: FilePath -> M () -> M ()
gen_file fn f = do
  h <- S.lift $ openFile outFn WriteMode
  modify $ \st -> st{ outH = h }
  () <- f
  modify $ \st -> st{ outH = error "outH" }
  S.lift $ hClose h
  where
  outFn = fn ++ ".ll"

gen_istrings :: FilePath -> M ()
gen_istrings fn = gen_file ("istrings_" ++ fn) $ do
  xs <- gets istrings_tbl
  when (not $ null xs) $ do
    let ys = [ ("@.istr" ++ show i, cs, show $ length cs + 1)
               | (i,cs) <- zip [0 :: Int .. ] xs ]
    mapM_ f ys
    out 0 ["@.istrings = global [" ++ show (length ys) ++ " x i8*]"]
    out 2 ["["]
    mapM_ (g ",") (init ys)
    g "" (last ys)
    out 2 ["]"]
  where
  f (i,s,n) =
    out 0 [i, "= private constant [" ++ n ++ " x i8]", const_cstring s ]
  g s (i,_,n) =
    out 2 [ "i8* getelementptr ([" ++ n, "x i8]*"
          , i ++ ", i32 0, i32 0)" ++ s]

string :: String -> Term IString
string s = Lift $ do
  xs <- gets istrings_tbl
  case elemIndex s xs of
    Just i -> return $ tag $ fromIntegral i
    Nothing -> error "STRING"

tyof :: Ty -> String
tyof x = case x of
  TyEnum{} -> "i" ++ show (sizeof x)
  TyUnit -> "void"
  TyPtr a -> tyof a ++ "*"
  TyPair a b -> "{" ++ tyof a ++ ", " ++ tyof b ++ "}"
  TyArray i a -> brackets $ show i ++ " x " ++ tyof a
  TySum xs -> tyof $ ty_sum xs
  TyDouble -> "double"
  TyFloat -> "float"
  TyFun{} -> (tyof $ last xs) ++ parens (commaSep $ map tyof $ init xs) ++ "*"
    where xs = unfold_tyfun x

unfold_tyfun :: Ty -> [Ty]
unfold_tyfun x = case x of
  TyFun a b -> a : unfold_tyfun b
  _ -> [x]

ty_sum :: [Ty] -> Ty
ty_sum xs = TyPair (TyEnum $ genericLength xs) $ max_tysum xs

max_tysum :: [Ty] -> Ty
max_tysum = maximumBy (\a b -> compare (sizeof a) (sizeof b))

sizeofptr :: Integer
sizeofptr = 32 -- fixme:non-portable

idx :: (Count cnt, Typed a) =>
  Term (Ptr (Array cnt a)) -> Term (Idx cnt) -> Term (Ptr a)
idx = flip gep

sizeof :: Ty -> Integer
sizeof x = case x of
  TyEnum i -> bitsToEncode i
  TyUnit -> 0
  TyPtr{} -> sizeofptr
  TyPair a b -> sizeof a + sizeof b
  TyFun{} -> sizeofptr
  TyArray i a -> i * sizeof a
  TySum xs -> sizeof $ ty_sum xs
  TyDouble -> 64
  TyFloat -> 32

char :: Char -> Term Char
char = tag . fromIntegral . ord

eval :: Typed a => Term a -> M (Term a)
eval = liftM Val . evalv

eval_ :: Typed a => Term a -> M ()
eval_ x = eval x >> return ()

lam_ :: (Typed a, Typed b) => (Term a -> Term b) -> Term (a -> b)
lam_ = Arg (error "lam")

with_local :: Typed a => (V a -> M ()) -> Term a
with_local f = Lift $ do
  v <- new_local
  f v
  return $ Val v

vof :: V a -> String
vof (V v) = v

vtof :: Typed a => V a -> String
vtof a = unwords [tof a, vof a]

out :: Int -> [String] -> M ()
out i xs = do
  h <- gets outH
  S.lift $ hPutStrLn h $ replicate i ' ' ++ unwords xs

new_local :: M (V a)
new_local = liftM (V . (++) "%v") $ S.lift freshNm

new_label :: M Label
new_label = liftM ((++) "LBL") $ S.lift freshNm

unit :: Term ()
unit = val $ error "UNIT"

call :: Typed a => Decl a -> Term a
call (Decl n _) = callt n []

declare :: Typed a => Decl a -> M ()
declare (Decl n (_ :: Term a)) =
  out 0 ["declare", tyof $ last xs, n ++ args_ (init xs) ]
  where
  xs = loop (typeof (unused :: a))
  loop t = case t of
    TyFun a b -> [a] ++ loop b
    _ -> [t]

args_ :: [Ty] -> String
args_ xs = parens $ commaSep $ map tyof $ filter (not . is_tyunit) xs

const_cstring :: String -> String
const_cstring s = "c\"" ++ concatMap const_char s ++ "\\00\""

const_char :: Char -> String
const_char c
  | c < ' ' || c > '~' || c == '\\' = encode_char c
  | otherwise = [c]

encode_char :: Enum a => a -> String
encode_char c =
  '\\' : (if i <= 0xf then "0" else "") ++ map toUpper (showHex i "")
  where i = fromEnum c

bitsToEncode :: Integer -> Integer
bitsToEncode 0 = 0
bitsToEncode i = ceiling $ logBase 2 (fromIntegral i :: Double)

freshNm :: IO String
freshNm = liftM (show . hashUnique) newUnique

commaSep :: [String] -> String
commaSep = concat . intersperse ", "

inttag :: (INT a, Typed a, Count cnt) => Integer -> Term (Tag a cnt)
inttag = inttagt unused

inttagt :: (INT a, Typed a, Count cnt) => a -> Integer -> Term (Tag a cnt)
inttagt (_ :: a) i = Lift $ do
  V s :: V a <- evalv (int i)
  return $ val s

chartag :: Char -> Term (Tag Char Cnt256)
chartag = cast . char

stringtag :: String -> Term (Tag IString CntW32)
stringtag = cast . string

defaulttag :: Tagged a cnt => Term (Tag a cnt)
defaulttag = val $ error "DEFAULT"
