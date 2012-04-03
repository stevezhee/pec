{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.PUtil where

import Control.Monad
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Development.Shake.FilePath
import Distribution.Text
import Grm.Layout
import Grm.Lex
import Grm.Prims
import Paths_pec
import System.Console.CmdArgs
import qualified Language.Pds.Abs as D
import qualified Language.Pds.Par as PD
import qualified Language.Pec.Abs as P
import qualified Language.Pec.Par as PP

copyright :: String
copyright = "(C) Brett Letner 2011-2012"

vers :: String
vers = display version

getLibDir :: IO FilePath
getLibDir = liftM takeDirectory $ getDataFileName "lib/Prelude.pec"

parse_m ::
  (FilePath -> IO [Token Point]) -> ([Token Point] -> a) -> (a -> String) -> FilePath -> IO a
parse_m lexF parF modidF fn = do
  ts <- lexF fn
  m <- return $ parF $ filter notWSToken ts
  let n = modidF m
  if ((splitDirectories $ dropExtension $ normalise fn) `has_suffix` unqual n)
     then return m
     else error $ "module name mismatch:" ++ fn ++ ":" ++ n
  
parse_pec :: FilePath -> IO (P.Module Point)
parse_pec = parse_m P.grmLexFilePath (PP.grmParse . layout) modid_p
  
parse_pds :: FilePath -> IO D.Module
parse_pds = parse_m D.grmLexFilePath PD.grmParse (init . modid_d)

readFileDeps :: FilePath -> IO ([String],[String])
readFileDeps fn = do
  D.Module _ _ xs ys _ _ _ <- parse_pds fn
  return ([ init x | D.ImportD x <- xs], [ y | D.CountD y <- ys])
  
has_suffix :: Eq a => [a] -> [a] -> Bool
has_suffix a b = drop (length a - length b) a == b

imports :: D.Module -> [String]
imports (D.Module _ _ xs _ _ _ _) = [ a | D.ImportD a <- xs ]

modid_d :: D.Module -> String
modid_d (D.Module n _ _ _ _ _ _) = n

modid_p :: P.Module a -> String
modid_p (P.Module _ n _ _ _) = ppShow n

counts :: P.Module Point -> [Integer]
counts (P.Module _ _ _ _ xs) = nub $
  [ genericLength (a :: [P.ConC Point])
  | P.TyTagged _ a <- universeBi xs ] ++
  [ genericLength (a :: [P.Exp Point]) | P.ArrayE _ a <- universeBi xs ] ++
  [ pcount a | P.TyCount _ a <- universeBi xs ] ++
  [ pcount a | P.CountE _ a <- universeBi xs ]

pcount :: P.Count Point -> Integer
pcount (P.Count _ i) = read i

qual_to_und :: String -> String
qual_to_und = map f
  where f c = if c == '.' then '_' else c

und_to_path :: String -> String
und_to_path = joinPath . splitBy ((==) '_')

unqual :: String -> [String]
unqual = splitBy ((==) '.')

unqual_name :: String -> String
unqual_name s = case unqual s of
  [] -> ""
  xs -> last xs

qual :: [String] -> String
qual = concat . intersperse "."

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = loop [] []
  where
    loop [] ys [] = filter (not . null) $ reverse ys
    loop xs ys [] = loop [] (reverse xs : ys) []
    loop xs ys (c:cs)
      | p c = loop [] (reverse xs : ys) cs
      | otherwise = loop (c:xs) ys cs
        
summarize :: String -> String
summarize prog = prog ++ " v" ++ vers ++ ", " ++ copyright

data Arch = C | LLVM deriving (Show, Data, Typeable)

instance Default Arch where
  def = C
