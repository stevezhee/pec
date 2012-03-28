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
import Language.Pec.Abs
import Language.Pec.Abs as P
import Language.Pec.Par
import Paths_pec
import System.Console.CmdArgs
import qualified Language.Pds.Abs as D

copyright :: String
copyright = "(C) Brett Letner 2011-2012"

parse_pec :: FilePath -> IO (P.Module Point)
parse_pec fn = do
  ts <- P.grmLexFilePath fn
  let m@(P.Module _ a _ _ _) = grmParse $ layout $ filter notWSToken ts
  if ((splitDirectories $ dropExtension $ normalise fn) `has_suffix`
      (unqual $ ppShow a))
     then return m
     else error $ "module name mismatch:" ++ fn ++ ":" ++ ppShow a
  
has_suffix :: Eq a => [a] -> [a] -> Bool
has_suffix a b = drop (length a - length b) a == b

imports :: D.Module -> [String]
imports (D.Module _ _ xs _ _ _) = [ a | D.ImportD a _ <- xs ]

modid :: D.Module -> String
modid (D.Module n _ _ _ _ _) = n

counts :: P.Module Point -> [Integer]
counts (P.Module _ _ _ _ xs) = nub $
  [ genericLength (a :: [P.ConC Point])
  | P.TyTagged _ a <- universeBi xs ] ++
  [ genericLength (a :: [P.Exp Point]) | ArrayE _ a <- universeBi xs ] ++
  [ pcount a | TyCount _ a <- universeBi xs ] ++
  [ pcount a | CountE _ a <- universeBi xs ]

pcount :: Count Point -> Integer
pcount (Count _ i) = read i

qual_to_und :: String -> String
qual_to_und = map f
  where f c = if c == '.' then '_' else c

und_to_path :: String -> String
und_to_path = joinPath . splitBy ((==) '_')

unqual :: String -> [String]
unqual = splitBy ((==) '.')

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
summarize prog = prog ++ " v" ++ display version ++ ", " ++ copyright

readFileDeps :: FilePath -> IO ([String],[Integer])
readFileDeps = liftM read . readFile

writeFileDeps :: FilePath -> ([String],[Integer]) -> IO ()
writeFileDeps fn = writeFileBinary fn . show

data Arch = C | LLVM deriving (Show, Data, Typeable)

instance Default Arch where
  def = C
