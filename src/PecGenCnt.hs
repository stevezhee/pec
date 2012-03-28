{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Main where

import Data.Char
import Grm.Prims
import Pec.HUtil
import Pec.PUtil
import System.Console.CmdArgs
import System.FilePath
import qualified Language.Haskell as H

data Args = Args
  { targets :: [String]
  } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  a <- cmdArgs argsDesc
  case targets a of
    [] -> putStrLn $ summarize prog
    xs -> mapM_ hCountModule xs

hCountModule :: FilePath -> IO ()
hCountModule fn0 = do
  writeFileBinary (joinPath [d,fn]) $ H.prettyPrint $
    hmodule n ["EmptyDataDecls"] Nothing
    [ importDecl_ False "Pec.Base" ]
    [ hdatadecl n [] []
    , hinstdecl [] "Count" [H.tyCon n]
      [H.InsDecl $
       H.simpleFun nl (H.name "countof") (H.name "_") (H.intE i)]
    , hinstdecl [] "Typed" [H.tyCon n]
      [ H.InsDecl $
        H.simpleFun nl (H.name "ty") (H.name "_")
        (H.App (H.var "tyCnt") (H.intE i))
      ]
    , H.nameBind nl (H.Ident m) $ H.ExpTypeSig nl
      (H.App (H.var "varE") (hString m)) $
      H.TyApp (H.tyCon "E") (H.tyCon n)
    ]
  where
    d = takeDirectory fn0
    i = read $ filter isDigit $ takeBaseName fn0
    n = "Cnt" ++ show i
    m = lowercase n
    fn = n ++ ".hs"

argsDesc :: Args
argsDesc = Args
  { targets = def &= args
  } &= summary (summarize prog) &= program prog

prog :: String
prog = "pecgencnt"
