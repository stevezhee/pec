{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Main(main) where

import Control.Monad
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import System.Console.CmdArgs
import System.Directory

data Args = Args
  { targets :: [String]
  } deriving (Show, Data, Typeable)

copyright :: String
copyright = "(C) Brett Letner 2011-2012"

vers :: String
vers = "0.2.2"

argsDesc :: Args
argsDesc = Args
  { targets = def &= args
  } &= summary summry &= program prog

summry :: String
summry = prog ++ " v" ++ vers ++ ", " ++ copyright

prog :: String
prog = "MkPec.exe"

src_files :: [String]
src_files =
  [ "Pec/Base.hs"
  , "Pec/C.hs"
  , "Pec/LLVM.hs"
  , "Pec/Desugar.hs"
  , "Pec/IUtil.hs"
  , "Pec/HUtil.hs"
  , "Pec/PUtil.hs"
  , "src/Pec.hs"
  , "src/PecGen.hs"
  , "src/PecGenCnt.hs"
  , "lib/Prelude.pec"
  , "lib/Data/Stack.pec"
  , "lib/Data/Array.pec"
  , "lib/Data/Deque.pec"
  , "lib/Data/Queue.pec"
  , "lib/Data/StrBuf.pec"
  ]

gen_files :: [String]
gen_files =
    [ "Language/Pec/Abs.hs"
    , "Language/Pec/Par.hs"
    , "Language/Pec/Par.y"
    , "Language/Pds/Abs.hs"
    , "Language/Pir/Abs.hs"
    , "Language/C/Abs.hs"
    , "Language/LLVM/Abs.hs"
    ]

clean :: IO ()
clean = mapM_ rmrf ["dist",".hpc","pec-0.2.0"]

whenHasExe :: String -> Action () -> Action ()
whenHasExe exe f = do
  ma <- liftIO $ findExecutable exe
  when (isJust ma) f

main :: IO ()
main = do
  a <- cmdArgs argsDesc

  let setup = "runhaskell"
  
  when ("clean" `elem` targets a) clean
    
  when ("vclean" `elem` targets a) $ do
    clean
    rmrf "Language"

  shake shakeOptions $ do
    want ["dist/build/autogen/Paths_pec.hs"]

    "Language/*/Par.y" *> \fnOut -> do
      need [ dropFileName fnOut ++ "Abs.hs" ]

    "Language/*/Abs.hs" *> \fnOut -> do
      let fn = grammar fnOut ++ ".grm"
      need [fn]
      whenHasExe "grm" $ if fn == "Pec.grm"
        then system' "grm" [fn, "--locations"]
        else system' "grm" [fn]

    "Language/*/Par.hs" *> \parOut -> do
      let fn = takeDirectory parOut </> "Par.y"
      need [fn]
      whenHasExe "happy" $ system' "happy" ["-o", parOut, fn]

    "dist/setup-config" *> \_ -> do
      need $ src_files ++ gen_files
      need ["pec.cabal", "Setup.hs"]
      system' setup ["Setup.hs", "configure", "--user"]

    "dist/build/autogen/Paths_pec.hs" *> \_ -> do
      need ["dist/setup-config"]
      system' setup ["Setup.hs", "build"]
      system' setup ["Setup.hs", "install"]

grammar :: FilePath -> String
grammar fn = case splitDirectories fn of
  "Language":x:_ -> x
  _ -> error $ "unable to determine grammar name from filename:" ++ fn

rmrf :: FilePath -> IO ()
rmrf fn = do
  r <- doesDirectoryExist fn
  when r $ removeDirectoryRecursive fn
