{-# OPTIONS -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Main(main) where

import Control.Monad
import Data.List
import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Grm.Prims
import Pec.PUtil
import System.Console.CmdArgs
import System.Directory hiding (readable)

data Args = Args
  { targets :: [String]
  , idir :: [FilePath]
  , lib :: [String]
  , ldir :: [FilePath]
  , march :: Arch
  , readable :: Bool
  } deriving (Show, Data, Typeable)

argsDesc :: Args
argsDesc = Args
  { targets = def &= args
  , idir = def &= typDir &= help "import directory"
  , lib = def &= help "library"
  , ldir = def &= typDir &= name "L" &= help "library directory"
  , march = def &= help "arch to build (C or LLVM)"
  , readable = def &= help "generate human readable C (experimental)"
  } &= summary summry &= program prog

summry :: String
summry = prog ++ " v" ++ vers ++ ", " ++ copyright

prog :: String
prog = "pec"

buildDir :: String
buildDir = ".build"

mkTarget :: String -> String
mkTarget t =
  if takeExtension t == ".pec" then replaceExtension t "exe" else t

getExeFiles :: IO [FilePath]
getExeFiles = liftM (filter isExeFile) (getDirectoryContents ".")

isExeFile :: FilePath -> Bool
isExeFile = (==) ".exe" . takeExtension

rmrf :: FilePath -> IO ()
rmrf fn = do
  r <- doesDirectoryExist fn
  when r $ removeDirectoryRecursive fn
  
main :: IO ()
main = do
  a <- cmdArgs argsDesc
  libDir <- getLibDir
  let idirs = ["."] ++ idir a ++ [libDir]

  when ("clean" `elem` targets a) $ do
    rmrf buildDir
    getExeFiles >>= mapM_ removeFile
    
  let ts = map mkTarget $ targets a \\ ["clean","run"]

  let do_runghc outFn = do
        let hsFn = dropExtension outFn ++ "_.hs"
        need [hsFn]
        system' "runghc"
          [ "-i" ++ buildDir, hsFn
          , "--march=" ++ show (march a)
          , "--readable=" ++ show (readable a)]
      
  -- shake shakeOptions{ shakeVerbosity = Diagnostic } $ do
  shake shakeOptions $ do

    want ts

    "//*.exe" *> \outFn -> case march a of
      C -> do
        let pdsFn = buildPath $ replaceExtension outFn "pds"
        need [pdsFn]
        xs <- loadFileDeps pdsFn
        let fns = [ replaceExtension x "o" | x <- xs ]
        need fns
        system' "gcc" $
          ["-g", "-O2", "-Wall", "-Wextra", "-o", outFn] ++
          ["-L" ++ l | l <- ldir a] ++ ["-l" ++ l | l <- lib a] ++ fns
      LLVM -> do
        let fn = buildPath $ replaceExtension outFn "s"
        need [fn]
        system' "gcc" [ "-O2", "-o", outFn, fn ]

    "//*.s" *> \outFn -> do
      let fn = replaceExtension outFn "opt"
      need [fn]
      system' "llc" [ "-o", outFn, fn ]

    "//*.opt" *> \outFn -> do
      let fn = replaceExtension outFn "bca"
      need [fn]
      system' "opt" [ "-o", outFn, "-std-compile-opts", "-std-link-opts", fn ]
  
    "//*.bca" *> \outFn -> do
      let pdsFn = replaceExtension outFn "pds"
      need [pdsFn]
      xs <- loadFileDeps pdsFn
      let fns = [ replaceExtension x "bc" | x <- xs ]
      need fns
      system' "llvm-link" $ [ "-o", outFn ] ++ fns

    "//*.o" *> \outFn -> do
      let fn = replaceExtension outFn "c"
      need [fn, replaceExtension outFn "h"]
      system' "gcc" ["-g", "-o", outFn, "-c", fn ]
      
    "//*.c" *> do_runghc
    "//*.h" *> do_runghc
    "//*.ll" *> do_runghc

    "//*.bc" *> \outFn -> do
      let fn = replaceExtension outFn "ll"
      need [fn]
      system' "llvm-as" ["-o", outFn, fn ]

    "//*_.hs" *> \hsOut -> do
      need [(init $ dropExtension hsOut) ++ ".pds"]

    "//*.pds" *> \pdsOut -> do
      let fn = dropDirectory1
            (und_to_path (dropExtension pdsOut) ++ ".pec")
      mFn <- liftIO $ findFile idirs fn
      case mFn of
        Nothing -> error $ "unknown file:" ++ fn
        Just fn1 -> do
          need [fn1] -- BAL: needed?
          system' "pecgen" ["-d", buildDir, fn1]
          (xs,ys) <- liftIO $ readFileDeps pdsOut
          need [ buildPath $ x ++ ".pds" | x <- xs ]
          need [ buildPath $ "Cnt" ++ y ++ ".hs" | y <- ys ]

    "//Cnt*.hs" *> \outFn -> system' "pecgencnt" [outFn]
      
    when ("run" `elem` targets a) $ action $ do -- BAL: doesn't work under windows
      pwd <- liftIO getCurrentDirectory
      mapM_ (\fn -> system' (pwd </> fn) []) $ filter isExeFile ts

loadFileDeps :: FilePath -> Action [String]
loadFileDeps fn0 = loop [] [fn0]
  where
  loop xs [] = return xs
  loop xs (y:ys)
    | y `elem` xs = loop xs ys
    | otherwise = do
        (zs,_) <- liftIO $ readFileDeps y
        loop (y : xs) ([ buildPath $ z ++ ".pds" | z <- zs ] ++ ys)
                     
buildPath :: FilePath -> FilePath
buildPath x = buildDir </> x
