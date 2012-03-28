{-# OPTIONS -Wall #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Pec.HUtil where

import Data.List
import Grm.Prims
import Text.PrettyPrint.Leijen
import qualified Language.Haskell as H

nl :: H.SrcLoc
nl = error "SRCLOC"

hmodule :: String -> [String] -> Maybe [H.ExportSpec] -> [H.ImportDecl] -> [H.Decl] -> H.Module
hmodule n ps = H.Module nl (H.ModuleName n)
  [H.LanguagePragma nl [H.name p] | p <- ps ] Nothing

importDecl :: [String] -> Bool -> String -> H.ImportDecl
importDecl xs q n = (importDecl_ q n){ H.importSpecs = Just (True, map (H.IVar . H.name) xs) }

importDecl_ :: Bool -> String -> H.ImportDecl
importDecl_ q n = H.ImportDecl
  { H.importModule = H.ModuleName n
  , H.importQualified = q
  , H.importLoc = nl
  , H.importSrc = False
  , H.importPkg = Nothing
  , H.importAs = Nothing
  , H.importSpecs = Nothing
  }

hinstdecl :: H.Context -> String -> [H.Type] -> [H.InstDecl] -> H.Decl
hinstdecl qs n = H.InstDecl nl (nub $ sort qs) (H.qname n)

hdatadecl :: String -> [H.TyVarBind] -> [H.QualConDecl] -> H.Decl
hdatadecl n vs ds = H.DataDecl nl H.DataType [] (H.name n) vs ds []

doList :: [H.Exp] -> H.Exp
doList xs =
  H.Do $ map H.Qualifier $ xs ++ [H.apps (H.var "return") [H.Tuple []]]

hAppCon :: String -> H.Exp -> H.Exp
hAppCon = H.App . H.con

hString :: Pretty a => a -> H.Exp
hString = H.Lit . H.String . ppShow
