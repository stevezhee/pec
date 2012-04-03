{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011-2012, Brett Letner

module Main (main) where

import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Grm.Prims
import Language.Haskell as H
import Language.Pds.Abs as P
import Pec.Desugar
import Pec.HUtil
import Pec.PUtil
import System.Console.CmdArgs
import System.Directory
import System.FilePath

data Args = Args
  { files :: [FilePath]
  , dir :: FilePath
  } deriving (Show, Data, Typeable)

argsDesc :: Args
argsDesc = Args
  { files = def &= args
  , dir = def &= typDir &= help "output directory"
  } &= summary (summarize prog) &= program prog

prog :: String
prog = "pecgen"

main :: IO ()
main = do
  args <- cmdArgs argsDesc
  let fns = files args
  case fns of
    [] -> putStrLn $ summarize prog
    [fn] -> do
      m0 <- parse_pec fn
      m1 <- return $ desugar m0
      let n = modid_d m1
      createDirectoryIfMissing True $ dir args
      writeFileBinary (joinPath [dir args, init n ++ ".pds"]) $
        ppShow m1
      m <- return $ hModule (dir args) m1
      writeFileBinary (joinPath [dir args, n ++ ".hs"]) $
        prettyPrint m
    _ -> error "expecting exactly one input file"

hModule :: FilePath -> P.Module -> H.Module
hModule outdir (P.Module a bs cs hs ds es fs) =
  H.Module nl (ModuleName a)
  hPragmas
  Nothing
  (Just $ map hExport bs)
  (map hImport_ (["Pec.Base"] ++ [ "Cnt" ++ h | CountD h <- hs]) ++
   map hImport cs)
  (mainD outdir a cs fs :
   map hDataDecl ds ++
   map hInstD es ++
   catMaybes (map hTyped ds) ++
   catMaybes (map hTagged ds) ++
   map hVarD fs)

hInstD :: P.InstD -> Decl
hInstD (P.InstD a b) = H.InstDecl nl [] (H.qname a) [hType b] []

mainD :: FilePath -> String -> [ImportD] -> [VarD] -> Decl
mainD outdir a bs xs = hNameBind "main" $ apps (var "dModule")
  [ hString outdir
  , hString a
  , H.List [ hString b | ImportD b <- bs]
  , H.List $ catMaybes $ map f xs
  ]
  where
    f x = case x of
      VarD a Define _ -> Just $ apps (var "defn") [var a]
      _ -> Nothing

hLanguagePragma :: String -> H.ModulePragma
hLanguagePragma a = H.LanguagePragma nl [H.name a]

hPragmas :: [ModulePragma]
hPragmas = map hLanguagePragma
  [ "ScopedTypeVariables", "EmptyDataDecls", "NoMonomorphismRestriction" ]

hUnkindedVar :: Var -> TyVarBind
hUnkindedVar = H.UnkindedVar . H.name . ppShow

hDataDecl :: TypeD -> Decl
hDataDecl (TypeD a bs c) = case c of
  TySyn t -> TypeDecl nl n vs (hType t)
  _ -> DataDecl nl DataType [] n vs [] []
  where
  n = H.name $ unqual_name a
  vs = map hUnkindedVar bs

typeTy :: String -> [Var] -> P.Type
typeTy a bs = TyConstr a $ map (TyVarT . unVar) bs

eType :: P.Type -> H.Type
eType x =
  hTyForall [ hClassA "Typed" [P.Var a] | TyVarT a <- universe x ] $
  H.TyApp (tyCon "E") $ hType x
    
hAsst :: Cxt -> Asst
hAsst (Cxt a bs) = hClassA a bs

hTyVar :: Var -> H.Type
hTyVar = tyVar . ppShow

hClassA :: String -> [Var] -> Asst
hClassA a bs = ClassA (qname a) (map hTyVar bs)

unVar :: Var -> Lident
unVar (P.Var a) = a

hTagged :: TypeD -> Maybe Decl
hTagged (TypeD a bs c) = case c of
  TyTagged xs -> f [ d | ConC d _ <- xs ]
  TyEnum xs -> f [ d | EnumC d <- xs ]
  _ -> Nothing
  where
    f ds = Just $
      H.InstDecl nl [] (H.qname "Tagged") [hType $ typeTy a bs]
      [InsDecl $ bind "tags" [PWildCard] $ H.List $ map H.strE ds ]
    
hTyped :: TypeD -> Maybe Decl
hTyped (TypeD a bs0 c) = case c of
  TySyn{} -> Nothing
  TyUnit{} -> pre
    [ ty $ var "tyVoid"
    ]
  TyNewtype _ t -> pre
    [ ty $ apps (var "ty") [e]
    , tydecls $ apps (var "tydecls_") [e]
    ]
    where e = hExpTypeSig (var "unused") (hNoAssts $ hType t)
  TyRecord xs -> pre
    [ ty $ apps (con "Type") [ H.strE a, H.List [ apps (var "ty") [u]
                                                | u <- us ] ]
    , tydecls $ apps (var "recordTyDecls")
      [ H.List
        [ apps (var "tydecls")
          [hExpTypeSig (var "unused") (hNoAssts $ hType z)]
        | FieldT _ z <- xs ]
      , H.List [ tuple
                 [ H.strE y
                 , apps (var "ty")
                   [hExpTypeSig (var "unused") (hNoAssts $ hType z)] ]
               | FieldT y z <- xs ]
      ]
    ]
  TyTagged xs -> pre
    [ ty $ apps (var "Type") [ H.strE a
                             , H.List [ apps (var "ty") [u] | u <- us ] ]
    , tydecls $ apps (var "taggedTyDecls")
      [ H.List
        [ apps (var "tydecls")
          [hExpTypeSig (var "unused") (hNoAssts $ hType z)]
        | ConC _ z <- xs ]
      , H.List [ tuple 
                 [H.strE y
                 , apps (var "ty")
                   [hExpTypeSig (var "unused") (hNoAssts $ hType z)] ]
               | ConC y z <- xs ]
      ]
    ]
  TyEnum xs -> pre
    [ ty $ apps (var "tyPrim") [ H.strE a ]
    , tydecls $ apps (var "enumTyDecls")
      [H.List [ H.strE y | EnumC y <- xs ]]
    ]
  where
  bs = bs0 `intersect` [ P.Var b | TyVarT b <- universeBi c]
  us = map (hExpTypeSig (var "unused") . hTyVar) bs
  assts =
    nub $ map (\b -> hClassA "Typed" [b]) bs0 ++
    [hAsst cxt | cxt@Cxt{} <- universeBi c]
  pre =
    Just . H.InstDecl nl assts (H.qname "Typed") [hType $ typeTy a bs0]
  ty v = InsDecl $ bind "ty" [PWildCard] v
  tydecls v = InsDecl $ bind "tydecls" [] v

hExport :: ExportD -> ExportSpec
hExport x = case x of
  TypeEx a -> EThingAll (qname a)
  VarEx a -> EVar (qname a)

hImport_ :: String -> ImportDecl
hImport_ = hImport . ImportD

hImport :: ImportD -> ImportDecl
hImport (ImportD a) = ImportDecl
  { importLoc = nl
  , importModule = ModuleName a
  , importQualified = False
  , importSrc = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing
  }

hVarD :: VarD -> Decl
hVarD (VarD a b c) = case b of
  Macro -> hNameBind a (hExp True c)
  Define -> hNameBind a $ term "defE" [hString a, hExp False c]
                      
hNameBind :: String -> H.Exp -> Decl
hNameBind a = nameBind nl (Ident $ lowercase $ unqual_name a)

hChar :: Char -> H.Exp
hChar = Lit . Char

hLit :: Lit -> H.Exp
hLit x = case x of
  CharL a -> term "charE" [hChar a]
  StringL a -> term "stringE" [hString a]
  NmbrL a -> term "nmbrE" [hString a]
  
term :: String -> [H.Exp] -> H.Exp
term a bs = apps (var a) bs

hExpTypeSig :: H.Exp -> H.Type -> H.Exp
hExpTypeSig a b = Paren (ExpTypeSig nl a b)

hLambda :: Pat -> H.Exp -> H.Exp
hLambda p e = Lambda nl [p] e

hTyTuple :: [H.Type] -> H.Type
hTyTuple = TyTuple Boxed

hLet :: String -> H.Exp -> H.Exp -> H.Exp
hLet a b c = Let (BDecls [hNameBind a b]) c

hExp :: Bool -> P.Exp -> H.Exp
hExp r x = case x of
  SwitchE a mb cs -> case mb of
    DefaultSome b -> term "switchE" [y, hExp r b, z]
    DefaultNone -> term "switchE_" [y, z]
    where
      y = hExp r a
      z = List [tuple [hExp r m, hExp r n] | SwitchAlt m n <- cs]
  LetE a b c d -> case b of
    Macro -> hLet a (hExp True c) (hExp r d)
    Define -> term "letE" [ hNameStr r a
                          , hExp r c
                          , hLambda (H.pVar a) (hExp r d) ]
  LamE a b -> term "lamE" [hNameStr r a, hLambda (H.pVar a) (hExp r b)]
  AppE a b -> term "appE" [hExp r a, hExp r b]
  VarE a -> var a
  LitE a -> hLit a
  AscribeE a b -> hExpTypeSig (hExp r a) (eType b)

hNameStr :: Bool -> [Char] -> H.Exp
hNameStr inMacro a = hString s
  where s = if inMacro then "~" ++ a else a -- this allows for more readable code generation (procedure variable names are prefered over macro variable names)

hAssts :: H.Type -> [Asst]
hAssts x = nub $ case x of
  TyForall _ b c -> b ++ hAssts c
  H.TyFun a b -> hAssts a ++ hAssts b
  TyTuple _ bs -> concatMap hAssts bs
  TyList a -> hAssts a
  TyApp a b -> hAssts a ++ hAssts b
  TyVar{} -> []
  TyCon{} -> []
  TyParen a -> hAssts a
  TyInfix a _ c -> hAssts a ++ hAssts c
  TyKind a _ -> hAssts a

hNoAssts :: H.Type -> H.Type
hNoAssts x = case x of
  TyForall _ _ c -> hNoAssts c
  H.TyFun a b -> H.TyFun (hNoAssts a) (hNoAssts b)
  TyTuple a bs -> TyTuple a $ map hNoAssts bs
  TyList a -> TyList (hNoAssts a)
  TyApp a b -> TyApp (hNoAssts a) (hNoAssts b)
  TyVar{} -> x
  TyCon{} -> x
  TyParen a -> TyParen $ hNoAssts a
  TyInfix a b c -> TyInfix (hNoAssts a) b (hNoAssts c)
  TyKind a b -> TyKind (hNoAssts a) b

hTyForall :: [Asst] -> H.Type -> H.Type
hTyForall x = hFixType . TyForall Nothing x

hFixType :: H.Type -> H.Type
hFixType x = case hAssts x of
  [] -> x
  cxt -> TyForall Nothing (nub cxt) $ hNoAssts x
  
hType :: P.Type -> H.Type
hType x = hFixType $ case x of
  TyCxt ys z -> hTyForall (map hAsst ys) (hType z)
  P.TyFun a b -> H.TyFun (hType a) (hType b)
  TyVoid -> hTyTuple []
  TyConstr a bs -> tyApps (tyCon a) $ map hType bs
  TyVarT a -> tyVar a
