{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The pec embedded compiler
-- Copyright 2011, Brett Letner

module Main (main) where

import Control.Monad.State
import Data.Char
import Data.Either
import Data.List
import GHC.IO.Exception
import Language.Pec.Abs
import Language.Pec.ErrM
import Language.Pec.Layout
import Language.Pec.Par
import Language.Pec.Print
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Process
import qualified Language.Haskell as H

data Args = Args
  { files :: [FilePath]
  , keep_tmp_files :: Bool
  , rebuild_all :: Bool
  } deriving (Show, Data, Typeable)

argsDesc :: Args
argsDesc = Args
  { files = def &= args
  , keep_tmp_files = def &= help "Keep temporary files"
  , rebuild_all = def &= help "Rebuild all files"
  } &= summary summry &= program prog

summry :: String
summry = prog ++ " v" ++ vers ++ ", " ++ copyright

prog :: String
prog = "pec"

copyright :: String
copyright = "(C) Brett Letner 2011"

vers :: String
vers = "0.1"

initSt :: Args -> St
initSt x = St
  { visited = []
  , modified = False
  , rebuild = rebuild_all x
  , not_visited = map (\fn -> dropExtension fn ++ "_") $ files x
  , strings = []
  , counts = []
  , keep_tmps = keep_tmp_files x
--   , mains = []
  }

type M a = StateT St IO a

data St = St
  { not_visited :: [FilePath]
  , modified :: Bool
  , rebuild :: Bool
  , visited :: [FilePath]
  , strings :: [String]
  , counts :: [Integer]
  , keep_tmps :: Bool
--   , mains :: [FilePath]
  }

main :: IO ()
main = do
  args <- cmdArgs argsDesc
  let fns = files args
  case fns of
    [] -> putStrLn summry
    (fn:_) -> do
      let mnFn = new_ext "_main.hs" fn
      let llFn = new_ext ".ll" fn
      st <- execStateT pec_all $ initSt args
      when (modified st || rebuild st) $ writeFile mnFn $ H.prettyPrint $
        hMainModule (dropExtension fn) (visited st) (strings st)
      let mnExe = new_ext ".exe" mnFn
      my_system $ unwords ["ghc --make -o", mnExe, mnFn] -- fixme:check exit code
      pwd <- getCurrentDirectory
      my_system $ pwd ++ "/" ++ mnExe
      let llFns = ["istrings_" ++ new_ext ".ll" fn] ++ (map (new_ext ".ll" . init) $ visited st)
      let llBc = new_ext ".bc" llFn
      my_system $ "cat " ++ unwords llFns ++ " | llvm-as > " ++ llBc
      let llExe = new_ext ".exe" llFn
      my_system $ unwords ["llvm-ld -disable-opt -o", llExe, llBc] -- array bug in llvm
      when (not $ keep_tmps st) $ my_system $ unwords $
        [ "rm *.hi *.o", "*_.hs" -- fixme: clean up Cnts files
        , mnExe, mnFn, llBc
        ] ++ llFns
      return ()

pec_all :: M ()
pec_all = do
  st <- get
  case not_visited st of
    [] -> return ()
    (n:ns) -> do
      put $ st{ not_visited = ns }
      when (n `notElem` visited st) $ do
        ns <- pec_file n
        modify $ \st -> st{ visited = n : visited st
                          , not_visited = ns ++ not_visited st
                          }
      pec_all

pec_file :: FilePath -> M [FilePath]
pec_file n = do
  let fn = init n ++ ".pec"
  let hsFn = new_ext "_.hs" fn
  ss <- lift $ liftM lines $ readFile fn
  let (xs,ys) = partition (\s -> not (null s) && head s == '>') ss
  case pModule $ resolveLayout True $ myLexer $ unlines ys of
    Bad e -> error e
    Ok m -> do
      b <- isOutOfDate fn hsFn
      h <- hModule m
      when b $ do
        modify $ \st -> st{ modified = True }
        lift $ writeFile hsFn $ unlines $
          imports_hack (lines $ H.prettyPrint h) (map tail xs)
      return $ imports m

isOutOfDate :: FilePath -> FilePath -> M Bool
isOutOfDate inFn outFn = do
  b <- gets rebuild
  if b
    then return True
    else do
      r <- lift $ doesFileExist outFn
      if r
        then liftM2 (>) (lift $ getModificationTime inFn) (lift $ getModificationTime outFn)
        else return True

imports_hack :: [String] -> [String] -> [String]
imports_hack xs ys
  | null es = bs ++ cs ++ ds
  | otherwise = bs ++ ds ++ fs ++ cs ++ gs
  where
  (bs,cs) = break (has_prefix "import ") xs
  (ds,es) = break (has_prefix "import ") ys
  (fs,gs) = span (has_prefix "import ") es

doList :: [H.Exp] -> H.Exp
doList xs =
  H.Do $ map H.Qualifier $ xs ++ [H.apps (H.var "return") [H.Tuple []]]

importDecl :: Bool -> String -> H.ImportDecl
importDecl q n = H.ImportDecl
  { H.importModule = H.ModuleName n
  , H.importQualified = q
  , H.importLoc = nl
  , H.importSrc = False
  , H.importPkg = Nothing
  , H.importAs = Nothing
  , H.importSpecs = Nothing
  }

typeSig :: String -> H.Type -> H.Decl
typeSig s = H.TypeSig nl [H.name s]

gen :: Modid -> [TopDecl] -> [H.Decl]
gen n xs =
  [ typeSig "gen" $ H.TyApp (H.tyCon "M") (H.TyTuple H.Boxed [])
  , H.bind "gen" [] $ H.apps (H.var "gen_file") [H.strE $ printTree n, doList $ concatMap f xs ]
  ]
  where
  f x = case x of
    ExternD _ a _ -> [H.apps (H.var "declare") [hvar a "d"]]
    ProcD a _ _ -> [H.apps (H.var "define") [hvar a "d"]]
    _ -> []

hmodule :: String -> [String] -> Maybe [H.ExportSpec] -> [H.ImportDecl] -> [H.Decl] -> H.Module
hmodule n ps = H.Module nl (H.ModuleName n)
  [H.LanguagePragma nl [H.name p] | p <- ps ] Nothing

hMainModule :: FilePath -> [String] -> [String] -> H.Module
hMainModule fn ns ss = hmodule "Main" [] Nothing imps [mainDecl]
  where
  imps = importDecl False "Pec.Base" : map (importDecl True) ns
  mainDecl = H.bind "main" [] $ H.apps (H.var "gen_files")
    [ H.strE fn
    , doList [H.qvar (H.ModuleName n) (H.name "gen") | n <- ns ]
    , H.List $ map H.strE $ nub $ sort ss
    ]

filterCnts :: (Ord a, Num a) => [a] -> [a]
filterCnts xs = filter (`notElem` [256, 4294967296]) $ nub $ sort xs

hCountModule :: Integer -> M ()
hCountModule i = do
  q <- gets rebuild
  r <- lift $ doesFileExist fn
  when (q || not r) $ do
    modify $ \st -> st{ modified = True }
    lift $ writeFile fn $ H.prettyPrint $ hmodule n [] Nothing
      [importDecl False "Pec.Base"]
      [ hdatadecl n [] [H.QualConDecl nl [] [] (H.ConDecl (H.name n) [])]
      , hinstdecl [] "Count" [H.tyCon n]
        [H.InsDecl $ H.simpleFun nl (H.name "countof") (H.name "_") (H.intE i)]
      , hinstdecl [] "Typed" [H.tyCon n]
        [H.InsDecl $ H.simpleFun nl (H.name "typeof") (H.name "_") (H.con "TyUnit")]
      ]
  where
  n = "Cnt" ++ show i
  fn = n ++ ".hs"

hinstdecl :: H.Context -> String -> [H.Type] -> [H.InstDecl] -> H.Decl
hinstdecl qs n = H.InstDecl nl (nub $ sort qs) (H.qname n)

hModule :: Module -> M H.Module
hModule (Module a b cs) = do
  (xs,ys) <- liftM partitionEithers $ mapM (hTopDecl n) cs
  cnts <- liftM filterCnts $ gets counts
  modify $ \st -> st{ counts = [] }
  mapM_ hCountModule cnts
  return $ hmodule n
    [ "FlexibleInstances", "MultiParamTypeClasses", "ScopedTypeVariables"
    , "TypeSynonymInstances" ]
    (hExpDecl a cs b)
    (importDecl False "Pec.Base" : [ importDecl False $ "Cnt" ++ show cnt | cnt <- cnts ] ++ xs)
    (gen a cs ++ concat ys)
  where n = userc a

hExpDecl :: Modid -> [TopDecl] -> ExportDecl -> Maybe [H.ExportSpec]
hExpDecl n xs a = case a of
  ExpAllD -> Nothing
  ExpListD bs -> Just $ (H.EVar $ H.Qual (H.ModuleName $ userc n) (H.name "gen")) : concatMap (hExport xs) bs

hevar :: String -> H.ExportSpec
hevar = H.EVar . H.qname

hExport :: [TopDecl] -> Export -> [H.ExportSpec]
hExport xs ex = case ex of
  VarEx a -> [hevar $ user a]
  TypeEx a b -> H.EThingWith (H.qname $ userc a) [] : map hevar zs
    where
    zs = case (lookupType xs a, b) of
      (_, Neither) -> []
      (TyTagged ys, Decon) -> concat $ map (tail . nPat) ys
      (TyTagged ys, Both) -> concatMap nPat ys
      (TyRecord ys, Both) -> map hgetfld ys
      _ -> error "export spec doesn't match type decl"

lookupType :: Print a => [TopDecl] -> a -> TyDecl
lookupType xs a = case [ b | TypeD a1 _ b <- xs, printTree a == printTree a1 ] of
  [] -> error $ "unknown type decl:" ++ printTree a
  (b:_) -> b

hTopDecl :: String -> TopDecl -> M (Either H.ImportDecl [H.Decl])
hTopDecl qn x = case x of
  VarD a b -> do
    e <- hExp b
    return $ Right [ nameBind (user a) e ]
  ProcD a bs c -> do
    e0 <- hExp c
    e1 <- foldM hArg e0 $ reverse bs
    return $ Right
      [ nameBind n $ H.App (H.var "call") (H.var nd)
      , nameBind nd $ H.apps (H.var "proc")
          [ H.strE $ init $ if n == "main_" then n else qn ++ "." ++ n
          , e1
          ]
      ]
    where
    n = user a
    nd = n ++ "d"
  ImportD a b -> case b of
    AsAS{} -> error "todo:'import as' not implemented"
    EmptyAS -> return $ Left $ importDecl False (userc a)
  ExternD a b c -> do
    t <- htype "Decl" c
    return $ Right
      [ nameBind (user b) $ H.App (H.var "call") (H.var nd)
      , nameBind nd $ ascribe (H.App (H.var "extern") (H.strE s)) t
      ]
    where
    s = case a of
      NoneNm -> printTree b
      SomeNm s -> s
    nd = user b ++ "d"
  AscribeD a b -> do
    t <- htype "Term" b
    return $ Right [typeSig (user a) t]
  TypeD a bs0 c -> do
    let bs = map uTyVar bs0
    let t0 = dtype_ty a bs
    case c of
      TySyn d -> do
        t <- pType d
        return $ Right
          [H.TypeDecl nl (H.name $ userc a) (map htvbind bs) t]
      TyRecord fs0 -> do
        let fs = sortBy cmpFieldT fs0
        Right ds <- hTopDecl qn $ TypeD a bs0 $ TyTagged [ConC a [ t | FieldT _ t <- fs ]]
        xs <- mapM (hfieldt t0 bs $ length fs) $ zip fs [0 .. ]
        return $ Right $ concat xs ++ ds
      TyTagged xs0 -> do
        let xs = sortBy (\(ConC a _) (ConC b _) -> compare a b) xs0
        let cnt = genericLength xs
        add_count cnt
        let dd = hdatadecl (userc a) (map htvbind bs) []
        cts <- sequence [ htytuple ts | ConC _ ts <- xs ]
        let rs = concatMap (fconc a bs cnt (isTyEnum xs)) $ zip (zip xs [0 .. ]) cts
        let typed_id e = hinstdecl (pre bs) "Typed" [t0] [ H.InsDecl $ nameBind "typeof" $ H.lamE nl [H.PParen $ H.PatTypeSig nl (H.PWildCard) t0 ] e ]
        return $ Right $ case () of
          () | isTyUnit xs ->
                 let
                   [ConC m _] = xs
                   n = user m
                 in
                 [ dd
                 , typed_id $ H.con "TyUnit"
                 , typeSig n $ tpred bs $ tterm t0
                 , nameBind n $ H.App (H.var "cast") (H.var "unit")
                 , hinstdecl (hassts "EQ" bs) "EQ" [t0]
                     [ H.InsDecl $ nameBind "eq" (H.var "eq_unit")
                     , H.InsDecl $ nameBind "ne" (H.var "ne_unit") ]
                 ]
             | isTyNewtype xs ->
                 let
                   [ct] = cts
                   [ConC m _] = xs
                   n = user m
                 in
                 [ dd
                 , typed_id $ unused_ty ct
                 , hinstdecl (hassts "Typed" bs) "Newtype" [t0, ct] []
                 -- , hinstdecl (hassts "EQ" bs) "EQ" [t0]
                 --     [ H.InsDecl $ nameBind "eq" (H.App (H.var "unwrap2") (H.var "eq"))
                 --     , H.InsDecl $ nameBind "ne" (H.App (H.var "unwrap2") (H.var "ne")) ]
                 , typeSig n $ tpred bs $ tterm $ H.TyFun ct t0
                 , nameBind n $ H.var "wrap"
                 ]
             | isTyEnum xs ->
                 [ dd
                 , typed_id $ H.App (H.con "TyEnum") (H.intE cnt)
                 , hinstdecl (pre bs) "Tagged" [ t0, tcnt cnt ]
                     [H.InsDecl $ nameBind "tagof" (H.var "cast")]
                 , hinstdecl [] "EQ" [t0] []
                 ] ++ rs
             | otherwise ->
                 [ dd
                 , typed_id $ H.App (H.con "TySum") (H.List [unused_ty ct | ct <- cts ])
                 , hinstdecl (pre bs) "Tagged" [ tptr t0, tcnt cnt ]
                     [H.InsDecl $ nameBind "tagof" (H.var "tagofp")]
                 ] ++ rs

hfieldt :: H.Type -> [TyVar] -> Int -> (FieldT, Int) -> M [H.Decl]
hfieldt t0 tvs cnt (fld@(FieldT _ t), i) = do
  ht <- pType t
  return
    [ typeSig n $ tpred tvs $ H.TyFun (tterm $ tptr t0) (tterm $ tptr ht)
    , nameBind n $ foldr (\a b -> H.InfixApp (H.var $ a ++ "_get") (H.qvop ".") b) (H.App (H.var "app") (H.var "unwrap_ptr_")) ss
    ]
  where
  n = hgetfld fld
  ss = case cnt of
    1 -> []
    _ | i == cnt - 1 -> replicate i "snd"
    _ -> ["fst"] ++ replicate i "snd"

hgetfld :: FieldT -> String
hgetfld (FieldT f _) = user f ++ "get"

cmpFieldT :: FieldT -> FieldT -> Ordering
cmpFieldT (FieldT a _) (FieldT b _) = compare a b

cmpFieldD :: FieldD -> FieldD -> Ordering
cmpFieldD (FieldD a _) (FieldD b _) = compare a b

unused_ty :: H.Type -> H.Exp
unused_ty t = H.App (H.var "typeof") $ ascribe (H.var "unused") t

isTyUnit :: [ConC] -> Bool
isTyUnit x = isTyNewtype x && isTyEnum x

isTyNewtype :: [a] -> Bool
isTyNewtype [_] = True
isTyNewtype _ = False

isTyEnum :: [ConC] -> Bool
isTyEnum xs = and [ null ys | ConC _ ys <- xs ]

hdatadecl :: String -> [H.TyVarBind] -> [H.QualConDecl] -> H.Decl
hdatadecl n vs ds = H.DataDecl nl H.DataType [] (H.name n) vs ds []

htycon :: Print a => a -> H.Type
htycon = H.tyCon . userc

htyvar :: TyVar -> H.Type
htyvar = H.tyVar . printTree . uncnttv

uncnttv :: TyVar -> Lident
uncnttv x = case x of
  CntTV a -> a
  VarTV a -> a

uTyVar :: TyVar -> TyVar
uTyVar x = case x of
  CntTV a -> CntTV $ Lident $ user a
  VarTV a -> VarTV $ Lident $ user a

pre :: [TyVar] -> [H.Asst]
pre = map hasst

hasst :: TyVar -> H.Asst
hasst x = case x of
  CntTV{} -> H.ClassA (H.qname "Count") [htyvar x]
  VarTV{} -> H.ClassA (H.qname "Typed") [htyvar x]

hassts :: String -> [TyVar] -> [H.Asst]
hassts s tvs = [H.ClassA (H.qname s) [htyvar tv] | tv@VarTV{} <- tvs ] ++ map hasst tvs

dtype_ty :: Print a => a -> [TyVar] -> H.Type
dtype_ty a tvs = H.tyApps (htycon a) $ map htyvar tvs

tptr :: H.Type -> H.Type
tptr = H.TyApp (H.tyCon "Ptr")

tcnt :: Show a => a -> H.Type
tcnt cnt = H.tyCon $ "Cnt" ++ show cnt

tpred :: [TyVar] -> H.Type -> H.Type
tpred xs = H.TyForall Nothing (pre xs)

tterm :: H.Type -> H.Type
tterm = H.TyApp (H.tyCon "Term")

fconc :: (Print a, Show a1) =>
                        a
                        -> [TyVar]
                        -> a1
                        -> Bool
                        -> ((ConC, Integer), H.Type)
                        -> [H.Decl]
fconc a tvs cnt is_enm ((conc@(ConC _ ts), i),t1) =
    [ typeSig nTag $ tpred tvs $ tterm $ H.tyApps (H.tyCon "Tag") [mtptr t0, tcnt cnt]
    , nameBind nTag $ H.App (H.var "tag") (H.intE i)
    , typeSig n $ tpred tvs $ tterm $ if null ts then t0 else H.TyFun t1 t0
    , nameBind n $ H.App (H.var $ if is_enm then "cast" else if null ts then "constr0" else "constr") (H.var nTag)
    , typeSig nAlt $ tpred ((VarTV $ Lident "a") : tvs) $ H.TyFun (H.TyFun (tterm (mtptr t1)) (tterm ta)) $ H.TyFun (tterm (mtptr t0)) (tterm ta)
    , nameBind nAlt $ H.var $ if is_enm then "alt0" else "alt"
    ]
  where
  t0 = dtype_ty a tvs
  [n, nTag, nAlt] = nPat conc
  ta = H.tyVar "a"
  mtptr v = if is_enm then v else tptr v

nPat :: ConC -> [String]
nPat (ConC c _) = [n, n ++ "tag", n ++ "alt"]
  where
  n = user c

hArg :: H.Exp -> Exp -> M H.Exp
hArg e x = case x of
  VarE{} -> return $ H.apps (H.var "arg")
    [H.strE (printTree x), lambda [hpvar x] e]
  TupleE [] -> return $ H.App (H.var "unitarg") e
  TupleE [a] -> hArg e a
  _ -> do
    let v = varE "v"
    s <- hStmt $ LetS x v
    hArg (blocke [s, [H.Qualifier $ eval e]]) v

ascribe :: H.Exp -> H.Type -> H.Exp
ascribe a b = H.Paren $ H.ExpTypeSig nl a b

nameBind :: String -> H.Exp -> H.Decl
nameBind s = H.nameBind nl (H.name s)

htype :: String -> Type -> M H.Type
htype s = liftM (H.TyApp (H.tyCon s)) . pType

pType :: Type -> M H.Type
pType x = case x of
  TyConstr0{} -> return $ htycon x
  TyFun a b -> liftM2 H.TyFun (pType a) (pType b)
  TyTuple xs -> htytuple xs
  TyConstr a bs -> liftM (H.tyApps (htycon a)) $ mapM pType bs
  TyArray a b -> liftM (H.tyApps (H.tyCon "Array")) $ mapM pType [a,b]
  TyCount a -> add_count i >> return (tcnt i)
    where i = pcount a
  TyVarT a -> return $ H.tyVar $ user a

htytuple :: [Type] -> M H.Type
htytuple xs0 = case xs0 of
  [] -> return $ tytuple []
  [a] -> pType a
  [a,b] -> liftM tytuple $ mapM pType [a,b]
  (x:xs) -> liftM tytuple $ mapM pType [x, TyTuple xs]

htvbind :: TyVar -> H.TyVarBind
htvbind = H.UnkindedVar . H.name . printTree . uncnttv

pcount :: Count -> Integer
pcount = read . tail . printTree

tytuple :: [H.Type] -> H.Type
tytuple = H.TyTuple H.Boxed

lambda :: [H.Pat] -> H.Exp -> H.Exp
lambda = H.Lambda nl

hpvar :: Print a => a -> H.Pat
hpvar = H.pVar . user

hvar :: Print a => a -> String -> H.Exp
hvar a s = H.var $ user a ++ s

hCaseAlt :: CaseAlt -> M H.Exp
hCaseAlt (CaseAlt p x) = do
  e <- hExp x
  case p of
    VarP{} -> return $ H.tuple [ H.var "defaulttag", lambda [hpvar p] e]
    ConP a b -> return $ H.tuple
      [ H.var $ s ++ "tag"
      , H.App (H.var $ s ++ "alt") (lambda [hpvar b] e)
      ]
      where s = user a
    LitP y -> case y of
      EnumL a -> hCaseAlt (CaseAlt (ConP a (var "_")) x)
      IntL a -> f "int" (H.Paren $ H.Lit $ H.Int $ read $ printTree a)
      StringL a -> add_string a >> f "string" (H.Lit  $ H.String a)
      CharL a -> f "char" (H.Lit $ H.Char a)
      FracL{} -> error $ "can't case on fractional:" ++ printTree y
      where
      f s t = return $ H.tuple [ H.App (H.var $ s ++ "tag") t
                               , lambda [H.wildcard] e]
  
eval :: H.Exp -> H.Exp
eval e = H.App (H.var "eval") e

hStmt :: Exp -> M [H.Stmt]
hStmt x = case x of
  LetS a b -> case a of
    VarE{} -> do
      e <- hExp b
      return [H.Generator nl (hpvar a) $ eval e]
    TupleE xs0 -> case xs0 of
      [] -> do
        e <- hExp b
        t <- htype "Term" $ TyTuple []
        return [H.Qualifier $ eval $ ascribe e t]
      [e] -> hStmt $ LetS e b
      [ea,eb] -> liftM concat $ mapM hStmt
        [ LetS e b
        , LetS ea $ AppE (varE "fst") e
        , LetS eb $ AppE (varE "snd") e
        ] where e = varE "pat"
      (x:xs) -> hStmt $ LetS (TupleE [x, TupleE xs]) b
    AscribeE e t -> hStmt $ LetS e $ AscribeE b t
    _ -> error $ "unexpected pattern:" ++ printTree x
  _ -> do
    e <- hExp x
    return [H.Qualifier $ eval e]

varE :: String -> Exp
varE = VarE . var

var :: String -> Var
var = Var . Lident

con :: String -> Con
con = Con . Uident

blocke :: [[H.Stmt]] -> H.Exp
blocke = H.App (H.var "lift") . H.Do . concat

enuml :: String -> Lit
enuml = EnumL . con

penum :: String -> CasePat
penum = LitP . enuml

hExp :: Exp -> M H.Exp
hExp x = case x of
  RecordE xs -> hExp $ BlockE $
    [ LetS p $ varE "alloca'" ] ++
    [ StoreE (FldE p a) b | FieldD a b <- sortBy cmpFieldD xs ] ++
    [ UnOpE Load p ]
    where p = varE "p"
  BlockE xs -> liftM blocke $ mapM hStmt xs
  LetS{} -> error "let stmt must be inside of do block"
  LetE a b c -> hExp $ BlockE [LetS a b, c]
  BranchE bs0 -> case bs0 of
    (BranchAlt (BoolBP f) g : bs) | not $ null bs -> hExp $ CaseE f
      [ CaseAlt (penum "True") g
      , CaseAlt (penum "False") $ BranchE bs
      ]
    [BranchAlt DefaultBP g] -> hExp g
    _ -> error "malformed branch expression"
  CaseE a bs -> do
    e <- hExp a
    cs <- mapM hCaseAlt bs
    return $ H.apps (H.var "switch") [ e, H.List cs ]
  StoreE a b -> do
    ea <- hExp a
    eb <- hExp b
    return $ H.apps (H.var "store") [ea, eb]
  AppE a b -> do
    ea <- hExp a
    eb <- hExp b
    return $ H.apps (H.var "app") [ea, eb]
  BinOpE a b c -> do
    ea <- hExp a
    ec <- hExp c
    return $ H.Paren $ H.InfixApp ea (H.qvop $ eSym $ printTree b) ec
  UnOpE a b ->
    liftM (H.App (H.Paren $ H.var $ eSym $ printTree a)) $ hExp b
  IdxE a b -> do
    ea <- hExp a
    eb <- hExp b
    return $ H.apps (H.var "idx") [ea,eb]
  FldE a b -> liftM (H.App (hvar b "get")) $ hExp a
  ArrayE xs -> do
    e <- hExp $ CountE (Count $ '#' : show (length xs))
    es <- mapM hExp xs
    return $ H.apps (H.var "array") [ e, H.List es ]
  TupleE xs0 -> case xs0 of
    [] -> return $ H.var "unit"
    [a] -> hExp a
    [a,b] -> do
      ea <- hExp a
      eb <- hExp b
      return $ H.apps (H.var "pair") [ea,eb]
    (x:xs) -> hExp $ TupleE [x, TupleE xs]
  AscribeE a b -> liftM2 ascribe (hExp a) (htype "Term" b)
  CountE a -> do
    add_count i
    return $ ascribe (H.var "unused") $
            H.TyApp (H.tyCon "Term") (H.tyCon $ "Cnt" ++ show i)
    where i = pcount a
  VarE a -> return $ hvar a ""
  LitE a -> hLit a

nl :: H.SrcLoc
nl = error "SRCLOC"

add_string :: String -> M ()
add_string s = modify $ \st -> st{ strings = s : strings st }

add_count :: Integer -> M ()
add_count s = modify $ \st -> st{ counts = s : counts st }

eSym :: String -> String
eSym cs = cs ++ "$"

hLit :: Lit -> M H.Exp
hLit x = case x of
  EnumL a -> return $ H.var $ user a
  CharL a -> return $ H.App (H.var "char") $ H.Lit $ H.Char a
  IntL a -> return $ H.App (H.var "int") $ H.Paren $ H.Lit $ H.Int $ read $ printTree a
  StringL a -> do
    add_string a
    return (H.App (H.var "string") $ H.Lit $ H.String a)
  FracL a -> return $ H.App (H.var "frac") $ H.Paren $ H.Lit $ H.Frac $ toRational $ ((read $ printTree a) :: Double)

lowercase :: String -> String
lowercase "" = ""
lowercase (c:cs) = toLower c : cs

has_prefix :: Eq a => [a] -> [a] -> Bool
has_prefix pre s = take (length pre) s == pre

new_ext :: String -> FilePath -> FilePath
new_ext s fn = dropExtension fn ++ s

imports :: Module -> [String]
imports (Module _ _ xs) = [ userc a | ImportD a _ <- xs ]

user :: Print a => a -> String
user = lowercase . userc

userc :: Print a => a -> String
userc a = printTree a ++ "_"

my_system :: String -> IO ()
my_system s = do
  putStrLn $ "system:" ++ s
  ec <- system s
  case ec of
    ExitSuccess -> return ()
    ExitFailure i -> error $ "exiting(" ++ show i ++ ")"
