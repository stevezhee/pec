{-# LANGUAGE DeriveDataTypeable #-}
module Language.Pec.Abs where
import Control.DeepSeq
import Data.Generics
import Grm.Prims
import Grm.Lex
import Text.PrettyPrint.Leijen
myLexemes = ["#","(",")",",","->",".","..","::",";","<-","=","=>","@","Array","[","\\","]","as","branch","case","do","exports","extern","imports","in","instance","let","module","of","switch","type","where","{","|","}"]
grmLexFilePath = lexFilePath myLexemes
grmLexContents = lexContents myLexemes
data Module a
  = Module a (Modid a) (ExportDecls a) (ImportDecls a) (TopDeclList a)
  deriving (Show,Eq,Ord,Data,Typeable)
data ExportDecls a
  = ExpListD a (ExportList a)
  | ExpAllD a
  deriving (Show,Eq,Ord,Data,Typeable)
data ImportDecls a
  = ImpListD a (ImportList a)
  | ImpNoneD a
  deriving (Show,Eq,Ord,Data,Typeable)
data Export a
  = TypeEx a (Con a) (Spec a)
  | VarEx a (Var a)
  deriving (Show,Eq,Ord,Data,Typeable)
data Import a
  = Import a (Modid a) (AsSpec a)
  deriving (Show,Eq,Ord,Data,Typeable)
data AsSpec a
  = AsAS a (Modid a)
  | EmptyAS a
  deriving (Show,Eq,Ord,Data,Typeable)
data Spec a
  = Neither a
  | Decon a
  | Both a
  deriving (Show,Eq,Ord,Data,Typeable)
data TopDecl a
  = ExternD a (ExtNm a) (Var a) (Type a)
  | TypeD a (Con a) (VarList a) (TyDecl a)
  | TypeD0 a (Con a) (VarList a)
  | AscribeD a (Var a) (Type a)
  | VarD a (Var a) (DeclSym a) (Exp a)
  | ProcD a (Var a) (Exp0List a) (DeclSym a) (Exp a)
  | InstD a (Con a) (Type a)
  deriving (Show,Eq,Ord,Data,Typeable)
data DeclSym a
  = Macro a
  | Define a
  deriving (Show,Eq,Ord,Data,Typeable)
data ExtNm a
  = SomeNm a String
  | NoneNm a
  deriving (Show,Eq,Ord,Data,Typeable)
data Exp a
  = BlockE a (Exp5List a)
  | LetS a (Exp4 a) (DeclSym a) (Exp a)
  | LetE a (Exp4 a) (DeclSym a) (Exp a) (Exp a)
  | LamE a (Exp0List a) (Exp a)
  | StoreE a (Exp4 a) (Exp a)
  | CaseE a (Exp a) (CaseAltList a) (DefaultAlt a)
  | SwitchE a (Exp a) (SwitchAltList a) (DefaultAlt a)
  | BranchE a (BranchAltList a) (Exp a)
  | BinOpE a (Exp3 a) Usym (Exp3 a)
  | AppE a (Exp3 a) (Exp2 a)
  | UnOpE a (UnOp a) (Exp1 a)
  | IdxE a (Exp1 a) (Exp a)
  | FldE a (Exp1 a) (Field a)
  | ArrayE a (ExpList a)
  | RecordE a (FieldDList a)
  | TupleE a (ExpList a)
  | AscribeE a (Exp a) (Type a)
  | CountE a (Count a)
  | VarE a (Var a)
  | LitE a (Lit a)
  deriving (Show,Eq,Ord,Data,Typeable)
type Exp5 a = Exp a
type Exp4 a = Exp a
type Exp3 a = Exp a
type Exp2 a = Exp a
type Exp1 a = Exp a
type Exp0 a = Exp a
data UnOp a
  = Load a
  deriving (Show,Eq,Ord,Data,Typeable)
data CaseAlt a
  = CaseAlt a (Con a) (VarList a) (Exp a)
  deriving (Show,Eq,Ord,Data,Typeable)
data SwitchAlt a
  = SwitchAlt a (Lit a) (Exp a)
  deriving (Show,Eq,Ord,Data,Typeable)
data DefaultAlt a
  = DefaultAlt a (Var a) (Exp a)
  | DefaultNone a
  deriving (Show,Eq,Ord,Data,Typeable)
data BranchAlt a
  = BranchAlt a (Exp4 a) (Exp a)
  deriving (Show,Eq,Ord,Data,Typeable)
data Cxt a
  = Cxt a (Con a) (VarList a)
  deriving (Show,Eq,Ord,Data,Typeable)
data Type a
  = TyCxt a (CxtList a) (Type3 a)
  | TyFun a (Type2 a) (Type3 a)
  | TyArray a (Type1 a) (Type1 a)
  | TyConstr a (Con a) (Type1List a)
  | TyTuple a (TypeList a)
  | TyCount a (Count a)
  | TyVarT a (TyVar a)
  | TyConstr0 a (Con a)
  deriving (Show,Eq,Ord,Data,Typeable)
type Type3 a = Type a
type Type2 a = Type a
type Type1 a = Type a
type Type0 a = Type a
data TyDecl a
  = TyRecord a (FieldTList a)
  | TyTagged a (ConCList a)
  | TySyn a (Type a)
  deriving (Show,Eq,Ord,Data,Typeable)
data ConC a
  = ConC a (Con a) (Type0List a)
  deriving (Show,Eq,Ord,Data,Typeable)
data FieldT a
  = FieldT a (Field a) (Type a)
  deriving (Show,Eq,Ord,Data,Typeable)
data Lit a
  = CharL a Char
  | StringL a String
  | NmbrL a Number
  | EnumL a (Con a)
  deriving (Show,Eq,Ord,Data,Typeable)
data FieldD a
  = FieldD a (Field a) (Exp a)
  deriving (Show,Eq,Ord,Data,Typeable)
data Count a
  = Count a Number
  deriving (Show,Eq,Ord,Data,Typeable)
data Var a
  = Var a Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data Con a
  = Con a Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data Modid a
  = Modid a Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data Field a
  = Field a Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data TyVar a
  = VarTV a Lident
  | CntTV a Lident
  deriving (Show,Eq,Ord,Data,Typeable)
type BranchAltList a = [BranchAlt a]
type CaseAltList a = [CaseAlt a]
type SwitchAltList a = [SwitchAlt a]
type TopDeclList a = [TopDecl a]
type ImportList a = [Import a]
type Exp5List a = [Exp5 a]
type ExportList a = [Export a]
type ConCList a = [ConC a]
type Exp0List a = [Exp0 a]
type VarList a = [Var a]
type ExpList a = [Exp a]
type FieldDList a = [FieldD a]
type FieldTList a = [FieldT a]
type Type0List a = [Type0 a]
type Type1List a = [Type1 a]
type TypeList a = [Type3 a]
type CxtList a = [Cxt a]
instance HasMeta Module where
  meta x = case x of
    Module a _ _ _ _ -> a
instance HasMeta ExportDecls where
  meta x = case x of
    ExpListD a _ -> a
    ExpAllD a  -> a
instance HasMeta ImportDecls where
  meta x = case x of
    ImpListD a _ -> a
    ImpNoneD a  -> a
instance HasMeta Export where
  meta x = case x of
    TypeEx a _ _ -> a
    VarEx a _ -> a
instance HasMeta Import where
  meta x = case x of
    Import a _ _ -> a
instance HasMeta AsSpec where
  meta x = case x of
    AsAS a _ -> a
    EmptyAS a  -> a
instance HasMeta Spec where
  meta x = case x of
    Neither a  -> a
    Decon a  -> a
    Both a  -> a
instance HasMeta TopDecl where
  meta x = case x of
    ExternD a _ _ _ -> a
    TypeD a _ _ _ -> a
    TypeD0 a _ _ -> a
    AscribeD a _ _ -> a
    VarD a _ _ _ -> a
    ProcD a _ _ _ _ -> a
    InstD a _ _ -> a
instance HasMeta DeclSym where
  meta x = case x of
    Macro a  -> a
    Define a  -> a
instance HasMeta ExtNm where
  meta x = case x of
    SomeNm a _ -> a
    NoneNm a  -> a
instance HasMeta Exp where
  meta x = case x of
    BlockE a _ -> a
    LetS a _ _ _ -> a
    LetE a _ _ _ _ -> a
    LamE a _ _ -> a
    StoreE a _ _ -> a
    CaseE a _ _ _ -> a
    SwitchE a _ _ _ -> a
    BranchE a _ _ -> a
    BinOpE a _ _ _ -> a
    AppE a _ _ -> a
    UnOpE a _ _ -> a
    IdxE a _ _ -> a
    FldE a _ _ -> a
    ArrayE a _ -> a
    RecordE a _ -> a
    TupleE a _ -> a
    AscribeE a _ _ -> a
    CountE a _ -> a
    VarE a _ -> a
    LitE a _ -> a
instance HasMeta UnOp where
  meta x = case x of
    Load a  -> a
instance HasMeta CaseAlt where
  meta x = case x of
    CaseAlt a _ _ _ -> a
instance HasMeta SwitchAlt where
  meta x = case x of
    SwitchAlt a _ _ -> a
instance HasMeta DefaultAlt where
  meta x = case x of
    DefaultAlt a _ _ -> a
    DefaultNone a  -> a
instance HasMeta BranchAlt where
  meta x = case x of
    BranchAlt a _ _ -> a
instance HasMeta Cxt where
  meta x = case x of
    Cxt a _ _ -> a
instance HasMeta Type where
  meta x = case x of
    TyCxt a _ _ -> a
    TyFun a _ _ -> a
    TyArray a _ _ -> a
    TyConstr a _ _ -> a
    TyTuple a _ -> a
    TyCount a _ -> a
    TyVarT a _ -> a
    TyConstr0 a _ -> a
instance HasMeta TyDecl where
  meta x = case x of
    TyRecord a _ -> a
    TyTagged a _ -> a
    TySyn a _ -> a
instance HasMeta ConC where
  meta x = case x of
    ConC a _ _ -> a
instance HasMeta FieldT where
  meta x = case x of
    FieldT a _ _ -> a
instance HasMeta Lit where
  meta x = case x of
    CharL a _ -> a
    StringL a _ -> a
    NmbrL a _ -> a
    EnumL a _ -> a
instance HasMeta FieldD where
  meta x = case x of
    FieldD a _ _ -> a
instance HasMeta Count where
  meta x = case x of
    Count a _ -> a
instance HasMeta Var where
  meta x = case x of
    Var a _ -> a
instance HasMeta Con where
  meta x = case x of
    Con a _ -> a
instance HasMeta Modid where
  meta x = case x of
    Modid a _ -> a
instance HasMeta Field where
  meta x = case x of
    Field a _ -> a
instance HasMeta TyVar where
  meta x = case x of
    VarTV a _ -> a
    CntTV a _ -> a

















instance Pretty (Module a) where
  pretty = ppModule
ppModule x = case x of
  Module _ v1 v2 v3 v4 -> text "module" <+> ppModid v1 <+> ppExportDecls v2 <+> ppImportDecls v3 <+> text "where" <+> text "{" <+> ppTopDeclList v4 <+> text "}"
instance Pretty (ExportDecls a) where
  pretty = ppExportDecls
ppExportDecls x = case x of
  ExpListD _ v1 -> text "exports" <+> text "{" <+> ppExportList v1 <+> text "}"
  ExpAllD _  -> Text.PrettyPrint.Leijen.empty
instance Pretty (ImportDecls a) where
  pretty = ppImportDecls
ppImportDecls x = case x of
  ImpListD _ v1 -> text "imports" <+> text "{" <+> ppImportList v1 <+> text "}"
  ImpNoneD _  -> Text.PrettyPrint.Leijen.empty
instance Pretty (Export a) where
  pretty = ppExport
ppExport x = case x of
  TypeEx _ v1 v2 -> ppCon v1 <+> ppSpec v2
  VarEx _ v1 -> ppVar v1
instance Pretty (Import a) where
  pretty = ppImport
ppImport x = case x of
  Import _ v1 v2 -> ppModid v1 <+> ppAsSpec v2
instance Pretty (AsSpec a) where
  pretty = ppAsSpec
ppAsSpec x = case x of
  AsAS _ v1 -> text "as" <+> ppModid v1
  EmptyAS _  -> Text.PrettyPrint.Leijen.empty
instance Pretty (Spec a) where
  pretty = ppSpec
ppSpec x = case x of
  Neither _  -> Text.PrettyPrint.Leijen.empty
  Decon _  -> text "(" <+> text "." <+> text ")"
  Both _  -> text "(" <+> text ".." <+> text ")"
instance Pretty (TopDecl a) where
  pretty = ppTopDecl
ppTopDecl x = case x of
  ExternD _ v1 v2 v3 -> text "extern" <+> ppExtNm v1 <+> ppVar v2 <+> text "::" <+> ppType v3
  TypeD _ v1 v2 v3 -> text "type" <+> ppCon v1 <+> ppVarList v2 <+> text "=" <+> ppTyDecl v3
  TypeD0 _ v1 v2 -> text "type" <+> ppCon v1 <+> ppVarList v2
  AscribeD _ v1 v2 -> ppVar v1 <+> text "::" <+> ppType v2
  VarD _ v1 v2 v3 -> ppVar v1 <+> ppDeclSym v2 <+> ppExp v3
  ProcD _ v1 v2 v3 v4 -> ppVar v1 <+> ppExp0List v2 <+> ppDeclSym v3 <+> ppExp v4
  InstD _ v1 v2 -> text "instance" <+> ppCon v1 <+> ppType v2
instance Pretty (DeclSym a) where
  pretty = ppDeclSym
ppDeclSym x = case x of
  Macro _  -> text "=>"
  Define _  -> text "="
instance Pretty (ExtNm a) where
  pretty = ppExtNm
ppExtNm x = case x of
  SomeNm _ v1 -> ppString v1
  NoneNm _  -> Text.PrettyPrint.Leijen.empty
instance Pretty (Exp a) where
  pretty = ppExp
ppExp x = case x of
  BlockE _ v1 -> text "do" <+> text "{" <+> ppExp5List v1 <+> text "}"
  LetS _ v1 v2 v3 -> ppExp4 v1 <+> ppDeclSym v2 <+> ppExp v3
  LetE _ v1 v2 v3 v4 -> text "let" <+> ppExp4 v1 <+> ppDeclSym v2 <+> ppExp v3 <+> text "in" <+> ppExp v4
  LamE _ v1 v2 -> text "\\" <+> ppExp0List v1 <+> text "->" <+> ppExp v2
  StoreE _ v1 v2 -> ppExp4 v1 <+> text "<-" <+> ppExp v2
  CaseE _ v1 v2 v3 -> text "case" <+> ppExp v1 <+> text "of" <+> text "{" <+> ppCaseAltList v2 <+> ppDefaultAlt v3 <+> text "}"
  SwitchE _ v1 v2 v3 -> text "switch" <+> ppExp v1 <+> text "of" <+> text "{" <+> ppSwitchAltList v2 <+> ppDefaultAlt v3 <+> text "}"
  BranchE _ v1 v2 -> text "branch" <+> text "{" <+> ppBranchAltList v1 <+> text "|" <+> ppExp v2 <+> text ";" <+> text "}"
  BinOpE _ v1 v2 v3 -> ppExp3 v1 <+> ppUsym v2 <+> ppExp3 v3
  AppE _ v1 v2 -> ppExp3 v1 <+> ppExp2 v2
  UnOpE _ v1 v2 -> ppUnOp v1 <+> ppExp1 v2
  IdxE _ v1 v2 -> ppExp1 v1 <+> text "[" <+> ppExp v2 <+> text "]"
  FldE _ v1 v2 -> ppExp1 v1 <+> text "." <+> ppField v2
  ArrayE _ v1 -> text "Array" <+> text "[" <+> ppExpList v1 <+> text "]"
  RecordE _ v1 -> text "{" <+> ppFieldDList v1 <+> text "}"
  TupleE _ v1 -> text "(" <+> ppExpList v1 <+> text ")"
  AscribeE _ v1 v2 -> text "(" <+> ppExp v1 <+> text "::" <+> ppType v2 <+> text ")"
  CountE _ v1 -> ppCount v1
  VarE _ v1 -> ppVar v1
  LitE _ v1 -> ppLit v1
ppExp5 = ppExp
ppExp4 = ppExp
ppExp3 = ppExp
ppExp2 = ppExp
ppExp1 = ppExp
ppExp0 = ppExp
instance Pretty (UnOp a) where
  pretty = ppUnOp
ppUnOp x = case x of
  Load _  -> text "@"
instance Pretty (CaseAlt a) where
  pretty = ppCaseAlt
ppCaseAlt x = case x of
  CaseAlt _ v1 v2 v3 -> ppCon v1 <+> ppVarList v2 <+> text "->" <+> ppExp v3
instance Pretty (SwitchAlt a) where
  pretty = ppSwitchAlt
ppSwitchAlt x = case x of
  SwitchAlt _ v1 v2 -> ppLit v1 <+> text "->" <+> ppExp v2
instance Pretty (DefaultAlt a) where
  pretty = ppDefaultAlt
ppDefaultAlt x = case x of
  DefaultAlt _ v1 v2 -> ppVar v1 <+> text "->" <+> ppExp v2 <+> text ";"
  DefaultNone _  -> Text.PrettyPrint.Leijen.empty
instance Pretty (BranchAlt a) where
  pretty = ppBranchAlt
ppBranchAlt x = case x of
  BranchAlt _ v1 v2 -> ppExp4 v1 <+> text "->" <+> ppExp v2
instance Pretty (Cxt a) where
  pretty = ppCxt
ppCxt x = case x of
  Cxt _ v1 v2 -> ppCon v1 <+> ppVarList v2
instance Pretty (Type a) where
  pretty = ppType
ppType x = case x of
  TyCxt _ v1 v2 -> text "{" <+> ppCxtList v1 <+> text "}" <+> text "=>" <+> ppType3 v2
  TyFun _ v1 v2 -> ppType2 v1 <+> text "->" <+> ppType3 v2
  TyArray _ v1 v2 -> text "Array" <+> ppType1 v1 <+> ppType1 v2
  TyConstr _ v1 v2 -> ppCon v1 <+> ppType1List v2
  TyTuple _ v1 -> text "(" <+> ppTypeList v1 <+> text ")"
  TyCount _ v1 -> ppCount v1
  TyVarT _ v1 -> ppTyVar v1
  TyConstr0 _ v1 -> ppCon v1
ppType3 = ppType
ppType2 = ppType
ppType1 = ppType
ppType0 = ppType
instance Pretty (TyDecl a) where
  pretty = ppTyDecl
ppTyDecl x = case x of
  TyRecord _ v1 -> text "{" <+> ppFieldTList v1 <+> text "}"
  TyTagged _ v1 -> text "|" <+> ppConCList v1
  TySyn _ v1 -> ppType v1
instance Pretty (ConC a) where
  pretty = ppConC
ppConC x = case x of
  ConC _ v1 v2 -> ppCon v1 <+> ppType0List v2
instance Pretty (FieldT a) where
  pretty = ppFieldT
ppFieldT x = case x of
  FieldT _ v1 v2 -> ppField v1 <+> text "::" <+> ppType v2
instance Pretty (Lit a) where
  pretty = ppLit
ppLit x = case x of
  CharL _ v1 -> ppChar v1
  StringL _ v1 -> ppString v1
  NmbrL _ v1 -> ppNumber v1
  EnumL _ v1 -> ppCon v1
instance Pretty (FieldD a) where
  pretty = ppFieldD
ppFieldD x = case x of
  FieldD _ v1 v2 -> ppField v1 <+> text "=" <+> ppExp v2
instance Pretty (Count a) where
  pretty = ppCount
ppCount x = case x of
  Count _ v1 -> text "#" <> ppNumber v1
instance Pretty (Var a) where
  pretty = ppVar
ppVar x = case x of
  Var _ v1 -> ppLident v1
instance Pretty (Con a) where
  pretty = ppCon
ppCon x = case x of
  Con _ v1 -> ppUident v1
instance Pretty (Modid a) where
  pretty = ppModid
ppModid x = case x of
  Modid _ v1 -> ppUident v1
instance Pretty (Field a) where
  pretty = ppField
ppField x = case x of
  Field _ v1 -> ppLident v1
instance Pretty (TyVar a) where
  pretty = ppTyVar
ppTyVar x = case x of
  VarTV _ v1 -> ppLident v1
  CntTV _ v1 -> text "#" <+> ppLident v1
ppBranchAltList = ppList ppBranchAlt Terminator ";" Vert
ppCaseAltList = ppList ppCaseAlt Terminator ";" Vert
ppSwitchAltList = ppList ppSwitchAlt Terminator ";" Vert
ppTopDeclList = ppList ppTopDecl Terminator ";" Vert
ppImportList = ppList ppImport Terminator ";" Vert
ppExp5List = ppList ppExp5 Terminator ";" Vert
ppExportList = ppList ppExport Terminator ";" Vert
ppConCList = ppList ppConC Separator "|" Horiz
ppExp0List = ppList ppExp0 Separator "" Horiz
ppVarList = ppList ppVar Separator "" Horiz
ppExpList = ppList ppExp Separator "," Horiz
ppFieldDList = ppList ppFieldD Separator "," Horiz
ppFieldTList = ppList ppFieldT Separator "," Horiz
ppType0List = ppList ppType0 Separator "" Horiz
ppType1List = ppList ppType1 Separator "" Horiz
ppTypeList = ppList ppType3 Separator "," Horiz
ppCxtList = ppList ppCxt Separator "," Horiz