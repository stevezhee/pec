{-# LANGUAGE DeriveDataTypeable #-}
module Language.Pds.Abs where
import Control.DeepSeq
import Data.Generics
import Grm.Prims
import Grm.Lex
import Text.PrettyPrint.Leijen
myLexemes = ["(",")",",","->","::",";","=","=>","VoidT","\\","enum","export","import","in","instance","let","module","newtype","of","record","switch","synonym","tagged","type","unit","{","|","}"]
grmLexFilePath = lexFilePath myLexemes
grmLexContents = lexContents myLexemes
data Module 
  = Module  Uident ExportDList ImportDList TypeDList InstDList VarDList
  deriving (Show,Eq,Ord,Data,Typeable)
data ExportD 
  = TypeEx  Uident
  | VarEx  Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data ImportD 
  = ImportD  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data TypeD 
  = TypeD  Uident VarList TyDecl
  deriving (Show,Eq,Ord,Data,Typeable)
data VarD 
  = VarD  Lident DeclSym Exp
  deriving (Show,Eq,Ord,Data,Typeable)
data InstD 
  = InstD  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data DeclSym 
  = Macro 
  | Define 
  deriving (Show,Eq,Ord,Data,Typeable)
data TyDecl 
  = TyRecord  FieldTList
  | TyTagged  ConCList
  | TyEnum  EnumCList
  | TySyn  Type
  | TyNewtype  Uident Type
  | TyUnit  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data Exp 
  = LetE  Lident DeclSym Exp Exp
  | LamE  Lident Exp
  | SwitchE  Exp Default SwitchAltList
  | AppE  Exp Exp
  | AscribeE  Exp Type
  | VarE  Lident
  | LitE  Lit
  deriving (Show,Eq,Ord,Data,Typeable)
data Default 
  = DefaultNone 
  | DefaultSome  Exp
  deriving (Show,Eq,Ord,Data,Typeable)
data Lit 
  = CharL  Char
  | StringL  String
  | NmbrL  Number
  deriving (Show,Eq,Ord,Data,Typeable)
data Type 
  = TyCxt  CxtList Type
  | TyFun  Type Type
  | TyVoid 
  | TyConstr  Uident TypeList
  | TyVarT  Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data SwitchAlt 
  = SwitchAlt  Exp Exp
  deriving (Show,Eq,Ord,Data,Typeable)
data Cxt 
  = Cxt  Uident VarList
  deriving (Show,Eq,Ord,Data,Typeable)
data ConC 
  = ConC  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data EnumC 
  = EnumC  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data FieldT 
  = FieldT  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data Var 
  = Var  Lident
  deriving (Show,Eq,Ord,Data,Typeable)
type ExportDList  = [ExportD ]
type ImportDList  = [ImportD ]
type VarDList  = [VarD ]
type TypeDList  = [TypeD ]
type InstDList  = [InstD ]
type SwitchAltList  = [SwitchAlt ]
type EnumCList  = [EnumC ]
type ConCList  = [ConC ]
type FieldTList  = [FieldT ]
type VarList  = [Var ]
type TypeList  = [Type ]
type CxtList  = [Cxt ]
instance Pretty (Module ) where
  pretty = ppModule
ppModule x = case x of
  Module  v1 v2 v3 v4 v5 v6 -> text "module" <+> ppUident v1 <+> text "{" <+> ppExportDList v2 <+> ppImportDList v3 <+> ppTypeDList v4 <+> ppInstDList v5 <+> ppVarDList v6 <+> text "}"
instance Pretty (ExportD ) where
  pretty = ppExportD
ppExportD x = case x of
  TypeEx  v1 -> text "export" <+> ppUident v1
  VarEx  v1 -> text "export" <+> ppLident v1
instance Pretty (ImportD ) where
  pretty = ppImportD
ppImportD x = case x of
  ImportD  v1 -> text "import" <+> ppUident v1
instance Pretty (TypeD ) where
  pretty = ppTypeD
ppTypeD x = case x of
  TypeD  v1 v2 v3 -> text "type" <+> ppUident v1 <+> ppVarList v2 <+> text "=" <+> ppTyDecl v3
instance Pretty (VarD ) where
  pretty = ppVarD
ppVarD x = case x of
  VarD  v1 v2 v3 -> ppLident v1 <+> ppDeclSym v2 <+> ppExp v3
instance Pretty (InstD ) where
  pretty = ppInstD
ppInstD x = case x of
  InstD  v1 v2 -> text "instance" <+> ppUident v1 <+> ppType v2
instance Pretty (DeclSym ) where
  pretty = ppDeclSym
ppDeclSym x = case x of
  Macro   -> text "=>"
  Define   -> text "="
instance Pretty (TyDecl ) where
  pretty = ppTyDecl
ppTyDecl x = case x of
  TyRecord  v1 -> text "record" <+> text "{" <+> ppFieldTList v1 <+> text "}"
  TyTagged  v1 -> text "tagged" <+> ppConCList v1
  TyEnum  v1 -> text "enum" <+> ppEnumCList v1
  TySyn  v1 -> text "synonym" <+> ppType v1
  TyNewtype  v1 v2 -> text "newtype" <+> ppUident v1 <+> ppType v2
  TyUnit  v1 -> text "unit" <+> ppUident v1
instance Pretty (Exp ) where
  pretty = ppExp
ppExp x = case x of
  LetE  v1 v2 v3 v4 -> text "let" <+> ppLident v1 <+> ppDeclSym v2 <+> ppExp v3 <+> text "in" <+> ppExp v4
  LamE  v1 v2 -> text "\\" <+> ppLident v1 <+> text "->" <+> ppExp v2
  SwitchE  v1 v2 v3 -> text "switch" <+> ppExp v1 <+> text "of" <+> ppDefault v2 <+> text "{" <+> ppSwitchAltList v3 <+> text "}"
  AppE  v1 v2 -> text "(" <+> ppExp v1 <+> ppExp v2 <+> text ")"
  AscribeE  v1 v2 -> text "(" <+> ppExp v1 <+> text "::" <+> ppType v2 <+> text ")"
  VarE  v1 -> ppLident v1
  LitE  v1 -> ppLit v1
instance Pretty (Default ) where
  pretty = ppDefault
ppDefault x = case x of
  DefaultNone   -> Text.PrettyPrint.Leijen.empty
  DefaultSome  v1 -> ppExp v1
instance Pretty (Lit ) where
  pretty = ppLit
ppLit x = case x of
  CharL  v1 -> ppChar v1
  StringL  v1 -> ppString v1
  NmbrL  v1 -> ppNumber v1
instance Pretty (Type ) where
  pretty = ppType
ppType x = case x of
  TyCxt  v1 v2 -> text "{" <+> ppCxtList v1 <+> text "}" <+> text "=>" <+> ppType v2
  TyFun  v1 v2 -> text "(" <+> ppType v1 <+> text "->" <+> ppType v2 <+> text ")"
  TyVoid   -> text "VoidT"
  TyConstr  v1 v2 -> text "(" <+> ppUident v1 <+> ppTypeList v2 <+> text ")"
  TyVarT  v1 -> ppLident v1
instance Pretty (SwitchAlt ) where
  pretty = ppSwitchAlt
ppSwitchAlt x = case x of
  SwitchAlt  v1 v2 -> ppExp v1 <+> text "->" <+> ppExp v2
instance Pretty (Cxt ) where
  pretty = ppCxt
ppCxt x = case x of
  Cxt  v1 v2 -> ppUident v1 <+> ppVarList v2
instance Pretty (ConC ) where
  pretty = ppConC
ppConC x = case x of
  ConC  v1 v2 -> ppUident v1 <+> ppType v2
instance Pretty (EnumC ) where
  pretty = ppEnumC
ppEnumC x = case x of
  EnumC  v1 -> ppUident v1
instance Pretty (FieldT ) where
  pretty = ppFieldT
ppFieldT x = case x of
  FieldT  v1 v2 -> ppUident v1 <+> text "::" <+> ppType v2
instance Pretty (Var ) where
  pretty = ppVar
ppVar x = case x of
  Var  v1 -> ppLident v1
ppExportDList = ppList ppExportD Terminator ";" Vert
ppImportDList = ppList ppImportD Terminator ";" Vert
ppVarDList = ppList ppVarD Terminator ";" Vert
ppTypeDList = ppList ppTypeD Terminator ";" Vert
ppInstDList = ppList ppInstD Terminator ";" Vert
ppSwitchAltList = ppList ppSwitchAlt Terminator ";" Vert
ppEnumCList = ppList ppEnumC Separator "|" Horiz
ppConCList = ppList ppConC Separator "|" Horiz
ppFieldTList = ppList ppFieldT Separator "," Horiz
ppVarList = ppList ppVar Separator "" Horiz
ppTypeList = ppList ppType Separator "" Horiz
ppCxtList = ppList ppCxt Separator "," Horiz