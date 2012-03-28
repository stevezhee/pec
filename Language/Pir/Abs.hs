{-# LANGUAGE DeriveDataTypeable #-}
module Language.Pir.Abs where
import Control.DeepSeq
import Data.Generics
import Grm.Prims
import Grm.Lex
import Text.PrettyPrint.Leijen
myLexemes = ["(",")",",","->","::",":=",";","=","alloca","call","cast","define","enum","if","import","load","module","noop","record","return","switch","tagged","when","while","{","|","}"]
grmLexFilePath = lexFilePath myLexemes
grmLexContents = lexContents myLexemes
data Module 
  = Module  Uident ImportList DefineList
  deriving (Show,Eq,Ord,Data,Typeable)
data Import 
  = Import  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data Define 
  = Define  Type Lident TVarList StmtList
  deriving (Show,Eq,Ord,Data,Typeable)
data Stmt 
  = LetS  TVar Exp
  | StoreS  TVar Atom
  | CallS  TVar AtomList
  | SwitchS  Atom StmtList SwitchAltList
  | IfS  Atom StmtList StmtList
  | WhenS  Atom StmtList
  | WhileS  StmtList Atom StmtList
  | ReturnS  Atom
  | NoOpS 
  deriving (Show,Eq,Ord,Data,Typeable)
data Exp 
  = CallE  TVar AtomList
  | CastE  TVar Type
  | AllocaE  Type
  | LoadE  TVar
  | AtomE  Atom
  deriving (Show,Eq,Ord,Data,Typeable)
data Atom 
  = LitA  TLit
  | VarA  TVar
  deriving (Show,Eq,Ord,Data,Typeable)
data SwitchAlt 
  = SwitchAlt  TLit StmtList
  deriving (Show,Eq,Ord,Data,Typeable)
data Lit 
  = StringL  String
  | NmbrL  Number
  | CharL  Char
  | EnumL  Uident
  | VoidL 
  deriving (Show,Eq,Ord,Data,Typeable)
data TyDecl 
  = TyEnum  EnumCList
  | TyRecord  FieldTList
  | TyTagged  ConCList
  deriving (Show,Eq,Ord,Data,Typeable)
data TVar 
  = TVar  Lident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data TLit 
  = TLit  Lit Type
  deriving (Show,Eq,Ord,Data,Typeable)
data FieldT 
  = FieldT  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data ConC 
  = ConC  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data Type 
  = Type  Uident TypeList
  deriving (Show,Eq,Ord,Data,Typeable)
data EnumC 
  = EnumC  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
type ConCList  = [ConC ]
type FieldTList  = [FieldT ]
type EnumCList  = [EnumC ]
type TypeList  = [Type ]
type ExpList  = [Exp ]
type AtomList  = [Atom ]
type TVarList  = [TVar ]
type SwitchAltList  = [SwitchAlt ]
type StmtList  = [Stmt ]
type DefineList  = [Define ]
type ImportList  = [Import ]
instance Pretty (Module ) where
  pretty = ppModule
ppModule x = case x of
  Module  v1 v2 v3 -> text "module" <+> ppUident v1 <+> ppImportList v2 <+> ppDefineList v3
instance Pretty (Import ) where
  pretty = ppImport
ppImport x = case x of
  Import  v1 -> text "import" <+> ppUident v1
instance Pretty (Define ) where
  pretty = ppDefine
ppDefine x = case x of
  Define  v1 v2 v3 v4 -> text "define" <+> ppType v1 <+> ppLident v2 <+> text "(" <+> ppTVarList v3 <+> text ")" <+> text "=" <+> text "{" <+> ppStmtList v4 <+> text "}"
instance Pretty (Stmt ) where
  pretty = ppStmt
ppStmt x = case x of
  LetS  v1 v2 -> ppTVar v1 <+> text "=" <+> ppExp v2
  StoreS  v1 v2 -> ppTVar v1 <+> text ":=" <+> ppAtom v2
  CallS  v1 v2 -> text "call" <+> ppTVar v1 <+> text "(" <+> ppAtomList v2 <+> text ")"
  SwitchS  v1 v2 v3 -> text "switch" <+> ppAtom v1 <+> text "{" <+> ppStmtList v2 <+> text "}" <+> text "{" <+> ppSwitchAltList v3 <+> text "}"
  IfS  v1 v2 v3 -> text "if" <+> ppAtom v1 <+> text "{" <+> ppStmtList v2 <+> text "}" <+> text "{" <+> ppStmtList v3 <+> text "}"
  WhenS  v1 v2 -> text "when" <+> ppAtom v1 <+> text "{" <+> ppStmtList v2 <+> text "}"
  WhileS  v1 v2 v3 -> text "while" <+> text "{" <+> ppStmtList v1 <+> text "}" <+> ppAtom v2 <+> text "{" <+> ppStmtList v3 <+> text "}"
  ReturnS  v1 -> text "return" <+> ppAtom v1
  NoOpS   -> text "noop"
instance Pretty (Exp ) where
  pretty = ppExp
ppExp x = case x of
  CallE  v1 v2 -> ppTVar v1 <+> text "(" <+> ppAtomList v2 <+> text ")"
  CastE  v1 v2 -> text "cast" <+> ppTVar v1 <+> ppType v2
  AllocaE  v1 -> text "alloca" <+> ppType v1
  LoadE  v1 -> text "load" <+> text "(" <+> ppTVar v1 <+> text ")"
  AtomE  v1 -> ppAtom v1
instance Pretty (Atom ) where
  pretty = ppAtom
ppAtom x = case x of
  LitA  v1 -> ppTLit v1
  VarA  v1 -> ppTVar v1
instance Pretty (SwitchAlt ) where
  pretty = ppSwitchAlt
ppSwitchAlt x = case x of
  SwitchAlt  v1 v2 -> ppTLit v1 <+> text "->" <+> text "{" <+> ppStmtList v2 <+> text "}"
instance Pretty (Lit ) where
  pretty = ppLit
ppLit x = case x of
  StringL  v1 -> ppString v1
  NmbrL  v1 -> ppNumber v1
  CharL  v1 -> ppChar v1
  EnumL  v1 -> ppUident v1
  VoidL   -> Text.PrettyPrint.Leijen.empty
instance Pretty (TyDecl ) where
  pretty = ppTyDecl
ppTyDecl x = case x of
  TyEnum  v1 -> text "enum" <+> ppEnumCList v1
  TyRecord  v1 -> text "record" <+> text "{" <+> ppFieldTList v1 <+> text "}"
  TyTagged  v1 -> text "tagged" <+> ppConCList v1
instance Pretty (TVar ) where
  pretty = ppTVar
ppTVar x = case x of
  TVar  v1 v2 -> ppLident v1 <+> text "::" <+> ppType v2
instance Pretty (TLit ) where
  pretty = ppTLit
ppTLit x = case x of
  TLit  v1 v2 -> ppLit v1 <+> text "::" <+> ppType v2
instance Pretty (FieldT ) where
  pretty = ppFieldT
ppFieldT x = case x of
  FieldT  v1 v2 -> ppUident v1 <+> text "::" <+> ppType v2
instance Pretty (ConC ) where
  pretty = ppConC
ppConC x = case x of
  ConC  v1 v2 -> ppUident v1 <+> ppType v2
instance Pretty (Type ) where
  pretty = ppType
ppType x = case x of
  Type  v1 v2 -> ppUident v1 <+> text "(" <+> ppTypeList v2 <+> text ")"
instance Pretty (EnumC ) where
  pretty = ppEnumC
ppEnumC x = case x of
  EnumC  v1 -> ppUident v1
ppConCList = ppList ppConC Separator "|" Horiz
ppFieldTList = ppList ppFieldT Separator "," Horiz
ppEnumCList = ppList ppEnumC Separator "|" Horiz
ppTypeList = ppList ppType Separator "," Horiz
ppExpList = ppList ppExp Separator "," Horiz
ppAtomList = ppList ppAtom Separator "," Horiz
ppTVarList = ppList ppTVar Separator "," Horiz
ppSwitchAltList = ppList ppSwitchAlt Terminator ";" Vert
ppStmtList = ppList ppStmt Terminator ";" Vert
ppDefineList = ppList ppDefine Terminator ";" Vert
ppImportList = ppList ppImport Terminator ";" Vert