{-# LANGUAGE DeriveDataTypeable #-}
module Language.C.Abs where
import Control.DeepSeq
import Data.Generics
import Grm.Prims
import Grm.Lex
import Text.PrettyPrint.Leijen
myLexemes = ["\n","\n#define","\n#endif","!","#ifndef","#include","&","(",")","*","++",",","--","->",".",":",";","<","=",">","[","]","alloca","break","case","default:","else","enum","if","return","sizeof","struct","switch","typedef","union","while","{","}"]
grmLexFilePath = lexFilePath myLexemes
grmLexContents = lexContents myLexemes
data Module 
  = HModule  Uident Uident ImportList DeclareList
  | CModule  ImportList DefineList
  deriving (Show,Eq,Ord,Data,Typeable)
data Import 
  = Import  String
  | GImport  Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data Declare 
  = Declare  FunDecl
  | Typedef  Decl
  deriving (Show,Eq,Ord,Data,Typeable)
data Define 
  = Define  FunDecl StmtList
  deriving (Show,Eq,Ord,Data,Typeable)
data FunDecl 
  = FunFD  Type Lident ArgDeclList
  | RetFunFD  Type Lident ArgDeclList TypeList
  deriving (Show,Eq,Ord,Data,Typeable)
data Type 
  = TyName  Uident
  | TyPtr  Type
  | TyFun  Type TypeList
  | TyArray  Type Number
  | TyEnum  EnumCList
  | TyStruct  DeclList
  | TyUnion  DeclList
  deriving (Show,Eq,Ord,Data,Typeable)
data EnumC 
  = EnumC  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
data Decl 
  = Decl  Type Lident
  | FunD  Type Lident TypeList
  deriving (Show,Eq,Ord,Data,Typeable)
data Exp 
  = AllocaE  Type
  | VarE  Lident
  | LitE  Lit
  | CastE  Type Exp
  | LoadE  Exp
  | ArrowE  Exp Exp
  | DotE  Exp Exp
  | AddrE  Exp
  | IdxE  Exp Exp
  | CallE  Exp ExpList
  | BinOpE  Exp Usym Exp
  | ParenE  Exp
  | NotE  Exp
  deriving (Show,Eq,Ord,Data,Typeable)
data Stmt 
  = DeclS  Decl
  | AssignS  Exp Exp
  | CallS  Exp ExpList
  | SwitchS  Exp SwitchAltList
  | BreakS 
  | IfS  Exp StmtList StmtList
  | WhenS  Exp StmtList
  | WhileS  Exp StmtList
  | ReturnS  Exp
  | RetVoidS 
  | NoOpS 
  | BlockS  StmtList
  | IncS  Exp
  | DecS  Exp
  deriving (Show,Eq,Ord,Data,Typeable)
data SwitchAlt 
  = SwitchAlt  Lit StmtList
  | DefaultAlt  StmtList
  deriving (Show,Eq,Ord,Data,Typeable)
data Lit 
  = StringL  String
  | CharL  Char
  | NmbrL  Number
  | EnumL  Uident
  deriving (Show,Eq,Ord,Data,Typeable)
type TypeList  = [Type ]
type ExpList  = [Exp ]
type EnumCList  = [EnumC ]
type DeclList  = [Decl ]
type ArgDeclList  = [Decl ]
type SwitchAltList  = [SwitchAlt ]
type StmtList  = [Stmt ]
type DeclareList  = [Declare ]
type DefineList  = [Define ]
type ImportList  = [Import ]
instance Pretty (Module ) where
  pretty = ppModule
ppModule x = case x of
  HModule  v1 v2 v3 v4 -> text "#ifndef" <+> ppUident v1 <+> text "\n#define" <+> ppUident v2 <+> text "\n" <+> ppImportList v3 <+> ppDeclareList v4 <+> text "\n#endif"
  CModule  v1 v2 -> ppImportList v1 <+> ppDefineList v2
instance Pretty (Import ) where
  pretty = ppImport
ppImport x = case x of
  Import  v1 -> text "#include" <+> ppString v1
  GImport  v1 -> text "#include" <+> text "<" <> ppLident v1 <> text ">"
instance Pretty (Declare ) where
  pretty = ppDeclare
ppDeclare x = case x of
  Declare  v1 -> ppFunDecl v1
  Typedef  v1 -> text "typedef" <+> ppDecl v1
instance Pretty (Define ) where
  pretty = ppDefine
ppDefine x = case x of
  Define  v1 v2 -> ppFunDecl v1 <+> text "{" <+> ppStmtList v2 <+> text "}"
instance Pretty (FunDecl ) where
  pretty = ppFunDecl
ppFunDecl x = case x of
  FunFD  v1 v2 v3 -> ppType v1 <+> ppLident v2 <> text "(" <> ppArgDeclList v3 <> text ")"
  RetFunFD  v1 v2 v3 v4 -> ppType v1 <+> text "(" <+> text "*" <+> ppLident v2 <+> text "(" <> ppArgDeclList v3 <> text ")" <+> text ")" <+> text "(" <> ppTypeList v4 <> text ")"
instance Pretty (Type ) where
  pretty = ppType
ppType x = case x of
  TyName  v1 -> ppUident v1
  TyPtr  v1 -> ppType v1 <> text "*"
  TyFun  v1 v2 -> ppType v1 <+> text "(" <> text "*" <> text ")" <+> text "(" <> ppTypeList v2 <> text ")"
  TyArray  v1 v2 -> ppType v1 <+> text "[" <+> ppNumber v2 <+> text "]"
  TyEnum  v1 -> text "enum" <+> text "{" <+> ppEnumCList v1 <+> text "}"
  TyStruct  v1 -> text "struct" <+> text "{" <+> ppDeclList v1 <+> text "}"
  TyUnion  v1 -> text "union" <+> text "{" <+> ppDeclList v1 <+> text "}"
instance Pretty (EnumC ) where
  pretty = ppEnumC
ppEnumC x = case x of
  EnumC  v1 -> ppUident v1
instance Pretty (Decl ) where
  pretty = ppDecl
ppDecl x = case x of
  Decl  v1 v2 -> ppType v1 <+> ppLident v2
  FunD  v1 v2 v3 -> ppType v1 <+> ppLident v2 <> text "(" <> ppTypeList v3 <> text ")"
instance Pretty (Exp ) where
  pretty = ppExp
ppExp x = case x of
  AllocaE  v1 -> text "alloca" <+> text "(" <+> text "sizeof" <+> text "(" <+> ppType v1 <+> text ")" <+> text ")"
  VarE  v1 -> ppLident v1
  LitE  v1 -> ppLit v1
  CastE  v1 v2 -> text "(" <+> ppType v1 <+> text ")" <+> ppExp v2
  LoadE  v1 -> text "*" <> ppExp v1
  ArrowE  v1 v2 -> ppExp v1 <> text "->" <> ppExp v2
  DotE  v1 v2 -> ppExp v1 <> text "." <> ppExp v2
  AddrE  v1 -> text "&" <> ppExp v1
  IdxE  v1 v2 -> ppExp v1 <> text "[" <> ppExp v2 <> text "]"
  CallE  v1 v2 -> ppExp v1 <> text "(" <> ppExpList v2 <> text ")"
  BinOpE  v1 v2 v3 -> text "(" <> ppExp v1 <+> ppUsym v2 <+> ppExp v3 <> text ")"
  ParenE  v1 -> text "(" <+> ppExp v1 <+> text ")"
  NotE  v1 -> text "!" <> ppExp v1
instance Pretty (Stmt ) where
  pretty = ppStmt
ppStmt x = case x of
  DeclS  v1 -> ppDecl v1
  AssignS  v1 v2 -> ppExp v1 <+> text "=" <+> ppExp v2
  CallS  v1 v2 -> ppExp v1 <> text "(" <> ppExpList v2 <> text ")"
  SwitchS  v1 v2 -> text "switch" <+> text "(" <+> ppExp v1 <+> text ")" <+> text "{" <+> ppSwitchAltList v2 <+> text "}"
  BreakS   -> text "break"
  IfS  v1 v2 v3 -> text "if" <+> text "(" <+> ppExp v1 <+> text ")" <+> text "{" <+> ppStmtList v2 <+> text "}" <+> text "else" <+> text "{" <+> ppStmtList v3 <+> text "}"
  WhenS  v1 v2 -> text "if" <+> text "(" <+> ppExp v1 <+> text ")" <+> text "{" <+> ppStmtList v2 <+> text "}"
  WhileS  v1 v2 -> text "while" <+> text "(" <+> ppExp v1 <+> text ")" <+> text "{" <+> ppStmtList v2 <+> text "}"
  ReturnS  v1 -> text "return" <> text "(" <> ppExp v1 <> text ")"
  RetVoidS   -> text "return"
  NoOpS   -> Text.PrettyPrint.Leijen.empty
  BlockS  v1 -> ppStmtList v1
  IncS  v1 -> ppExp v1 <> text "++"
  DecS  v1 -> ppExp v1 <> text "--"
instance Pretty (SwitchAlt ) where
  pretty = ppSwitchAlt
ppSwitchAlt x = case x of
  SwitchAlt  v1 v2 -> text "case" <+> ppLit v1 <> text ":" <+> ppStmtList v2
  DefaultAlt  v1 -> text "default:" <+> ppStmtList v1
instance Pretty (Lit ) where
  pretty = ppLit
ppLit x = case x of
  StringL  v1 -> ppString v1
  CharL  v1 -> ppChar v1
  NmbrL  v1 -> ppNumber v1
  EnumL  v1 -> ppUident v1
ppTypeList = ppList ppType Separator "," Horiz
ppExpList = ppList ppExp Separator "," Horiz
ppEnumCList = ppList ppEnumC Separator "," Horiz
ppDeclList = ppList ppDecl Terminator ";" Horiz
ppArgDeclList = ppList ppDecl Separator "," Horiz
ppSwitchAltList = ppList ppSwitchAlt Terminator "" Vert
ppStmtList = ppList ppStmt Terminator ";" Vert
ppDeclareList = ppList ppDeclare Terminator ";" Vert
ppDefineList = ppList ppDefine Terminator ";" Vert
ppImportList = ppList ppImport Terminator "" Vert