{-# LANGUAGE DeriveDataTypeable #-}
module Language.LLVM.Abs where
import Control.DeepSeq
import Data.Generics
import Grm.Prims
import Grm.Lex
import Text.PrettyPrint.Leijen
myLexemes = ["\"","(",")","*",",",", i32 0,",", i32 0, i32 0)",", label %","...",":","::",";","=","= private constant [","[","\\00\"","]","add nuw nsw","alloca","and","bitcast","br i1","br label %","call","declare","define","double","eq","fadd","false","fcmp","fdiv","float","fmul","fpext","fptosi","fptoui","fptrunc","frem","fsub","getelementptr inbounds","getelementptr inbounds ([","i","i8","icmp","load","lshr","mul nuw nsw","ne","oge","ogt","ole","olt","or","ret","sdiv","sext","sge","sgt","shl","sitofp","sle","slt","srem","store","sub nuw nsw","switch","to","true","trunc","type","udiv","uge","ugt","uitofp","ule","ult","urem","void","x","x i8 ] c","x i8 ]*","xor","zext","{","|","}"]
grmLexFilePath = lexFilePath myLexemes
grmLexContents = lexContents myLexemes
data Module 
  = Module  DefineList
  deriving (Show,Eq,Ord,Data,Typeable)
data Define 
  = Define  Type Lident TVarList StmtList
  | Declare  Type Lident TypeList
  | TypeD  Lident Type
  | StringD  Lident Number Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data Stmt 
  = LetS  Lident Exp
  | StoreS  Atom TVar
  | CallS  TVar AtomList
  | SwitchS  Atom Lident SwitchAltList
  | ReturnS  Atom
  | LabelS  Lident
  | BrS  UAtom Lident Lident
  | Br0S  Lident
  | TagS  Atom
  | NoOpS 
  deriving (Show,Eq,Ord,Data,Typeable)
data Exp 
  = CallE  TVar AtomList
  | CastE  Cast TVar Type
  | AllocaE  Type
  | LoadE  TVar
  | AtomE  Atom
  | BinOpE  BinOp Type UAtom UAtom
  | IdxE  TVar Atom
  deriving (Show,Eq,Ord,Data,Typeable)
data Cast 
  = Bitcast 
  | Sitofp 
  | Uitofp 
  | Fptosi 
  | Fptoui 
  | Trunc 
  | Zext 
  | Sext 
  | Fptrunc 
  | Fpext 
  deriving (Show,Eq,Ord,Data,Typeable)
data BinOp 
  = Icmp  ICond
  | Fcmp  FCond
  | Add 
  | Fadd 
  | Sub 
  | Fsub 
  | Mul 
  | Fmul 
  | Udiv 
  | Sdiv 
  | Fdiv 
  | Urem 
  | Srem 
  | Frem 
  | Shl 
  | Lshr 
  | And 
  | Or 
  | Xor 
  deriving (Show,Eq,Ord,Data,Typeable)
data Atom 
  = LitA  TLit
  | VarA  TVar
  deriving (Show,Eq,Ord,Data,Typeable)
data UAtom 
  = LitUA  Lit
  | VarUA  Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data ICond 
  = Equ 
  | Neq 
  | Ugt 
  | Sgt 
  | Uge 
  | Sge 
  | Ult 
  | Slt 
  | Ule 
  | Sle 
  deriving (Show,Eq,Ord,Data,Typeable)
data FCond 
  = Ogt 
  | Oge 
  | Olt 
  | Ole 
  deriving (Show,Eq,Ord,Data,Typeable)
data SwitchAlt 
  = SwitchAlt  TLit Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data Lit 
  = NmbrL  Number
  | FalseL 
  | TrueL 
  | StringL  Number Lident
  | VoidL 
  deriving (Show,Eq,Ord,Data,Typeable)
data TVar 
  = TVar  Type Lident
  deriving (Show,Eq,Ord,Data,Typeable)
data TLit 
  = TLit  Type Lit
  deriving (Show,Eq,Ord,Data,Typeable)
data FieldT 
  = FieldT  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data ConC 
  = ConC  Uident Type
  deriving (Show,Eq,Ord,Data,Typeable)
data Type 
  = PtrT  Type
  | IntT  Number
  | FloatT 
  | DoubleT 
  | VoidT 
  | FunT  Type TypeList
  | VarArgsT 
  | UserT  Lident
  | StructT  TypeList
  | ArrayT  Number Type
  | CharT 
  deriving (Show,Eq,Ord,Data,Typeable)
type ConCList  = [ConC ]
type FieldTList  = [FieldT ]
type TypeList  = [Type ]
type ExpList  = [Exp ]
type AtomList  = [Atom ]
type TVarList  = [TVar ]
type SwitchAltList  = [SwitchAlt ]
type StmtList  = [Stmt ]
type DefineList  = [Define ]
instance Pretty (Module ) where
  pretty = ppModule
ppModule x = case x of
  Module  v1 -> ppDefineList v1
instance Pretty (Define ) where
  pretty = ppDefine
ppDefine x = case x of
  Define  v1 v2 v3 v4 -> text "define" <+> ppType v1 <+> ppLident v2 <+> text "(" <+> ppTVarList v3 <+> text ")" <+> text "{" <+> ppStmtList v4 <+> text "}"
  Declare  v1 v2 v3 -> text "declare" <+> ppType v1 <+> ppLident v2 <+> text "(" <+> ppTypeList v3 <+> text ")"
  TypeD  v1 v2 -> ppLident v1 <+> text "=" <+> text "type" <+> ppType v2
  StringD  v1 v2 v3 -> ppLident v1 <+> text "= private constant [" <+> ppNumber v2 <+> text "x i8 ] c" <> text "\"" <> ppLident v3 <> text "\\00\""
instance Pretty (Stmt ) where
  pretty = ppStmt
ppStmt x = case x of
  LetS  v1 v2 -> ppLident v1 <+> text "=" <+> ppExp v2
  StoreS  v1 v2 -> text "store" <+> ppAtom v1 <> text "," <+> ppTVar v2
  CallS  v1 v2 -> text "call" <+> ppTVar v1 <+> text "(" <+> ppAtomList v2 <+> text ")"
  SwitchS  v1 v2 v3 -> text "switch" <+> ppAtom v1 <> text ", label %" <> ppLident v2 <+> text "[" <+> ppSwitchAltList v3 <+> text "]"
  ReturnS  v1 -> text "ret" <+> ppAtom v1
  LabelS  v1 -> ppLident v1 <> text ":"
  BrS  v1 v2 v3 -> text "br i1" <+> ppUAtom v1 <> text ", label %" <> ppLident v2 <> text ", label %" <> ppLident v3
  Br0S  v1 -> text "br label %" <> ppLident v1
  TagS  v1 -> text "store" <+> ppAtom v1 <> text ","
  NoOpS   -> text ";"
instance Pretty (Exp ) where
  pretty = ppExp
ppExp x = case x of
  CallE  v1 v2 -> text "call" <+> ppTVar v1 <+> text "(" <+> ppAtomList v2 <+> text ")"
  CastE  v1 v2 v3 -> ppCast v1 <+> ppTVar v2 <+> text "to" <+> ppType v3
  AllocaE  v1 -> text "alloca" <+> ppType v1
  LoadE  v1 -> text "load" <+> ppTVar v1
  AtomE  v1 -> ppAtom v1
  BinOpE  v1 v2 v3 v4 -> ppBinOp v1 <+> ppType v2 <+> ppUAtom v3 <> text "," <+> ppUAtom v4
  IdxE  v1 v2 -> text "getelementptr inbounds" <+> ppTVar v1 <> text ", i32 0," <+> ppAtom v2
instance Pretty (Cast ) where
  pretty = ppCast
ppCast x = case x of
  Bitcast   -> text "bitcast"
  Sitofp   -> text "sitofp"
  Uitofp   -> text "uitofp"
  Fptosi   -> text "fptosi"
  Fptoui   -> text "fptoui"
  Trunc   -> text "trunc"
  Zext   -> text "zext"
  Sext   -> text "sext"
  Fptrunc   -> text "fptrunc"
  Fpext   -> text "fpext"
instance Pretty (BinOp ) where
  pretty = ppBinOp
ppBinOp x = case x of
  Icmp  v1 -> text "icmp" <+> ppICond v1
  Fcmp  v1 -> text "fcmp" <+> ppFCond v1
  Add   -> text "add nuw nsw"
  Fadd   -> text "fadd"
  Sub   -> text "sub nuw nsw"
  Fsub   -> text "fsub"
  Mul   -> text "mul nuw nsw"
  Fmul   -> text "fmul"
  Udiv   -> text "udiv"
  Sdiv   -> text "sdiv"
  Fdiv   -> text "fdiv"
  Urem   -> text "urem"
  Srem   -> text "srem"
  Frem   -> text "frem"
  Shl   -> text "shl"
  Lshr   -> text "lshr"
  And   -> text "and"
  Or   -> text "or"
  Xor   -> text "xor"
instance Pretty (Atom ) where
  pretty = ppAtom
ppAtom x = case x of
  LitA  v1 -> ppTLit v1
  VarA  v1 -> ppTVar v1
instance Pretty (UAtom ) where
  pretty = ppUAtom
ppUAtom x = case x of
  LitUA  v1 -> ppLit v1
  VarUA  v1 -> ppLident v1
instance Pretty (ICond ) where
  pretty = ppICond
ppICond x = case x of
  Equ   -> text "eq"
  Neq   -> text "ne"
  Ugt   -> text "ugt"
  Sgt   -> text "sgt"
  Uge   -> text "uge"
  Sge   -> text "sge"
  Ult   -> text "ult"
  Slt   -> text "slt"
  Ule   -> text "ule"
  Sle   -> text "sle"
instance Pretty (FCond ) where
  pretty = ppFCond
ppFCond x = case x of
  Ogt   -> text "ogt"
  Oge   -> text "oge"
  Olt   -> text "olt"
  Ole   -> text "ole"
instance Pretty (SwitchAlt ) where
  pretty = ppSwitchAlt
ppSwitchAlt x = case x of
  SwitchAlt  v1 v2 -> ppTLit v1 <> text ", label %" <> ppLident v2
instance Pretty (Lit ) where
  pretty = ppLit
ppLit x = case x of
  NmbrL  v1 -> ppNumber v1
  FalseL   -> text "false"
  TrueL   -> text "true"
  StringL  v1 v2 -> text "getelementptr inbounds ([" <+> ppNumber v1 <+> text "x i8 ]*" <+> ppLident v2 <> text ", i32 0, i32 0)"
  VoidL   -> Text.PrettyPrint.Leijen.empty
instance Pretty (TVar ) where
  pretty = ppTVar
ppTVar x = case x of
  TVar  v1 v2 -> ppType v1 <+> ppLident v2
instance Pretty (TLit ) where
  pretty = ppTLit
ppTLit x = case x of
  TLit  v1 v2 -> ppType v1 <+> ppLit v2
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
  PtrT  v1 -> ppType v1 <> text "*"
  IntT  v1 -> text "i" <> ppNumber v1
  FloatT   -> text "float"
  DoubleT   -> text "double"
  VoidT   -> text "void"
  FunT  v1 v2 -> ppType v1 <+> text "(" <+> ppTypeList v2 <+> text ")"
  VarArgsT   -> text "..."
  UserT  v1 -> ppLident v1
  StructT  v1 -> text "{" <+> ppTypeList v1 <+> text "}"
  ArrayT  v1 v2 -> text "[" <+> ppNumber v1 <+> text "x" <+> ppType v2 <+> text "]"
  CharT   -> text "i8"
ppConCList = ppList ppConC Separator "|" Horiz
ppFieldTList = ppList ppFieldT Separator "," Horiz
ppTypeList = ppList ppType Separator "," Horiz
ppExpList = ppList ppExp Separator "," Horiz
ppAtomList = ppList ppAtom Separator "," Horiz
ppTVarList = ppList ppTVar Separator "," Horiz
ppSwitchAltList = ppList ppSwitchAlt Terminator "" Vert
ppStmtList = ppList ppStmt Terminator "" Vert
ppDefineList = ppList ppDefine Terminator "" Vert