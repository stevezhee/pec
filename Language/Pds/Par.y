{
{-# OPTIONS -w #-}
-- Haskell module generated by grm *** DO NOT EDIT ***


module Language.Pds.Par where
import Grm.Prims
import Grm.Lex
import Language.Pds.Abs
}
%tokentype { (Token Point) }
%name grmParse
%token
  "(" { TSymbol _ "(" }
  ")" { TSymbol _ ")" }
  "," { TSymbol _ "," }
  "->" { TSymbol _ "->" }
  "::" { TSymbol _ "::" }
  ";" { TSymbol _ ";" }
  "=" { TSymbol _ "=" }
  "=>" { TSymbol _ "=>" }
  "VoidT" { TSymbol _ "VoidT" }
  "\\" { TSymbol _ "\\" }
  "count" { TSymbol _ "count" }
  "enum" { TSymbol _ "enum" }
  "export" { TSymbol _ "export" }
  "import" { TSymbol _ "import" }
  "in" { TSymbol _ "in" }
  "instance" { TSymbol _ "instance" }
  "let" { TSymbol _ "let" }
  "module" { TSymbol _ "module" }
  "newtype" { TSymbol _ "newtype" }
  "of" { TSymbol _ "of" }
  "record" { TSymbol _ "record" }
  "switch" { TSymbol _ "switch" }
  "synonym" { TSymbol _ "synonym" }
  "tagged" { TSymbol _ "tagged" }
  "type" { TSymbol _ "type" }
  "unit" { TSymbol _ "unit" }
  "{" { TSymbol _ "{" }
  "|" { TSymbol _ "|" }
  "}" { TSymbol _ "}" }

  uident { TUident _ _ }
  usym { TUsym _ _ }
  lident { TLident _ _ }
  string { TString _ _ }
  char { TChar _ _ }
  number { TNumber _ _ }
%%
Module
  : "module" uident "{" ExportDList ImportDList CountDList TypeDList InstDList VarDList "}" {Module   (unTUident $2)  $4 $5 $6 $7 $8 $9 }
  
ExportD
  : "export" uident {TypeEx   (unTUident $2)}
  | "export" lident {VarEx   (unTLident $2)}
ImportD
  : "import" uident {ImportD   (unTUident $2)}
  
CountD
  : "count" number {CountD   (unTNumber $2)}
  
TypeD
  : "type" uident VarList "=" TyDecl {TypeD   (unTUident $2) $3  $5}
  
VarD
  : lident DeclSym Exp {VarD  (unTLident $1) $2 $3}
  
InstD
  : "instance" uident Type {InstD   (unTUident $2) $3}
  
DeclSym
  : "=>" {Macro  }
  | "=" {Define  }
TyDecl
  : "record" "{" FieldTList "}" {TyRecord    $3 }
  | "tagged" ConCList {TyTagged   $2}
  | "enum" EnumCList {TyEnum   $2}
  | "synonym" Type {TySyn   $2}
  | "newtype" uident Type {TyNewtype   (unTUident $2) $3}
  | "unit" uident {TyUnit   (unTUident $2)}
Exp
  : "let" lident DeclSym Exp "in" Exp {LetE   (unTLident $2) $3 $4  $6}
  | "\\" lident "->" Exp {LamE   (unTLident $2)  $4}
  | "switch" Exp "of" Default "{" SwitchAltList "}" {SwitchE   $2  $4  $6 }
  | "(" Exp Exp ")" {AppE   $2 $3 }
  | "(" Exp "::" Type ")" {AscribeE   $2  $4 }
  | lident {VarE  (unTLident $1)}
  | Lit {LitE  $1}
Default
  : {DefaultNone }
  | Exp {DefaultSome  $1}
Lit
  : char {CharL  (unTChar $1)}
  | string {StringL  (unTString $1)}
  | number {NmbrL  (unTNumber $1)}
Type
  : "{" CxtList "}" "=>" Type {TyCxt   $2   $5}
  | "(" Type "->" Type ")" {TyFun   $2  $4 }
  | "VoidT" {TyVoid  }
  | "(" uident TypeList ")" {TyConstr   (unTUident $2) $3 }
  | lident {TyVarT  (unTLident $1)}
SwitchAlt
  : Exp "->" Exp {SwitchAlt  $1  $3}
  
Cxt
  : uident VarList {Cxt  (unTUident $1) $2}
  
ConC
  : uident Type {ConC  (unTUident $1) $2}
  
EnumC
  : uident {EnumC  (unTUident $1)}
  
FieldT
  : lident "::" Type {FieldT  (unTLident $1)  $3}
  
Var
  : lident {Var  (unTLident $1)}
  
ExportDList
  : REV_ExportDList {reverse $1}
  | {- empty -} { [] }
REV_ExportDList
  : ExportD ";" {[$1]}
  | REV_ExportDList ExportD ";" {$2 : $1}
ImportDList
  : REV_ImportDList {reverse $1}
  | {- empty -} { [] }
REV_ImportDList
  : ImportD ";" {[$1]}
  | REV_ImportDList ImportD ";" {$2 : $1}
VarDList
  : REV_VarDList {reverse $1}
  | {- empty -} { [] }
REV_VarDList
  : VarD ";" {[$1]}
  | REV_VarDList VarD ";" {$2 : $1}
TypeDList
  : REV_TypeDList {reverse $1}
  | {- empty -} { [] }
REV_TypeDList
  : TypeD ";" {[$1]}
  | REV_TypeDList TypeD ";" {$2 : $1}
CountDList
  : REV_CountDList {reverse $1}
  | {- empty -} { [] }
REV_CountDList
  : CountD ";" {[$1]}
  | REV_CountDList CountD ";" {$2 : $1}
InstDList
  : REV_InstDList {reverse $1}
  | {- empty -} { [] }
REV_InstDList
  : InstD ";" {[$1]}
  | REV_InstDList InstD ";" {$2 : $1}
SwitchAltList
  : REV_SwitchAltList {reverse $1}
  | {- empty -} { [] }
REV_SwitchAltList
  : SwitchAlt ";" {[$1]}
  | REV_SwitchAltList SwitchAlt ";" {$2 : $1}
EnumCList
  : REV_EnumCList {reverse $1}
  
REV_EnumCList
  : EnumC {[$1]}
  | REV_EnumCList "|" EnumC {$3 : $1}
ConCList
  : REV_ConCList {reverse $1}
  
REV_ConCList
  : ConC {[$1]}
  | REV_ConCList "|" ConC {$3 : $1}
FieldTList
  : REV_FieldTList {reverse $1}
  
REV_FieldTList
  : FieldT {[$1]}
  | REV_FieldTList "," FieldT {$3 : $1}
VarList
  : REV_VarList {reverse $1}
  | {- empty -} { [] }
REV_VarList
  : Var {[$1]}
  | REV_VarList Var {$2 : $1}
TypeList
  : REV_TypeList {reverse $1}
  | {- empty -} { [] }
REV_TypeList
  : Type {[$1]}
  | REV_TypeList Type {$2 : $1}
CxtList
  : REV_CxtList {reverse $1}
  | {- empty -} { [] }
REV_CxtList
  : Cxt {[$1]}
  | REV_CxtList "," Cxt {$3 : $1}