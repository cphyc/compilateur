(* Définition de la structure de l'arbre de syntaxe abstraite *)

(* Localisation d'un token *)
type loc = Lexing.position * Lexing.position

(* Ensemble de type de l'arbre de syntaxe abstraite *)
type fichier = {iostr: bool; fichierDecl: decl list; fichierLoc: loc}

and decl = 
| DeclVars of declVars
| DeclClass of declClass
| ProtoBloc of proto * bloc
    
and declVars = {declVarsTyp: typ; varList: var list; declVarsLoc: loc}
  
and declClass = {className: string; supersOpt: supers option;
                 memberList: member list; declClassLoc: loc}
  
and supers = string list
  
and member = 
| MemberDeclVars of declVars
| VirtualProto of bool * proto

and proto = {protoVar: protoVarT; argumentList: argument list;
             protoLoc: loc}

and protoVarT =
| Qvar of typ * qvar
| Tident of string
| TidentTident of string * string

and typ = {typCont: typContT; typLoc: loc}

and typContT =
| TypVoid
| TypInt
| TypIdent of string

and argument = {argumentTyp: typ; argumentVar: var; argumentLoc: loc}

and var = {varCont: varContT; varLoc: loc}
and varContT =
| VarIdent of string
| VarPointer of var
| VarReference of var

and qvar = {qvarCont: qvarContT; qvarLoc: loc}
and qvarContT =
| QvarQident of qident
| QvarPointer of qvar
| QvarReference of qvar

and qident =
| Ident of string
| IdentIdent of string * string

and expr = {exprCont: exprContT; exprLoc: loc}
and exprContT =
| ExprInt of int
| This
| False
| True
| Null
| ExprQident of qident
| ExprStar of expr
| ExprDot of expr * string
| ExprArrow of expr * string
| ExprEqual of expr * expr
| ExprApply of expr * (expr list)
| ExprNew of string * (expr list)
| ExprLIncr of expr
| ExprLDecr of expr
| ExprRIncr of expr
| ExprRDecr of expr
| ExprAmpersand of expr
| ExprExclamation of expr
| ExprMinus of expr
| ExprPlus of expr
| ExprOp of expr * op * expr
| ExprParenthesis of expr

and op = {opCont: opCont; opLoc: loc}
and opCont =
| OpEqual
| OpDiff
| OpLesser
| OpLesserEqual
| OpGreater
| OpGreaterEqual
| OpPlus
| OpMinus
| OpTimes
| OpDivide
| OpModulo
| OpAnd
| OpOr

and ins = {insCont: insContT; insLoc: loc}
and insContT =
| InsSemicolon
| InsExpr of expr
| InsDef of typ * var * (insDef option)
| InsIf of expr * ins
| InsIfElse of expr * ins * ins
| InsWhile of expr * ins
| InsFor of (expr list) * (expr option) * (expr list) * ins
| InsBloc of bloc
| InsCout of expr_str list
| InsReturn of expr option
and insDef =
| InsDefExpr of expr
| InsDefIdent of string * (expr list)

and expr_str =
| ExprStrExpr of expr
| ExprStrStr of string

and bloc = {blocCont : (ins list); blocLoc: loc}
