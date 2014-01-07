(* Définition de la structure de l'arbre de syntaxe abstraite typé *)

(* Localisation d'un token *)
type loc = Lexing.position * Lexing.position

(* Ensemble de type de l'arbre de syntaxe abstraite *)
type fichier = {iostr: bool; fichierDecl: decl list}

and decl = 
| DeclVars of declVars
| DeclClass of declClass
| ProtoBloc of proto * bloc
    
and declVars = var list
  
and declClass = {className: string; supersOpt: supers option;
                 memberList: member list}
  
and supers = string list
  
and member = 
| MemberDeclVars of declVars
| VirtualProto of bool * proto

and proto = {protoVar: protoVarT; argumentList: argument list; 
	     protoKind: protoKindT}
(* On décore l'arbre avec le type du prototype (méthode, fonction, constructeur) *)
and protoKindT = Function | Method of string | Cons of string | Class

and protoVarT =
| Qvar of typ * qvar
| Tident of string
| TidentTident of string * string

and typ =
| TypNull
| TypVoid
| TypInt
| TypIdent of string
| TypPointer of typ

and argument = var

and var = {varIdent: string; varRef: bool; varTyp: typ}

and qvar =
| QvarQident of qident
| QvarPointer of qvar
| QvarReference of qvar

and qident =
| Ident of string
| IdentIdent of string * string

and expr = {exprTyp: typ; exprCont: exprContT}

and exprContT =
| ExprInt of int
| This
(*| False
| True
^ ils ont été enlevés lors de l'analyse de type*)
| Null
| ExprQident of qident
| ExprStar of expr
| ExprDot of expr * string
(* | ExprArrow of expr * string *)
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

and op =
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

and ins =
| InsSemicolon
| InsExpr of expr
| InsDef of var * (insDef option)
| InsIf of expr * ins
| InsIfElse of expr * ins * ins
| InsWhile of expr * ins
| InsFor of (expr list) * (expr) * (expr list) * ins
| InsBloc of bloc
| InsCout of expr_str list
| InsReturn of expr option
and insDef =
| InsDefExpr of expr
| InsDefIdent of string * (expr list)

and expr_str =
| ExprStrExpr of expr
| ExprStrStr of string

and bloc = ins list
