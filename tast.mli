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
  
and declClass = { className: string; supersOpt: supers option;
                  memberList: member list}
  
and supers = string list
  
and member = 
| MemberDeclVars of declVars
| VirtualProto of bool * proto

and proto = {protoVar: protoVarT ; argumentList: argument list }

and protoVarT =
| Qvar of qvar (* Fonction ou méthode *)
| Tident of string (* Cons *)

and typ =
| TypNull
| TypVoid
| TypInt
| TypIdent of string
| TypPointer of typ

and argument = var

and var = {varIdent: string; varRef: bool; varTyp: typ}

and qvar = {qvarIdent: qident; qvarRef: bool; qvarTyp: typ}

and qident =
| Ident of string
| IdentIdent of string * string

and expr = {exprTyp: typ; exprCont: exprContT}

and exprContT =
| ExprInt of int
| This
| Null
| ExprQident of bool * qident
(* bool permet de savoir si c'est une reference *)
| ExprStar of expr
| ExprDot of expr * string
| ExprEqual of expr * expr
(* typ list : profil de la méthode appliquée *)
| ExprApply of expr * ((typ * bool) list) * (expr list) 
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
| InsReturn of (bool * expr option)
and insDef =
| InsDefExpr of expr
| InsDefIdent of string * (expr list)

and expr_str =
| ExprStrExpr of expr
| ExprStrStr of string

and bloc = ins list
