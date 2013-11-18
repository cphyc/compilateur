type loc = Lexing.position * Lexing.position

type fichier = {iostr: bool; decl: decl list; fichierLoc: loc}
  
and decl = 
|DeclVars of declVars
|DeclClass of declClass
|ProtoBloc of proto * bloc
    
and declVars = {declVarsTyp: typ; varList: var list; declVarsLoc: loc}
  
and declClass = {className: string; supersOpt: supers option;
                 memberList: member list; declClassLoc: loc}
  
and supers = string list
  
and member = 
|MemberDeclVars of declVars
|VirtualProto of bool * proto

and proto = {protoTyp: typ; protoVar: protoVar; argumentList: argument list;
             protoLoc: loc}

and protoVar =
|Qvar of qvar
|Qident of qident

and typ = {typCont: typCont; typLoc: loc}

and typCont =
|TypVoid
|TypInt
|TypIdent of string

and argument = {argumentTyp: typ; argumentVar: var; argumentLoc: loc}

and var = {varCont: varCont; varLoc: loc}
and varCont =
|VarIdent of string
|VarPointer of var
|VarReference of var

and qvar = {qvarCont: qvarCont; qvarLoc: loc}
and qvarCont =
|QvarQident of qident
|QvarPointer of qvar
|QvarReference of qvar

and qident =
|Ident of string
|IdentIdent of string * string

and expr = {exprCont: exprCont; exprLoc: loc}
and exprCont =
|ExprInt of int
|This
|False
|True
|Null
|ExprQident of qident
|ExprStar of expr
|ExprDot of expr * string
|ExprArrow of expr * string
|ExprEqual of expr * expr
|ExprApply of expr * (expr list)
|ExprNew of string * (expr list)
|ExprLIncr of expr
|ExprLDecr of expr
|ExprRIncr of expr
|ExprRDecr of expr
|ExprAmpersand of expr
|ExprExclamation of expr
|ExprMinus of expr
|ExprPlus of expr
|ExprOp of expr * op * expr
|ExprParenthesis of expr

and op = {opCont: opCont; opLoc: loc}
and opCont =
|OpEqual
|OpDiff
|OpLesser
|OpLesserEqual
|OpGreater
|OpGreaterEqual
|OpPlus
|OpMinus
|OpTimes
|OpDivide
|OpModulo (*Je crois*)
|OpAnd
|OpOr

and ins = {insCont: insCont; insLoc: loc}
and insCont =
|InsSemicolon
|InsExpr of expr
|InsDef of typ * var * insDef option
|InsIf of expr * ins
|InsIfElse of expr * ins * ins
|InsWhile of expr * ins
|InsFor of (expr list) * (expr option) * (expr list) * ins
|InsBloc of bloc
|InsCout of expr_str list
|InsReturn of expr option
and insDef =
|InsDefExpr of expr * (expr list)
|InsDefIdent of string * (expr list)

and expr_str =
|ExprStrExpr of expr
|ExprStrStr of string

and bloc = {blocCont : ins list; blocLoc: loc}
