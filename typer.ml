(* TODO : récupérer la position dans l'erreur *)
exception Error of string
open Tast

(* On créée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
type env = typ Smap.t

(* Simple fonction effectuant la conversion entre Ast et Tast *)
let rec typConverter = function
  | Ast.TypVoid -> TypVoid
  | Ast.TypInt -> TypInt
  | Ast.TypIdent s -> TypIdent s

let rec qidentTyper = function
  | Ast.Ident s -> Ident s
  | Ast.IdentIdent (s1, s2) -> assert false

let rec qvarTyper = function
  | Ast.QvarQident qident -> QvarQident (qidentTyper qident)
  | Ast.QvarPointer qvar -> assert false
    (* QvarPointer (qvarTyper qvar) *)
  | Ast.QvarReference qvar -> assert false
    (* QvarReference (qvarTyper qvar) *)

let protoVarTTyper = function
  | Ast.Qvar (typ, qvar) -> Qvar (typConverter typ.Ast.typCont, qvarTyper qvar.Ast.qvarCont)
  | Ast.Tident s -> assert false
  | Ast.TidentTident (s1, s2) -> assert false

(* let varTyper v = assert false *)

let argumentTyper arg = assert false

let opTyper o = match o.Ast.opCont with 
| Ast.OpEqual -> OpEqual
| Ast.OpDiff -> OpDiff
| Ast.OpLesser -> OpLesser
| Ast.OpLesserEqual -> OpLesserEqual
| Ast.OpGreater -> OpGreater
| Ast.OpGreaterEqual -> OpGreaterEqual
| Ast.OpPlus -> OpPlus
| Ast.OpMinus -> OpMinus
| Ast.OpTimes -> OpTimes
| Ast.OpDivide -> OpDivide
| Ast.OpModulo -> OpModulo
| Ast.OpAnd -> OpAnd
| Ast.OpOr -> OpOr

let rec exprTyper e = match e.Ast.exprCont with
  | Ast.ExprInt i -> ExprInt i
  | Ast.This -> assert false
  | Ast.False -> assert false
  | Ast.True -> assert false
  | Ast.Null -> Null
  | Ast.ExprQident q -> assert false
  | Ast.ExprStar e -> assert false
  | Ast.ExprDot (e, s) -> assert false
  | Ast.ExprArrow (e, s) -> assert false
  | Ast.ExprEqual (e1, e2) -> assert false
  | Ast.ExprApply (e, el) -> assert false
  | Ast.ExprNew (s, el) -> assert false
  | Ast.ExprLIncr e -> assert false
  | Ast.ExprLDecr e -> assert false
  | Ast.ExprRIncr e -> assert false
  | Ast.ExprRDecr e -> assert false
  | Ast.ExprAmpersand e -> assert false
  | Ast.ExprExclamation e -> assert false
  | Ast.ExprMinus e -> assert false
  | Ast.ExprPlus e -> assert false
  | Ast.ExprOp (e1, o, e2) -> ExprOp (exprTyper e1, opTyper o, exprTyper e2)
  | Ast.ExprParenthesis e -> 
    ExprParenthesis (exprTyper e)

let expr_strTyper = function
  | Ast.ExprStrExpr e -> ExprStrExpr (exprTyper e)
  | Ast.ExprStrStr s -> ExprStrStr s

let insTyper ins = match ins.Ast.insCont with
  | Ast.InsSemicolon -> assert false
  | Ast.InsExpr e -> assert false
  | Ast.InsDef (typ, var, insDef) -> assert false
  | Ast.InsIf (e, ins) -> assert false
  | Ast.InsIfElse (e, i1, i2) -> assert false
  | Ast.InsWhile (e, i) -> assert false
  | Ast.InsFor (el1, eopt, el2, i) -> assert false
  | Ast.InsBloc b -> assert false
  | Ast.InsCout s -> 
    InsCout (List.map expr_strTyper s)
  | Ast.InsReturn eopt -> assert false

(* t est un type, d une déclaration.*)
let declTyper = function
  | Ast.DeclVars v -> assert false
  | Ast.DeclClass c -> assert false

  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    ProtoBloc 
      ( {
	  protoVar = protoVarTTyper p.Ast.protoVar ;
	  argumentList = List.map argumentTyper p.Ast.argumentList;
        },
	List.map insTyper b.Ast.blocCont;
      )

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
