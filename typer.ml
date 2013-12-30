(* TODO : récupérer la position dans l'erreur *)
exception Error of string
open Tast

(* On créée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
let genv = Smap.empty

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

let rec exprTyper env exp = match exp.Ast.exprCont with
  | Ast.ExprInt i -> {exprTyp=TypInt; exprCont= ExprInt i}
  | Ast.This -> assert false
  | Ast.False -> assert false (* Sucre syntaxique à virer *)
  | Ast.True -> assert false (* Sucre syntaxique à virer *)
  | Ast.Null -> {exprTyp=TypNull; exprCont=Null}
  | Ast.ExprArrow (e, s) -> assert false (* Sucre syntaxique à virer *)
  | Ast.ExprEqual (e1, e2) -> assert false
  | Ast.ExprApply (e, el) -> assert false
  | Ast.ExprNew (s, el) -> assert false
  | Ast.ExprLIncr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprLIncr ne}
    | _ -> raise (Error "Pas une valeur gauche")
    end
  | Ast.ExprLDecr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprLDecr ne}
    | _ -> raise (Error "Pas une valeur gauche")
    end
  | Ast.ExprRIncr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRIncr ne}
    | _ -> raise (Error "Pas une valeur gauche")
    end
  | Ast.ExprRDecr e ->
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRDecr ne}
    | _ -> raise (Error "Pas une valeur gauche")
    end
  | Ast.ExprAmpersand e -> assert false
  | Ast.ExprExclamation e -> assert false
  | Ast.ExprMinus e -> assert false
  | Ast.ExprPlus e -> assert false
  | Ast.ExprOp (e1, o, e2) -> 
    let ne1 = exprTyper env e1 and ne2 = exprTyper env e2 in
    begin match (ne1.exprTyp, ne2.exprTyp) with
    | TypInt, TypInt -> 
      {exprTyp = TypInt; 
       exprCont = ExprOp (exprTyper env e1, opTyper o, exprTyper env e2)}
    | _ -> raise (Error "Type int attendu")
    end
  | Ast.ExprParenthesis e -> 
    exprTyper env e
  | e -> exprLVTyper env exp

(* Pour les valeurs gauches *)
and exprLVTyper env exp = match exp.Ast.exprCont with
  | Ast.ExprQident q -> assert false
  | Ast.ExprDot (e, s) -> assert false
  | Ast.ExprStar e -> 
    let ne = exprTyper env e in
    begin match ne.exprTyp with
    | TypPointer t -> {exprTyp=t; exprCont=ExprStar ne}
    | _ -> raise (Error "Pas un pointeur")
    end
  | _ -> assert false

let expr_strTyper env = function
  | Ast.ExprStrExpr e -> ExprStrExpr (exprTyper env e)
  | Ast.ExprStrStr s -> ExprStrStr s

let rec insTyper env = function
  | [] -> []
  | ins::insl -> 
    let  newEnv, tIns =  match ins.Ast.insCont with
      | Ast.InsSemicolon -> (env, InsSemicolon)
      | Ast.InsExpr e -> (env, InsExpr (exprTyper env e))
      | Ast.InsDef (typ, var, insDef) -> assert false
      | Ast.InsIf (e, ins) -> assert false
      | Ast.InsIfElse (e, i1, i2) -> assert false
      | Ast.InsWhile (e, i) -> assert false
      | Ast.InsFor (el1, eopt, el2, i) -> assert false
      | Ast.InsBloc b -> assert false
      | Ast.InsCout s -> 
	(env, InsCout (List.map (expr_strTyper env) s))
      | Ast.InsReturn eopt -> assert false
    in
    tIns::(insTyper newEnv insl)

(* t est un type, d une déclaration.*)
let declTyper = function
  | Ast.DeclVars v -> assert false
  | Ast.DeclClass c -> assert false

  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    (*Dans un monde merveilleux, le contexte env renvoie le contexte
      global ajouté aux types de tous les paramètres, ainsi que this si
      nécessaire*)
    let env, argList = argumentTyper p.Ast.argumentList in
    ProtoBloc 
      ( {
	  protoVar = protoVarTTyper p.Ast.protoVar ;
	  argumentList = argList;
        },
	insTyper env b.Ast.blocCont;
      )

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
