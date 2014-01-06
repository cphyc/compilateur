exception Error of string * Ast.loc
open Tast

(* On crée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
let genv:(typ Smap.t ref) = ref Smap.empty

(* Simple fonction effectuant la conversion entre Ast et Tast *)
let rec typConverter = function
  | Ast.TypVoid -> TypVoid
  | Ast.TypInt -> TypInt
  | Ast.TypIdent s -> TypIdent s

(* vérifie que t1 est un sous-type de t2 *)
let rec typIn t1 t2 = match t1, t2 with
  | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> assert false
  | TypNull, TypPointer _ -> true
  | TypPointer t1', TypPointer t2' -> typIn t1' t2'
  | _ -> false

(* Type numérique : int, pointeur a.k.a typenull *)
let typNum = function
  | TypInt | TypPointer _ | TypNull -> true
  | _ -> false

let rec qidentTyper = function
  | Ast.Ident s -> Ident s
  | Ast.IdentIdent (s1, s2) -> assert false

let rec qvarTyper = function
  | Ast.QvarQident qident -> QvarQident (qidentTyper qident)
  | Ast.QvarPointer qvar -> assert false
    (* QvarPointer (qvarTyper qvar) *)
  | Ast.QvarReference qvar -> assert false
    (* QvarReference (qvarTyper qvar) *)

let rec prevent_redeclaration env var = match var.Ast.varCont with
  | Ast.VarIdent s -> 
    if Smap.mem s env then
      raise (Error ("Redéfinition illégale de la variable "^s^".", var.Ast.varLoc))
    else ()
  | Ast.VarPointer v | Ast.VarReference v -> prevent_redeclaration env v
  
let protoVarTTyper = function
  | Ast.Qvar (typ, qvar) -> Qvar (typConverter typ.Ast.typCont, qvarTyper qvar.Ast.qvarCont)
  | Ast.Tident s -> assert false
  | Ast.TidentTident (s1, s2) -> assert false

let rec varTyper typ v0 = match v0.Ast.varCont with
  | Ast.VarIdent s -> { varIdent=s; varRef=false; varTyp=typConverter  typ}
  | Ast.VarPointer v -> 
    let nv = varTyper typ v in
    { varIdent=nv.varIdent; varRef=false; 
      varTyp=TypPointer nv.varTyp }
  | Ast.VarReference v ->
    let nv = varTyper typ v in
    if nv.varRef (* On n'autorise pas les dbl refs *)
    then raise (Error("Double référence", v0.Ast.varLoc))
    else 
      { varIdent=nv.varIdent; varRef=true;
	varTyp=nv.varTyp }

let rec argumentTyper env = function
  | [] -> env, []
  | arg::alist -> 
    let v = varTyper arg.Ast.argumentTyp.Ast.typCont  arg.Ast.argumentVar in
    let nenv, vlist = (argumentTyper env alist) in
    varUnique arg.Ast.argumentLoc v vlist;
    (Smap.add v.varIdent v.varTyp nenv), (v::vlist)

(* Cette fonction verifie qu'on n'a pas redondance de variable *)
and varUnique loc v0 = function
  | [] -> ()
  | v::alist -> if v0.varIdent == v.varIdent then
      raise (Error ("Variable redondante", loc))
    else varUnique loc v0 alist

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
  | Ast.ExprInt i -> { exprTyp=TypInt; exprCont= ExprInt i }
  | Ast.This -> assert false
  | Ast.False -> { exprTyp=TypInt; exprCont= ExprInt 0}
  | Ast.True -> { exprTyp=TypInt; exprCont= ExprInt 1}
  | Ast.Null -> { exprTyp=TypNull; exprCont=Null }
  | Ast.ExprArrow (e, s) -> assert false (* Sucre syntaxique à virer *)
  | Ast.ExprEqual (e1, e2) -> 
    let el, er = exprLVTyper env e1, exprTyper env e2 in
    if not (typIn er.exprTyp el.exprTyp) then
      raise (Error ("Types incompatibles.", exp.Ast.exprLoc))
    else if not (typNum er.exprTyp) then
      raise (Error ("Type numérique attendu.", e2.Ast.exprLoc))
    else
      { exprTyp = el.exprTyp; exprCont= ExprEqual (el, er) }
  | Ast.ExprApply (e, el) -> assert false
  | Ast.ExprNew (s, el) -> assert false
  | Ast.ExprLIncr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> { exprTyp=TypInt; exprCont= ExprLIncr ne }
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprLDecr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> { exprTyp=TypInt; exprCont= ExprLDecr ne }
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprRIncr e -> 
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRIncr ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprRDecr e ->
    let ne = exprLVTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRDecr ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprAmpersand e -> assert false
  | Ast.ExprExclamation e -> 
    let ne = exprTyper env e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont = ExprExclamation ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprMinus e ->
    let ne = exprTyper env e in
     begin match ne.exprTyp with
     | TypInt -> {exprTyp=TypInt; exprCont = ExprMinus ne}
     | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
     end
  | Ast.ExprPlus e ->     
    let ne = exprTyper env e in
     begin match ne.exprTyp with
     | TypInt -> {exprTyp=TypInt; exprCont = ExprPlus ne}
     | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
     end
  | Ast.ExprOp (e1, o, e2) -> 
    let ne1 = exprTyper env e1 and ne2 = exprTyper env e2 in
    begin match (ne1.exprTyp, ne2.exprTyp) with
    | TypInt, TypInt -> 
      {exprTyp = TypInt; 
       exprCont = ExprOp (exprTyper env e1, opTyper o, exprTyper env e2)}
    | _ -> raise (Error ("Type int attendu", exp.Ast.exprLoc))
    end
  | Ast.ExprParenthesis e -> 
    exprTyper env e
  | e -> exprLVTyper env exp

(* Pour les valeurs gauches *)
and exprLVTyper lenv exp = match exp.Ast.exprCont with
  | Ast.ExprQident Ast.Ident s ->
    (* On a un identificateur, on cherche son type dans l'env local puis gal *)
    let ttyp = 
      try Smap.find s lenv
      with
	Not_found -> try Smap.find s !genv
	  with Not_found ->
	    raise 
	      (Error ((String.concat "" ["Variable \"";s;"\" non déclarée"]),
		      exp.Ast.exprLoc))
    in
    { exprTyp = ttyp; exprCont = ExprQident (Ident s) }
  | Ast.ExprQident Ast.IdentIdent (s1, s2) -> assert false
  | Ast.ExprDot (e, s) -> assert false
  | Ast.ExprStar e -> 
    let ne = exprTyper lenv e in
    begin match ne.exprTyp with
    | TypPointer t -> { exprTyp=t; exprCont=ExprStar ne }
    | _ -> raise (Error ("Pas un pointeur", exp.Ast.exprLoc))
    end
  | _ -> raise (Error ("Pas une valeur gauche", exp.Ast.exprLoc))

let expr_strTyper env = function
  | Ast.ExprStrExpr e -> ExprStrExpr (exprTyper env e)
  | Ast.ExprStrStr s -> ExprStrStr s

(* Typage des instructions.
   La fonction type au fur et à mesure les instructions en mettant à jour 
   l'environnement.
   sig : env -> Ast.ins list -> Tast.ins *)
let rec insListTyper env = function
  | [] -> []
  | ins::insl -> 
    (* Sous fonction qui récupère le nouvel env et l'instruction typée *)
    let newEnv, tIns = match ins.Ast.insCont with
      | Ast.InsSemicolon -> (env, InsSemicolon)
      | Ast.InsExpr e -> (env, InsExpr (exprTyper env e))
      | Ast.InsDef (typ, var, insDef) ->
	(* On commence par vérifier que la variable n'a pas été déclarée,
	   puis on type l'instruction de définition *)
	prevent_redeclaration env var;
	begin
	  let ttyp = typConverter typ.Ast.typCont in
	  let tvar = varTyper typ.Ast.typCont var in
	  match insDef with
	  | Some Ast.InsDefExpr e -> 
	    let te = exprTyper env e in
	    if te.exprTyp != ttyp then
	      raise (Error ("Types incompatibles.", ins.Ast.insLoc))
	    else
	      (Smap.add tvar.varIdent ttyp env),
	      InsDef (ttyp, tvar, Some (InsDefExpr te))
	  | Some Ast.InsDefIdent (s, elist) -> assert false
	  | None -> ( Smap.add tvar.varIdent ttyp env), InsDef (ttyp, tvar, None) 
	end
      | Ast.InsIf (e, ins) -> assert false
      | Ast.InsIfElse (e, i1, i2) -> assert false
      | Ast.InsWhile (e, i) -> assert false
      | Ast.InsFor (el1, eopt, el2, i) -> assert false
      | Ast.InsBloc b -> 
	let insList = insListTyper env b.Ast.blocCont in
	(env, InsBloc insList)
      | Ast.InsCout s -> 
	(env, InsCout (List.map (expr_strTyper env) s))
      | Ast.InsReturn eopt -> assert false
    in
    tIns::(insListTyper newEnv insl)

let declTyper = function
  | Ast.DeclVars dv -> 
    let atyp = dv.Ast.declVarsTyp.Ast.typCont in
    (* let ttyp = typConverter atyp in *)
    let vlist = dv.Ast.varList in
    DeclVars (List.map 
		(fun var -> (* genv := Smap.add var ttyp !genv; *)
		  varTyper atyp var) vlist)   
    
  | Ast.DeclClass c -> assert false
  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    (* Dans un monde merveilleux, le contexte env renvoie le contexte
       global ajouté aux types de tous les paramètres, ainsi que this si
       nécessaire*)
    let env, argList = argumentTyper !genv p.Ast.argumentList in
    ProtoBloc 
      ( 
	{ protoVar = protoVarTTyper p.Ast.protoVar ;
	  argumentList = argList; },
	insListTyper env b.Ast.blocCont;
      )

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
