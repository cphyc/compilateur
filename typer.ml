exception Error of string * Ast.loc
open Tast

(* On crée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
(* Environnement global : à chaque variable un type *)
let genv: (string, typ) Hashtbl.t = Hashtbl.create 42

let classInheritances: (string, string list) Hashtbl.t = Hashtbl.create 17
let classFields: (string, string) Hashtbl.t = Hashtbl.create 17
let classCons: (string, typ list) Hashtbl.t = Hashtbl.create 17

let methodsTable: (string * string, typ list) Hashtbl.t = Hashtbl.create 17
let functionsTable: (string, typ list) Hashtbl.t = Hashtbl.create 17

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

(* Vérifie qu'un type est bien formé *)
let rec typBF = function
  | TypInt -> true
  | TypIdent s -> Hashtbl.mem classInheritances s
  | TypPointer p -> typBF p
  | _ -> false

(* Distingue méthode et fonction et renvoie la classe *)
let rec classOfMethod q0 = match q0.Ast.qvarCont with
  | Ast.QvarQident (Ast.Ident s) -> ""
  | Ast.QvarQident (Ast.IdentIdent (s1, s2)) -> s1
  | Ast.QvarPointer q -> classOfMethod q
  | Ast.QvarReference q -> classOfMethod q

let rec typEq t1 t2 = match t1, t2 with
  | TypNull, TypNull | TypVoid, TypVoid | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> s1 == s2
  | TypPointer nt1, TypPointer nt2 -> typEq nt1 nt2
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
      raise (Error ("Redéfinition illégale de la variable \""^s^"\".", var.Ast.varLoc))
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

let rec argumentTyper lenv = function
  | [] -> lenv, []
  | arg::alist -> 
    let v = varTyper arg.Ast.argumentTyp.Ast.typCont  arg.Ast.argumentVar in
    let nlenv, vlist = (argumentTyper lenv alist) in
    varUnique arg.Ast.argumentLoc v vlist;
    (Smap.add v.varIdent v.varTyp nlenv), (v::vlist)

(* Cette fonction verifie qu'on n'a pas redondance de variable *)
and varUnique loc v0 = function
  | [] -> ()
  | v::alist -> if v0.varIdent == v.varIdent then
      raise (Error ("Variable redondante", loc))
    else varUnique loc v0 alist

(* Pour convertir les membres d'une déclaration de classe  *)
let memberConverter s = function
  | Ast.MemberDeclVars dv ->
    let la = (List.map (varTyper dv.Ast.declVarsTyp.Ast.typCont) 
		dv.Ast.varList) in
    let aux var = 
      if not (typBF var.varTyp) 
      then raise (Error ("mal formé", dv.Ast.declVarsLoc));
      if var.varRef
      then raise (Error ("les champs ne peuvent pas être des références", 
		  dv.Ast.declVarsLoc));
      let v = var.varIdent in
      let l = Hashtbl.find_all classFields s in
      if List.mem v l 
      then raise (Error ("already defined", dv.Ast.declVarsLoc))
      else Hashtbl.add classFields s v
    in
    List.iter aux la;
    MemberDeclVars la
  | Ast.VirtualProto (b,p) ->     
    let _, argList = argumentTyper Smap.empty p.Ast.argumentList in
    let l = Hashtbl.find_all classCons s in
    let lt = List.map (fun a -> a.varTyp) argList in
    let profEq p1 p2 = 
      (List.length p1 == List.length p2)
     && List.for_all2 typEq p1 p2 in
    if List.exists (profEq lt) l
    then raise (Error ("the same profile already exists", p.Ast.protoLoc))
    else
      VirtualProto (b, {protoVar = protoVarTTyper p.Ast.protoVar ;
			argumentList = argList})

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

(* =========================TYPAGE DES EXPRESSIONS=========================== *)


let rec exprTyper lenv exp = match exp.Ast.exprCont with
  | Ast.ExprInt i -> { exprTyp=TypInt; exprCont= ExprInt i }
  | Ast.This -> assert false
  | Ast.False -> { exprTyp=TypInt; exprCont= ExprInt 0}
  | Ast.True -> { exprTyp=TypInt; exprCont= ExprInt 1}
  | Ast.Null -> { exprTyp=TypNull; exprCont=Null }
  | Ast.ExprArrow (e, s) -> 
    exprLVTyper lenv {Ast.exprLoc=exp.Ast.exprLoc;
		      Ast.exprCont = Ast.ExprDot 
	({Ast.exprCont=Ast.ExprStar e; Ast.exprLoc=exp.Ast.exprLoc},s)
		     } 
  | Ast.ExprEqual (e1, e2) -> 
    let el, er = exprLVTyper lenv e1, exprTyper lenv e2 in
    if not (typIn er.exprTyp el.exprTyp) then
      raise (Error ("Types incompatibles.", exp.Ast.exprLoc))
    else if not (typNum er.exprTyp) then
      raise (Error ("Type numérique attendu.", e2.Ast.exprLoc))
    else
      { exprTyp = el.exprTyp; exprCont= ExprEqual (el, er) }
  | Ast.ExprApply (e, el) -> assert false
  | Ast.ExprNew (s, el) -> assert false
  | Ast.ExprLIncr e -> 
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypInt -> { exprTyp=TypInt; exprCont= ExprLIncr ne }
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprLDecr e -> 
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypInt -> { exprTyp=TypInt; exprCont= ExprLDecr ne }
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprRIncr e -> 
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRIncr ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprRDecr e ->
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont= ExprRDecr ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprAmpersand e -> 
    let ne = exprLVTyper lenv e in
    {exprTyp=TypPointer ne.exprTyp; exprCont= ExprAmpersand ne}
  | Ast.ExprExclamation e -> 
    let ne = exprTyper lenv e in
    begin match ne.exprTyp with
    | TypInt -> {exprTyp=TypInt; exprCont = ExprExclamation ne}
    | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
    end
  | Ast.ExprMinus e ->
    let ne = exprTyper lenv e in
     begin match ne.exprTyp with
     | TypInt -> {exprTyp=TypInt; exprCont = ExprMinus ne}
     | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
     end
  | Ast.ExprPlus e ->     
    let ne = exprTyper lenv e in
     begin match ne.exprTyp with
     | TypInt -> {exprTyp=TypInt; exprCont = ExprPlus ne}
     | _ -> raise (Error ("Pas un type int", exp.Ast.exprLoc))
     end
  | Ast.ExprOp (e1, o, e2) -> 
    (* Attention, il faut pouvoir comparer les pointeurs : pour == et !=, on 
    doit vérifier des types num, pas int !!*)
    let ne1 = exprTyper lenv e1 and ne2 = exprTyper lenv e2 in
    begin match (ne1.exprTyp, ne2.exprTyp) with
    | TypInt, TypInt -> 
      {exprTyp = TypInt; 
       exprCont = ExprOp (exprTyper lenv e1, opTyper o, exprTyper lenv e2)}
    | _ -> raise (Error ("Type int attendu", exp.Ast.exprLoc))
    end
  | Ast.ExprParenthesis e -> 
    exprTyper lenv e
  | _ -> exprLVTyper lenv exp

(* Pour les valeurs gauches *)
and exprLVTyper lenv exp = match exp.Ast.exprCont with
  | Ast.ExprQident Ast.Ident s ->
    (* On a un identificateur, on cherche son type dans l'env local puis gal *)
    let ttyp = 
      try Smap.find s lenv
      with
	Not_found -> try Hashtbl.find genv s 
	  with Not_found ->
	    raise 
	      (Error ("Variable \""^s^"\" non déclarée", exp.Ast.exprLoc))
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



(* =========================TYPAGE DES INSTRUCTIONS========================== *)

let rec insTyper lenv ins = match ins.Ast.insCont with
  | Ast.InsSemicolon -> (lenv, InsSemicolon)
  | Ast.InsExpr e -> (lenv, InsExpr (exprTyper lenv e))
  | Ast.InsDef (typ, var, insDef) ->
	(* On commence par vérifier que la variable n'a pas été déclarée,
	   puis on type l'instruction de définition *)
    prevent_redeclaration lenv var;
    begin
      let tvar = varTyper typ.Ast.typCont var in
      match insDef with
      | Some Ast.InsDefExpr e -> 
	let te = exprTyper lenv e in
	if typIn te.exprTyp tvar.varTyp then
	  (Smap.add tvar.varIdent tvar.varTyp lenv),
	  InsDef (tvar, Some (InsDefExpr te))
	else
	  raise (Error ("Types incompatibles.", ins.Ast.insLoc))
      | Some Ast.InsDefIdent (s, elist) -> assert false
      | None -> ( Smap.add tvar.varIdent tvar.varTyp lenv), InsDef (tvar, None) 
    end
  | Ast.InsIf (e, i) -> 
    let _, ins = insTyper lenv i in
    lenv, InsIf (exprTyper lenv e, ins)
  | Ast.InsIfElse (e, i1, i2) -> 
    let _, ins1 = insTyper lenv i1 in
    let _, ins2 = insTyper lenv i2 in
    lenv,
    InsIfElse (exprTyper lenv e, ins1, ins2)
  | Ast.InsWhile (e, i) -> assert false
  | Ast.InsFor (el1, eopt, el2, i) -> assert false
  | Ast.InsBloc b -> 
    let insList = insListTyper lenv b.Ast.blocCont in
    (lenv, InsBloc insList)
  | Ast.InsCout s -> 
    (lenv, InsCout (List.map (expr_strTyper lenv) s))
  | Ast.InsReturn None -> (lenv, InsReturn None)
  | Ast.InsReturn (Some e) -> 
    let ne = exprTyper lenv e in
    if typEq (Smap.find "return" lenv) ne.exprTyp then
      (lenv, InsReturn (Some ne))
    else raise (Error ("wrong return type", ins.Ast.insLoc))

(* Typage des instructions.
   La fonction type au fur et à mesure les instructions en mettant à jour 
   l'environnement.
   sig : env -> Ast.ins list -> Tast.ins *)
and insListTyper lenv = function
  | [] -> []
  | ins::insl -> 
    (* Sous fonction qui récupère le nouvel lenv et l'instruction typée *)
    let nlenv, tIns = insTyper lenv ins in
    tIns::(insListTyper nlenv insl)



(* =========================TYPAGE DES DÉCLARATIONS========================== *)


let declTyper = function
  | Ast.DeclVars dv -> 
    let atyp = dv.Ast.declVarsTyp.Ast.typCont in
    let vlist = dv.Ast.varList in
    DeclVars 
      ( List.map (fun var -> let nv = varTyper atyp var in
			    Hashtbl.add genv nv.varIdent nv.varTyp;
			    nv) vlist)   
    
  | Ast.DeclClass c -> 
    let l = match c.Ast.supersOpt with
      | None -> []
      | Some l' -> l' in
    Hashtbl.add classInheritances c.Ast.className l;
    DeclClass {className = c.Ast.className; supersOpt = c.Ast.supersOpt;
     memberList = List.map (memberConverter c.Ast.className) c.Ast.memberList}

  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    (* Dans un monde merveilleux, le contexte env renvoie le contexte
       global ajouté aux types de tous les paramètres, ainsi que this si
       nécessaire*)

    let env, argList = argumentTyper Smap.empty p.Ast.argumentList in

    (* On rajoute "return" et le type de retour dans l'environnement.
    A priori, c'est sans danger vu qu'aucune variable ne s'appellera jamais
    "return", mais je sais pas si c'est une bonne idée. Ça me paraît néanmoins
    le plus simple pour transmettre le type de retour.*)

    match p.Ast.protoVar with
    | Ast.Qvar (t,q) when (classOfMethod q) == "" -> (* Fonction *)
      let t = typConverter t.Ast.typCont in
      if typNum t || (typEq t TypVoid) then
      ProtoBloc	
	( 
	  { protoVar = protoVarTTyper p.Ast.protoVar ;
	    argumentList = argList },
	  insListTyper (Smap.add "return" t env) b.Ast.blocCont;
	)
      else raise (Error ("la valeur de retour doit être numérique", 
			 p.Ast.protoLoc))
    | Ast.Qvar (t,q) -> (* Méthode *)
      let t = typConverter t.Ast.typCont in
      if typNum t || (typEq t TypVoid) then
	ProtoBloc 
	  ( 
	    { protoVar = protoVarTTyper p.Ast.protoVar ;
	      argumentList = argList },
	    insListTyper (Smap.add "this" (TypIdent (classOfMethod q)) 
			    (Smap.add "return" t env))
	      b.Ast.blocCont;
	  )
      else raise (Error ("la valeur de retour doit être numérique", 
			 p.Ast.protoLoc))
    | Ast.Tident s -> (* Constructeur *)
      ProtoBloc 
	( 
	  { protoVar = protoVarTTyper p.Ast.protoVar ;
	    argumentList = argList },
	  insListTyper (Smap.add "this" (TypIdent s) env) b.Ast.blocCont;
	)
    | Ast.TidentTident (s1, s2) -> (* Constructeur *)
      if s1==s2 then
      ProtoBloc 
	( 
	  { protoVar = protoVarTTyper p.Ast.protoVar ;
	    argumentList = argList },
	  insListTyper (Smap.add "this" (TypIdent s2) env) b.Ast.blocCont;
	)
      else raise (Error (s2^" n'est pas un constructeur", p.Ast.protoLoc))

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
