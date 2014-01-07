exception Error of string * Ast.loc
open Tast

(* On crée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
(* Environnement global : à chaque variable un type *)
let genv: (string, typ) Hashtbl.t = Hashtbl.create 42

let classInheritances: (string, string) Hashtbl.t = Hashtbl.create 17
let classFields: (string, string * typ) Hashtbl.t = Hashtbl.create 17
let classCons: (string, typ list) Hashtbl.t = Hashtbl.create 17

let methodsTable: (string * string, typ * typ list) Hashtbl.t = Hashtbl.create 7
(* associe à une fonction son type et sa signature *)
let functionsTable: (string, typ * typ list) Hashtbl.t = Hashtbl.create 17

(* Simple fonction effectuant la conversion entre Ast et Tast *)
let rec typConverter = function
  | Ast.TypVoid -> TypVoid
  | Ast.TypInt -> TypInt
  | Ast.TypIdent s -> TypIdent s

(* Teste si c1 est une sous-classe de c2 *)
let rec subClass c1 c2 = 
  (c1 == c2) 
  || (List.exists (fun c -> (subClass c c2)) 
	(Hashtbl.find_all classInheritances c1))

(* vérifie que t1 est un sous-type de t2 *)
let rec typIn t1 t2 = match t1, t2 with
  | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> subClass s1 s2
  | TypNull, TypPointer _ -> true
  | TypPointer (TypIdent s1), TypPointer (TypIdent s2) -> subClass s1 s2
  | _ -> false

(* Teste l'égalité de deux types *)
let rec typEq t1 t2 = match t1, t2 with
  | TypNull, TypNull | TypVoid, TypVoid | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> s1 == s2
  | TypPointer nt1, TypPointer nt2 -> typEq nt1 nt2
  | _ -> false

(* teste si l1 est un plus petit profil que l2 *)
let rec leqProf l1 l2 = match l1, l2 with
  | [], [] -> true
  | t1::q1, t2::q2 -> (typIn t1 t2) && leqProf q1 q2
  | _ -> false

(* teste l egalite entre deux profils *)
let rec eqProf l1 l2 = match l1, l2 with
  | [], [] -> true
  | t1::q1, t2::q2 -> (typEq t1 t2) && eqProf q1 q2
  | _ -> false

(* Donne la liste des profils plus grands que *)
let rec geqListProf p0 = function
  | [] -> []
  | p::l -> if leqProf p0 p then p::(geqListProf p0 l) else geqListProf p0 l

(* Donne la liste des profils minima *)
let minProf l0 =
  (* min donne l'un des minima de l0 relatifs à p0 *)
  let rec min p0 = function
    | [] -> p0
    | p::l -> let p' = min p0 l in
	      if (not (eqProf p p')) && (leqProf p p') 
	      then p else p' 
  in
  let rec minEns = function
    | [] -> []
    | p::l -> let l' = minEns l in
	      let m = min p l0 in
	      if List.exists (eqProf m) l' then l' else m::l'
  in
  minEns l0

(* renvoie le type d'un champ f pour la classe c et ses surclasses*)
let fieldType l c f =
  let rec aux c' = 
    if (List.mem_assoc f (Hashtbl.find_all classFields c')) then
      [(List.assoc f (Hashtbl.find_all classFields c'))]
      @ List.concat (List.map aux (Hashtbl.find_all classInheritances c'))
    else
      List.concat (List.map aux (Hashtbl.find_all classInheritances c'))
  in
  match aux c with
  | [] -> raise (Error ("ce champ n'existe pas", l))
  | [t] -> t
  | _ -> raise (Error ("ce champ est ambigu", l))


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

(* Distingue méthode et fonction et renvoie la classe en option *)
let rec classOfMethod q0 = match q0.Ast.qvarCont with
  | Ast.QvarQident (Ast.Ident s) -> Some s, None
  | Ast.QvarQident (Ast.IdentIdent (s1, s2)) -> Some s1, Some s2
  | Ast.QvarPointer q -> classOfMethod q
  | Ast.QvarReference q -> classOfMethod q
    
let rec qidentTyper = function
  | Ast.Ident s -> Ident s
  | Ast.IdentIdent (s1, s2) -> IdentIdent (s1, s2)

let rec qvarTyper = function
  | Ast.QvarQident qident -> QvarQident (qidentTyper qident)
  | Ast.QvarPointer qvar -> QvarPointer (qvarTyper qvar.Ast.qvarCont)
  | Ast.QvarReference qvar -> QvarReference (qvarTyper qvar.Ast.qvarCont)

let rec prevent_redeclaration env var = match var.Ast.varCont with
  | Ast.VarIdent s -> 
    if Smap.mem s env then
      raise (Error ("Redéfinition illégale de la variable \""^s^"\".", 
		    var.Ast.varLoc))
    else ()
  | Ast.VarPointer v | Ast.VarReference v -> prevent_redeclaration env v
  
let protoVarTTyper = function
  | Ast.Qvar (typ, qvar) -> 
    Qvar (typConverter typ.Ast.typCont, qvarTyper qvar.Ast.qvarCont)
  | Ast.Tident s -> Tident s
  | Ast.TidentTident (s1, s2) -> TidentTident (s1, s2)

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
	varTyp= nv.varTyp }

let rec argumentTyper lenv = function
  | [] -> lenv, [], []
  | arg::alist -> 
    let v = varTyper arg.Ast.argumentTyp.Ast.typCont  arg.Ast.argumentVar in
    let t = v.varTyp in
    let nlenv, vlist, tlist = (argumentTyper lenv alist) in
    varUnique arg.Ast.argumentLoc v vlist;
    (Smap.add v.varIdent v.varTyp nlenv), (v::vlist), (t::tlist)

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
      if List.mem_assoc v l 
      then raise (Error ("already defined", dv.Ast.declVarsLoc))
      else Hashtbl.add classFields s (v, var.varTyp)
    in
    List.iter aux la;
    MemberDeclVars la
  | Ast.VirtualProto (b,p) ->     
    let _, argList, _ = argumentTyper Smap.empty p.Ast.argumentList in
    let l = Hashtbl.find_all classCons s in
    let lt = List.map (fun a -> a.varTyp) argList in
    let profEq p1 p2 = 
      (List.length p1 == List.length p2) && List.for_all2 typEq p1 p2 in
    if List.exists (profEq lt) l
    then raise (Error ("ce profil a déjà été déclaré", p.Ast.protoLoc))
    else
      VirtualProto (b, {protoVar = protoVarTTyper p.Ast.protoVar ;
			argumentList = argList;protoKind = Class })


(* =========================TYPAGE DES EXPRESSIONS=========================== *)


let rec exprTyper lenv exp = match exp.Ast.exprCont with
  | Ast.ExprInt i -> { exprTyp=TypInt; exprCont= ExprInt i }
  | Ast.This -> 
    begin
    try {exprTyp = Smap.find "this" lenv ;  exprCont = This}
    with Not_found -> raise (Error("pas de this dans une fonction",
				   exp.Ast.exprLoc))
    end
  | Ast.False -> { exprTyp=TypInt; exprCont= ExprInt 0}
  | Ast.True -> { exprTyp=TypInt; exprCont= ExprInt 1}
  | Ast.Null -> { exprTyp=TypNull; exprCont=Null } 
  | Ast.ExprArrow (e, s) -> 
    exprTyper lenv 
      { Ast.exprLoc=exp.Ast.exprLoc;
	Ast.exprCont = Ast.ExprDot 
	  ( {Ast.exprCont=Ast.ExprStar e; Ast.exprLoc=exp.Ast.exprLoc}, s)
      }
  | Ast.ExprEqual (e1, e2) -> 
    let el, er = exprLVTyper lenv e1, exprTyper lenv e2 in
    if not (typIn er.exprTyp el.exprTyp) then
      raise (Error ("Types incompatibles.", exp.Ast.exprLoc))
    else if not (typNum er.exprTyp) then
      raise (Error ("Type numérique attendu.", e2.Ast.exprLoc))
    else
      { exprTyp = el.exprTyp; exprCont= ExprEqual (el, er) }
  | Ast.ExprApply (e, el) -> 
    let ne = exprLVTyper lenv e and nel = List.map (exprTyper lenv) el in
    begin
      match ne.exprCont with
      | ExprQident (Ident s) -> (* Fonction *)
	(* when (Hashtbl.mem functionsTable s) *)
	let lprof = snd (List.split (Hashtbl.find_all functionsTable s)) in
	let p = List.map (fun e -> e.exprTyp) nel in
	begin
	  match minProf (geqListProf p lprof) with
	  | [] -> raise (Error ("no profile corresponds", e.Ast.exprLoc))
	  | [p] -> { exprTyp = ne.exprTyp;  exprCont = ExprApply (ne, nel)}
	  | _ -> raise (Error ("several profiles correspond", e.Ast.exprLoc))
	end
      | _ -> assert false
    end
  | Ast.ExprNew (s, el) -> 
    let nel = List.map (exprTyper lenv) el in
    let lprof = Hashtbl.find_all classCons s in
    let p = List.map (fun e -> e.exprTyp) nel in
    begin
      match minProf (geqListProf p lprof) with
      | [] -> raise (Error ("no profile corresponds2", exp.Ast.exprLoc))
      | [p] -> { exprTyp = TypPointer (TypIdent s);  
		 exprCont = ExprNew (s, nel)}
      | _ -> raise (Error ("several profiles correspond", exp.Ast.exprLoc))
    end    
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
    let ne1 = exprTyper lenv e1 and ne2 = exprTyper lenv e2 in
    begin
      let o, tint = match o.Ast.opCont with 
	| Ast.OpEqual -> OpEqual, false
	| Ast.OpDiff -> OpDiff, false
	| Ast.OpLesser -> OpLesser, true
	| Ast.OpLesserEqual -> OpLesserEqual, true
	| Ast.OpGreater -> OpGreater, true
	| Ast.OpGreaterEqual -> OpGreaterEqual, true
	| Ast.OpPlus -> OpPlus, true
	| Ast.OpMinus -> OpMinus, true
	| Ast.OpTimes -> OpTimes, true
	| Ast.OpDivide -> OpDivide, true
	| Ast.OpModulo -> OpModulo, true
	| Ast.OpAnd -> OpAnd, true
	| Ast.OpOr -> OpOr, true 
      in
      if tint then 
	match (ne1.exprTyp, ne2.exprTyp) with
	| TypInt, TypInt -> 
	  {exprTyp = TypInt; 
	   exprCont = ExprOp (exprTyper lenv e1, o, exprTyper lenv e2)}
	| _ -> raise (Error ("Type int attendu", exp.Ast.exprLoc))
      else 
	(*À mon avis, il y a une faute dans le sujet, parce qu'on doit pouvoir
	  comparer un type pointeur au type null.*)
	if (typNum ne1.exprTyp) && (typNum ne2.exprTyp) then
	  {exprTyp = TypInt; 
	   exprCont = ExprOp (exprTyper lenv e1, o, exprTyper lenv e2)}
	else raise (Error ("Type numerique attendu", exp.Ast.exprLoc))
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
	    try fst (Hashtbl.find functionsTable s)
	    with Not_found -> 
	      raise 
		(Error ("Variable \""^s^"\" non déclarée", exp.Ast.exprLoc))
    in
    { exprTyp = ttyp; exprCont = ExprQident (Ident s) }
  | Ast.ExprQident Ast.IdentIdent (s1, s2) -> assert false
  | Ast.ExprDot (e, s) ->    
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypIdent c ->  {exprTyp = fieldType e.Ast.exprLoc c s; 
		      exprCont = ExprDot (ne, s)}
    | _ -> raise (Error ("n'est pas un constructeur", exp.Ast.exprLoc))
    end
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
	  if typBF tvar.varTyp then
	  (Smap.add tvar.varIdent tvar.varTyp lenv),
	  InsDef (tvar, Some (InsDefExpr te))
	  else raise (Error ("Type mal forme", ins.Ast.insLoc))
	else
	  raise (Error ("Types incompatibles.", ins.Ast.insLoc))
      | Some Ast.InsDefIdent (s, elist) -> assert false
      | None -> ( Smap.add tvar.varIdent tvar.varTyp lenv), InsDef (tvar, None) 
    end
  | Ast.InsIf (e, i) -> 
    let _, ni = insTyper lenv i in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv, InsIf (ne, ni)
  | Ast.InsIfElse (e, i1, i2) -> 
    let _, ins1 = insTyper lenv i1 in
    let _, ins2 = insTyper lenv i2 in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv,
    InsIfElse (exprTyper lenv e, ins1, ins2)
  | Ast.InsWhile (e, i) -> 
    let _, ni = insTyper lenv i in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv, InsWhile (ne, ni)
  | Ast.InsFor (el1, eopt, el2, i) -> 
    let nel1 = List.map (exprTyper lenv) el1 in
    let nel2 = List.map (exprTyper lenv) el2 in
    let _, ni = insTyper lenv i in
    let neopt = match eopt with 
      |None -> {exprTyp = TypInt; exprCont = ExprInt 1 (* = True *)}
      |Some e -> let ne = exprTyper lenv e in
		 if not (typEq ne.exprTyp TypInt) then
		   raise (Error ("Pas un type int comme condition", 
				 ins.Ast.insLoc));
		 ne
    in
    lenv, InsFor (nel1, neopt, nel2, ni)
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
			     	if Hashtbl.mem functionsTable nv.varIdent 
	then raise (Error ("Variable déjà utilisée",dv.Ast.declVarsLoc));
			    Hashtbl.add genv nv.varIdent nv.varTyp;
			    nv) vlist)   
    
  | Ast.DeclClass c -> 
    let l = match c.Ast.supersOpt with
      | None -> [""]
      | Some l' -> ""::l' in
    List.iter (Hashtbl.add classInheritances c.Ast.className) l;
    let memberList = List.map (memberConverter c.Ast.className) 
      c.Ast.memberList in
    if (List.for_all 
	  (function 
	  |VirtualProto (b,p) -> 
	    (match p.protoKind with |Cons _ -> false |_ -> true)
	  |_ -> true)
	  memberList)
    then Hashtbl.add classCons c.Ast.className [];
    DeclClass 
      { className = c.Ast.className; supersOpt = c.Ast.supersOpt;
	memberList = memberList}

  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    (* Dans un monde merveilleux, le contexte env renvoie le contexte
       global ajouté aux types de tous les paramètres, ainsi que this si
       nécessaire*)

    let env, argList, typList = argumentTyper Smap.empty p.Ast.argumentList in

    (* On ajoute la valeur de retour dans l'environnement local ce qui permet
       d'avoir son type, mais on renvoie via $v0 *)

    match p.Ast.protoVar with
    | Ast.Qvar (t,q) -> begin
      let t = typConverter t.Ast.typCont in
      let var = protoVarTTyper p.Ast.protoVar in
      (* On distingue les methodes des classes de fonctions *)
      match classOfMethod q with
      | Some s, None -> (* Fonctions *)
	if Hashtbl.mem functionsTable s 
	then raise (Error ("Fonction déjà définie",p.Ast.protoLoc));
	if Hashtbl.mem  genv s
	then raise (Error ("Nom déjà utilisé",p.Ast.protoLoc)); 
	(* On ajoute la fonction à la liste des fonctions *)
	Hashtbl.add functionsTable s (t, typList);
	if typNum t || (typEq t TypVoid) then
	  ProtoBloc	
	    ( 
	      { protoVar = var;
		argumentList = argList ; 
		protoKind = Function },
	      insListTyper (Smap.add "return" t env) b.Ast.blocCont;
	    )
	else raise (Error ("la valeur de retour doit être numérique", 
			   p.Ast.protoLoc))
      | Some s1, Some s2 -> (* Méthode *)
	if typNum t || (typEq t TypVoid) then
	  ProtoBloc 
	    ( 
	      { protoVar = var;
		argumentList = argList ;
		protoKind = Method s1 },
	      insListTyper (Smap.add "this" (TypIdent s1) 
			      (Smap.add "return" t env)) b.Ast.blocCont;
	    )
	else raise (Error ("la valeur de retour doit être numérique",
			   p.Ast.protoLoc))
      | None, _ -> assert false
    end
    | Ast.Tident s -> (* Constructeur de s *)
      ProtoBloc 
	( 
	  { protoVar = protoVarTTyper p.Ast.protoVar ;
	    argumentList = argList;
	    protoKind = Cons s },
	  insListTyper (Smap.add "this" (TypIdent s) env) b.Ast.blocCont;
	)
    | Ast.TidentTident (s1, s2) -> (* Constructeur *)
      if s1==s2 then
      ProtoBloc 
	( 
	  { protoVar = protoVarTTyper p.Ast.protoVar ;
	    argumentList = argList;
	    protoKind = Cons s2 },
	  insListTyper (Smap.add "this" (TypIdent s2) env) b.Ast.blocCont;
	)
      else raise (Error (s2^" n'est pas un constructeur", p.Ast.protoLoc))

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
