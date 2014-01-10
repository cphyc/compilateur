exception Error of string * Ast.loc
open Tast

(* On crée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
(* Environnement global : à chaque variable un type *)
let genv: ((typ*bool) Smap.t) ref = ref Smap.empty

let classExistence: (string, unit) Hashtbl.t = Hashtbl.create 17
let classInheritances: (string, string) Hashtbl.t = Hashtbl.create 17
let classFields: (string, string * (typ * bool)) Hashtbl.t = Hashtbl.create 17
let classCons: (string, (typ * bool) list) Hashtbl.t = Hashtbl.create 17
let classMethods: (string, string) Hashtbl.t = Hashtbl.create 17

(*  (class, method), ((class, virtual), (return type, ref)), profile *)
let methodsTable: 
    (string*string, ((string*bool) * (typ*bool)) * (typ*bool) list) 
    Hashtbl.t = Hashtbl.create 7
(* associe à une fonction son type et sa signature *)
let functionsTable: (string, (typ * bool) * (typ*bool) list) Hashtbl.t = 
  Hashtbl.create 17

let functionVars: (string, var) Hashtbl.t = Hashtbl.create 17


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

(* Teste l'égalité de deux types *)
let rec typEq t1 t2 = match t1, t2 with
  | TypNull, TypNull | TypVoid, TypVoid | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> s1 == s2
  | TypPointer nt1, TypPointer nt2 -> typEq nt1 nt2
  | _ -> false

(* vérifie que t1 est un sous-type de t2 *)
let typIn t1 t2 = match t1, t2 with
  | TypInt, TypInt -> true
  | TypIdent s1, TypIdent s2 -> subClass s1 s2
  | TypNull, TypPointer _ -> true
  | TypPointer (TypIdent s1), TypPointer (TypIdent s2) -> subClass s1 s2
  | t1, t2 -> typEq t1 t2

(* teste si l1 est un plus petit profil que l2 *)
let rec leqProf l1 l2 = match l1, l2 with
  | [], [] -> true
  | (t1,b1)::q1, (t2,b2)::q2 -> (typIn t1 t2) && leqProf q1 q2
  | _ -> false

(* teste l egalite entre deux profils *)
let rec eqProf l1 l2 = match l1, l2 with
  | [], [] -> true
  | (t1,b1)::q1, (t2,b2)::q2 -> (typEq t1 t2) && eqProf q1 q2
  | _ -> false

(* Donne la liste des profils plus grands que *)
let rec geqListProf p0 = function
  | [] -> []
  | p::l -> if leqProf p0 p then p::(geqListProf p0 l) else geqListProf p0 l

(* La même, avec le droit de mettre des infos à gauche (type renvoyé et
   virtualité) *)
let rec geqListProf2 p0 = function
  | [] -> []
  | (a,p)::l -> if leqProf p0 p then (a,p)::(geqListProf2 p0 l) 
    else geqListProf2 p0 l


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

(* Idem *)
let minProf2 l0 =
  let rec min (a0,p0) = function
    | [] -> (a0,p0)
    | (a,p)::l -> let (a',p') = min (a0,p0) l in
	      if (not (eqProf p p')) && (leqProf p p') 
	      then (a,p) else (a',p') 
  in
  let rec minEns = function
    | [] -> []
    | (a,p)::l -> let l' = minEns l in
	      let (b,m) = min (a,p) l0 in
	      if List.exists (fun (c,n) -> eqProf n m) l' then l' else (b,m)::l'
  in
  minEns l0

(* renvoie le type d'un champ f pour la classe c et ses surclasses*)
let fieldType l c f =
  let rec aux c' = 
    if (List.mem_assoc f (Hashtbl.find_all classFields c')) then
      [(List.assoc f (Hashtbl.find_all classFields c'))]
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
  | TypIdent s -> Hashtbl.mem classExistence s
  | TypPointer p -> typBF p
  | _ -> false

let rec qidentTyper = function
  | Ast.Ident s -> Ident s
  | Ast.IdentIdent (s1, s2) -> IdentIdent (s1, s2)

let rec qvarTyper typ v0 = match v0.Ast.qvarCont with
  | Ast.QvarQident qident -> 
    {qvarIdent = qidentTyper qident; qvarRef = false; 
     qvarTyp = typConverter typ}
  | Ast.QvarPointer qvar -> 
    let nv = qvarTyper typ qvar in
    {qvarIdent = nv.qvarIdent; qvarRef = false;
     qvarTyp = TypPointer nv.qvarTyp}
  | Ast.QvarReference qvar -> 
    let nv = qvarTyper typ qvar in
    if nv.qvarRef then
      raise (Error("Double reference", v0.Ast.qvarLoc))
    else
      {qvarIdent = nv.qvarIdent; qvarRef = true;
       qvarTyp = nv.qvarTyp}

let rec prevent_redeclaration env var = match var.Ast.varCont with
  | Ast.VarIdent s -> 
    if Smap.mem s env then
      raise (Error ("Redéfinition illégale de la variable \""^s^"\".", 
		    var.Ast.varLoc))
    else ()
  | Ast.VarPointer v | Ast.VarReference v -> prevent_redeclaration env v
  
let protoVarTTyper loc = function 
  | Ast.Qvar (typ, qvar) -> (* Fonction ou méthode *)
    Qvar (qvarTyper typ.Ast.typCont qvar)
  | Ast.Tident s -> Tident s (* Constructeur *)
  | Ast.TidentTident (s,s') -> 
    if s == s' then Tident s (* Constructeur *)
    else raise (Error ("pas un constructeur",loc))

let rec varTyper typ v0 = match v0.Ast.varCont with
  | Ast.VarIdent s -> 
    { varIdent=s; varRef=false; varTyp=typConverter typ}
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
    let v = varTyper arg.Ast.argumentTyp.Ast.typCont arg.Ast.argumentVar in
    let t = v.varTyp in
    let r = v.varRef in
    let nlenv, vlist, tlist = (argumentTyper lenv alist) in
    varUnique arg.Ast.argumentLoc v vlist;
    (Smap.add v.varIdent (v.varTyp, v.varRef) nlenv), (v::vlist), ((t,r)::tlist)

(* Cette fonction verifie qu'on n'a pas redondance de variable *)
and varUnique loc v0 = function
  | [] -> ()
  | v::alist -> if v0.varIdent == v.varIdent then
      raise (Error ("Variable redondante", loc))
    else varUnique loc v0 alist

(* Pour convertir les membres d'une déclaration de classe  *)
let memberConverter s0 = function
  | Ast.MemberDeclVars dv ->
    let la = (List.map (varTyper dv.Ast.declVarsTyp.Ast.typCont) 
		dv.Ast.varList) in
    let aux var = 
      if typEq var.varTyp (TypIdent s0)
      then raise (Error ("Constructeur incomplet", dv.Ast.declVarsLoc));
      if not (typBF var.varTyp) 
      then raise (Error ("mal formé", dv.Ast.declVarsLoc));
      if var.varRef
      then raise (Error ("les champs ne peuvent pas être des références", 
		  dv.Ast.declVarsLoc));
      let v = var.varIdent in
      let l = Hashtbl.find_all classFields s0 in
      if List.mem_assoc v l 
      then raise (Error ("already defined", dv.Ast.declVarsLoc))
      else Hashtbl.add classFields s0 (v, (var.varTyp, var.varRef))
    in
    List.iter aux la;
    MemberDeclVars la
  | Ast.VirtualProto (b,p) ->     
    let _, argList, _ = argumentTyper Smap.empty p.Ast.argumentList in
    let lt = List.map (fun a -> (a.varTyp,a.varRef)) argList in
    begin match p.Ast.protoVar with
    | Ast.Qvar (t, q) -> 
      let q' = qvarTyper t.Ast.typCont q in
      let s = match q'.qvarIdent with
	| Ident s -> s	  
	| IdentIdent (s1, s2) -> 
	  if s1 == s0 
	  then raise (Error ("extra-qualification", p.Ast.protoLoc))
	  else raise (Error ("pas la même classe", p.Ast.protoLoc))
      in
      let l = Hashtbl.find_all methodsTable (s0,s) in
      if List.exists (eqProf lt) (snd (List.split l))
      then raise (Error ("ce profil a déjà été déclaré", p.Ast.protoLoc));
      Hashtbl.add methodsTable (s0,s) (((s0,b), (q'.qvarTyp,q'.qvarRef)), lt);
      VirtualProto (b, {protoVar = Qvar q'; argumentList = argList;})

    | Ast.Tident s -> 
      if s != s0 then raise (Error ("pas la même classe", p.Ast.protoLoc));
      let _, argList, _ = argumentTyper Smap.empty p.Ast.argumentList in
      let lt = List.map (fun a -> (a.varTyp,a.varRef)) argList in
      let l = Hashtbl.find_all classCons s in
      if List.exists (eqProf lt) l
      then raise (Error ("ce profil a déjà été déclaré", p.Ast.protoLoc));      
      Hashtbl.add classCons s lt;
      VirtualProto (b, {protoVar = Tident s; argumentList = argList;})
      

    | Ast.TidentTident (s1, s2) -> 
      if (s1 != s2) || (s0 != s1) 
      then raise (Error ("pas la même classe", p.Ast.protoLoc));
      let _, argList, _ = argumentTyper Smap.empty p.Ast.argumentList in
      let lt = List.map (fun a -> (a.varTyp,a.varRef)) argList in
      let l = Hashtbl.find_all classCons s1 in
      if List.exists (eqProf lt) l
      then raise (Error ("ce profil a déjà été déclaré", p.Ast.protoLoc));      
      Hashtbl.add classCons s1 lt;
      VirtualProto (b, {protoVar = Tident s1; argumentList = argList;})
    end

(* =========================TYPAGE DES EXPRESSIONS=========================== *)


let rec exprTyper lenv exp = match exp.Ast.exprCont with
  | Ast.ExprInt i -> { exprTyp=TypInt; exprCont= ExprInt i }
  | Ast.This -> 
    begin
      if not (Smap.mem "this" lenv)
      then raise (Error("pas de this dans une fonction", exp.Ast.exprLoc));
      let typ, _ = Smap.find "this" lenv in
      {exprTyp = typ ;  exprCont = This}
    end
  | Ast.Null -> { exprTyp=TypNull; exprCont=Null } 
  | Ast.ExprEqual (e1, e2) -> 
    let el, er = exprLVTyper lenv e1, exprTyper lenv e2 in
    if not (typIn er.exprTyp el.exprTyp) then
      raise (Error ("Types incompatibles.", exp.Ast.exprLoc))
    else if not (typNum er.exprTyp) then
      raise (Error ("Type numérique attendu.", e2.Ast.exprLoc))
    else
      { exprTyp = el.exprTyp; exprCont= ExprEqual (el, er) }

  | Ast.ExprApply (e, el) -> 
    begin
      match e.Ast.exprCont with
      (* On distingue les fonctions des constructeurs, etc ... *)
      | Ast.ExprQident (Ast.Ident s) when Hashtbl.mem functionsTable s -> 
	(* function *)
	let profList =  Hashtbl.find_all functionsTable s in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = List.map (fun texpr -> (texpr.exprTyp, false)) 
	  argList in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [(t,r),p] -> 
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p,
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end

      | Ast.ExprQident (Ast.Ident s) -> 
	if not (Smap.mem "this" lenv)
	then raise (Error ("Identificateur de fonction non déclaré.",
			   exp.Ast.exprLoc)); 
        let className = match Smap.find "this" lenv with 
	  | TypPointer (TypIdent s), rf -> s 
	  | _ -> assert false in
	let surclassList = Hashtbl.find_all classInheritances className in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, texpr.exprTyp)) argList in
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (className::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [((c,v),(t,r)),p] -> 
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p, 
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end
		  
      | Ast.ExprQident (Ast.IdentIdent (c,s)) -> 
	if not (Smap.mem "this" lenv)
	then raise (Error ("Identificateur de fonction non déclaré.",
			   exp.Ast.exprLoc)); 
        let c0 = match Smap.find "this" lenv with 
	  | TypPointer (TypIdent s), _ -> s 
	  | _ -> assert false in
	if not (subClass c0 c)
	then raise (Error(c0^" n'est pas un sous-type de "^c, exp.Ast.exprLoc));
	let surclassList = Hashtbl.find_all classInheritances c in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, false)) argList in
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (c::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [((c,v),(t,r)),p] -> 
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p,
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end
	

      | Ast.ExprDot (e', s) -> 
	let ne' = exprTyper lenv e' in
	let className = match ne'.exprTyp with
	  | TypIdent  s -> s
	  | _ -> raise (Error ("Ce n'est pas une classe",e'.Ast.exprLoc))
	in
	let surclassList = Hashtbl.find_all classInheritances className in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp,false)) argList in
	
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (className::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond",e'.Ast.exprLoc))
	  | [((c,v),(t,_)),p] -> 
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; exprCont = ExprDot (ne', s)},p,
	    argList)}
	  | _ -> raise (Error("Trop de profils",e'.Ast.exprLoc))
	end
 	
      | _ -> raise 
	(Error("Cette expression ne peut etre utilisée comme une fonction",
	       e.Ast.exprLoc)) 
    end
  | Ast.ExprNew (s, el) -> 
    let nel = List.map (exprTyper lenv) el in
    let lprof = Hashtbl.find_all classCons s in
    let p = List.map (fun e -> (e.exprTyp, false)) nel in
    begin
      match minProf (geqListProf p lprof) with
      | [] -> raise (Error ("no profile corresponds", exp.Ast.exprLoc))
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
  (* Lâche copier-coller *)
  | Ast.ExprApply (e, el) -> 
    begin
      match e.Ast.exprCont with
      (* On distingue les fonctions des constructeurs, etc ... *)
      | Ast.ExprQident (Ast.Ident s) when Hashtbl.mem functionsTable s -> 
	let profList =  Hashtbl.find_all functionsTable s in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, false)) argList in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [(t,r),p] -> 
	    if not r then 
	      raise (Error ("ce n'est pas une reference", exp.Ast.exprLoc));
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p,
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end


      | Ast.ExprQident (Ast.Ident s) -> 
	if not (Smap.mem "this" lenv)
	then raise (Error ("Identificateur de fonction non déclaré.",
			   exp.Ast.exprLoc)); 
        let className = match Smap.find "this" lenv with 
	  | (TypPointer (TypIdent s)), _ -> s
	  | _ -> assert false in
	let surclassList = Hashtbl.find_all classInheritances className in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, false)) argList in
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (className::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [((c,v),(t,r)),p] -> 
	    if not r then 
	      raise (Error ("ce n'est pas une reference", exp.Ast.exprLoc));
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p,
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end
		  
      | Ast.ExprQident (Ast.IdentIdent (c,s)) -> 
	if not (Smap.mem "this" lenv)
	then raise (Error ("Identificateur de fonction non déclaré.",
			   exp.Ast.exprLoc)); 
        let c0 = match Smap.find "this" lenv with 
	  | TypPointer (TypIdent s), _ -> s 
	  | _ -> assert false in
	if not (subClass c0 c)
	then raise (Error(c0^" n'est pas un sous-type de "^c, exp.Ast.exprLoc));
	let surclassList = Hashtbl.find_all classInheritances c in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, false)) argList in
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (c::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond", exp.Ast.exprLoc))
	  | [((c,v),(t,r)),p] -> 
	    if not r then raise (Error ("ce n'est pas une reference", 
				 exp.Ast.exprLoc));
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; 
				    exprCont = ExprQident (r, Ident s)}, p,
				   argList)}
	  | _ -> raise (Error("Trop de profils",exp.Ast.exprLoc))
	end
	

      | Ast.ExprDot (e', s) -> 
	let ne' = exprTyper lenv e' in
	let className = match ne'.exprTyp with
	  | TypIdent  s -> s
	  | _ -> raise (Error ("Ce n'est pas une classe",e'.Ast.exprLoc))
	in
	let surclassList = Hashtbl.find_all classInheritances className in
	let argList = List.map (exprTyper lenv) el in
	let argTypList = 
	  List.map (fun texpr -> (texpr.exprTyp, false)) argList in
	
	let profList = List.concat 
	  (List.map (fun c -> Hashtbl.find_all methodsTable (c,s))
	  (className::surclassList)) in
	begin
	  match minProf2 (geqListProf2 argTypList profList) with
	  | [] -> raise (Error("Aucun profil ne correspond",e'.Ast.exprLoc))
	  | [((c,v),(t,r)),p] -> 
	    if not r then raise (Error("ce n'est pas une reference", 
				 exp.Ast.exprLoc));
	    {exprTyp = t ; 
	     exprCont = ExprApply ({exprTyp = t; exprCont = ExprDot (ne', s)},p,
	    argList)}
	  | _ -> raise (Error("Trop de profils",e'.Ast.exprLoc))
	end
 	
      | _ -> raise 
	(Error("Cette expression ne peut etre utilisée comme une fonction",
	       e.Ast.exprLoc)) 
    end


  | Ast.ExprQident (Ast.Ident s) ->
    if Smap.mem s lenv then
      let ttyp, rf = Smap.find s lenv in
      { exprTyp = ttyp; exprCont = ExprQident (rf, Ident s) }
    else if Smap.mem "this" lenv then 	(* Champ dans le constructeur *)
      let cl, rf = match Smap.find "this" lenv with
	| TypPointer (TypIdent s), rf -> s, rf
	| _ -> assert false
      in
      let ftyp = fieldType exp.Ast.exprLoc cl s in
      { exprTyp = fst ftyp; exprCont = ExprQident (rf, Ident s) }
    else
      raise (Error ("Variable \""^s^"\" non déclarée", exp.Ast.exprLoc))
  | Ast.ExprQident (Ast.IdentIdent (s1, s2)) -> assert false
  | Ast.ExprDot (e, s) ->    
    let ne = exprLVTyper lenv e in
    begin match ne.exprTyp with
    | TypIdent c -> {exprTyp = fst (fieldType e.Ast.exprLoc c s); 
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

let rec insTyper lenv qv ins = match ins.Ast.insCont with
  | Ast.InsSemicolon -> (lenv, InsSemicolon)
  | Ast.InsExpr e -> (lenv, InsExpr (exprTyper lenv e))
  | Ast.InsDef (typ, var, insDef) ->
	(* On commence par vérifier que la variable n'a pas été déclarée,
	   puis on type l'instruction de définition *)
    prevent_redeclaration lenv var;
    begin
      let tvar = varTyper typ.Ast.typCont var in
      begin
	match qv with
	| Qvar q ->
	  begin match q.qvarIdent with
	  | Ident s -> Hashtbl.add functionVars s tvar
	  | _ -> ()
	  end
	| Tident s -> ()
      end;
      match insDef with
      | Some Ast.InsDefExpr e -> 
	let te = 
	  if tvar.varRef 
	  then exprLVTyper lenv e
	  else exprTyper lenv e in
	if not (typIn te.exprTyp tvar.varTyp) then
	  raise (Error ("Types incompatibles.", ins.Ast.insLoc));
	if not (typBF tvar.varTyp) then
	  raise (Error ("Type mal forme", ins.Ast.insLoc));
	(Smap.add tvar.varIdent (tvar.varTyp, tvar.varRef) lenv),
	  InsDef (tvar, Some (InsDefExpr te))
      | Some Ast.InsDefIdent (s, elist) -> 
	if tvar.varRef
	then raise (Error ("une classe n'est pas une valeur gauche", 
			   ins.Ast.insLoc));
	let s0 = match tvar.varTyp with
	  | TypIdent s -> s
	  | _ -> raise (Error ("pas une classe", ins.Ast.insLoc))
	in
	if s != s0 then raise (Error ("pas la meme classe", ins.Ast.insLoc));
	let nel = List.map (exprTyper lenv) elist in
	let lprof = Hashtbl.find_all classCons s in
	let p = List.map (fun e -> (e.exprTyp, false)) nel in
	begin
	  match minProf (geqListProf p lprof) with
	  | [] -> raise (Error ("no profile corresponds", ins.Ast.insLoc))
	  | [p] -> (Smap.add tvar.varIdent (tvar.varTyp, tvar.varRef) lenv),
	    InsDef (tvar, Some (InsDefIdent (s, nel)))
	  | _ -> raise (Error ("several profiles correspond", 
			       ins.Ast.insLoc))
	end
      | None -> if not (typBF tvar.varTyp)
	then raise (Error ("type mal formé", ins.Ast.insLoc));
	( Smap.add tvar.varIdent (tvar.varTyp, tvar.varRef) lenv), 
	InsDef (tvar, None) 
    end
  | Ast.InsIf (e, i) -> 
    let _, ni = insTyper lenv qv i in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv, InsIf (ne, ni)
  | Ast.InsIfElse (e, i1, i2) -> 
    let _, ins1 = insTyper lenv qv i1 in
    let _, ins2 = insTyper lenv qv i2 in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv,
    InsIfElse (exprTyper lenv e, ins1, ins2)
  | Ast.InsWhile (e, i) -> 
    let _, ni = insTyper lenv qv i in
    let ne = exprTyper lenv e in
    if not (typEq ne.exprTyp TypInt) then 
      raise (Error ("Pas un type int comme condition", ins.Ast.insLoc));
    lenv, InsWhile (ne, ni)
  | Ast.InsFor (el1, eopt, el2, i) -> 
    let nel1 = List.map (exprTyper lenv) el1 in
    let nel2 = List.map (exprTyper lenv) el2 in
    let _, ni = insTyper lenv qv i in
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
    let insList = insListTyper lenv qv b.Ast.blocCont in
    (lenv, InsBloc insList)
  | Ast.InsCout s -> 
    (lenv, InsCout (List.map (expr_strTyper lenv) s))
  | Ast.InsReturn None -> lenv, InsReturn None
  | Ast.InsReturn (Some e) -> 
    let ne, rtyp = 
      match qv with
      | Tident _ -> 
	raise (Error ("pas de return dans les constructeurs", ins.Ast.insLoc))
      | Qvar q  -> 
	if q.qvarRef
	then (exprLVTyper lenv e), q.qvarTyp
	else (exprTyper lenv e), q.qvarTyp
    in
    if typIn ne.exprTyp rtyp then
      (lenv, InsReturn (Some ne))
    else raise (Error ("wrong return type", ins.Ast.insLoc))

(* Typage des instructions.
   La fonction type au fur et à mesure les instructions en mettant à jour 
   l'environnement. 
   qvar permet de savoir si l'on a affaire a un cons, une fonction ou une
   methode et nous donne l'eventuel type de retour.
   sig : env -> qvar -> Ast.ins list -> Tast.ins *)
and insListTyper lenv v = function
  | [] -> []
  | ins::insl -> 
    (* Sous fonction qui récupère le nouvel lenv et l'instruction typée *)
    let nlenv, tIns = insTyper lenv v ins in
    tIns::(insListTyper nlenv v insl)



(* =========================TYPAGE DES DÉCLARATIONS========================== *)

let declTyper = function
  | Ast.DeclVars dv -> 
    let atyp = dv.Ast.declVarsTyp.Ast.typCont in
    let vlist = dv.Ast.varList in
    DeclVars 
      ( List.map (fun var -> 
	let nv = varTyper atyp var in
	if not (typBF nv.varTyp) then
	  raise (Error ("type mal formé", dv.Ast.declVarsLoc));
	if Hashtbl.mem functionsTable nv.varIdent 
	then raise (Error ("Variable déjà utilisée",dv.Ast.declVarsLoc));
	genv := Smap.add nv.varIdent (nv.varTyp, nv.varRef) !genv;
	nv) vlist)   
      
  | Ast.DeclClass c -> 
    if Hashtbl.mem classExistence c.Ast.className
    then raise (Error ("Classe deja existente", c.Ast.declClassLoc));
    let l = match c.Ast.supersOpt with
      | None -> []
      | Some l' -> l' in
    List.iter (Hashtbl.add classInheritances c.Ast.className) l;
    Hashtbl.add classExistence c.Ast.className ();
    let memberList = List.map (memberConverter c.Ast.className) 
      c.Ast.memberList in
    if (List.for_all 
	  (function 
	  |VirtualProto (b, p) -> 
	    (match p.protoVar with 
	    | Tident _ -> false 
	    | _ -> true)
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
    let env, argList, typList = argumentTyper !genv p.Ast.argumentList in

    let var = protoVarTTyper p.Ast.protoLoc p.Ast.protoVar in
    match p.Ast.protoVar with
    | Ast.Qvar (t,q) -> begin
      (* On distingue les methodes, les classes et les fonctions *)
      let q' = qvarTyper t.Ast.typCont q in
      let t' = q'.qvarTyp in
      match q'.qvarIdent with
      | Ident s -> (* Fonctions *)
	if Hashtbl.mem functionsTable s 
	then raise (Error ("Fonction déjà définie",p.Ast.protoLoc));
	if Smap.mem s !genv
	then raise (Error ("Nom déjà utilisé",p.Ast.protoLoc)); 
	(* On ajoute la fonction à la liste des fonctions *)
	Hashtbl.add functionsTable s ((t',q'.qvarRef), typList);
	
	if typNum t' || (typEq t' TypVoid) then
	  ProtoBloc	
	    ( 
	      { protoVar = var;
		argumentList = argList },
		insListTyper env var b.Ast.blocCont;
	    )
	else raise (Error ("la valeur de retour doit être numérique", 
			   p.Ast.protoLoc))
      | IdentIdent (s1, s2) -> (* Méthode s2 de s1 *)
	if typNum t' || (typEq t' TypVoid) then
	  ProtoBloc 
	    ( 
	      { protoVar = var;
		argumentList = argList},	   
	      insListTyper (Smap.add "this" ((TypPointer (TypIdent s1)), false)
			      env) var b.Ast.blocCont;
	    )
	else raise (Error ("la valeur de retour doit être numérique",
			   p.Ast.protoLoc))
    end
    | Ast.Tident s -> (* Constructeur de s *)
      ProtoBloc 
	( 
	  { protoVar = var ;
	    argumentList = argList},
	  insListTyper (Smap.add "this" ((TypPointer (TypIdent s)), false) env) 
	    var b.Ast.blocCont;
	)
    | Ast.TidentTident (s1, s2) -> (* Constructeur *)
      if s1==s2 then
      ProtoBloc 
	( 
	  { protoVar = var ;
	    argumentList = argList },
	  insListTyper (Smap.add "this" ((TypPointer (TypIdent s2)),false) env) 
	    var b.Ast.blocCont;
	)
      else raise (Error (s2^" n'est pas un constructeur", p.Ast.protoLoc))

let file f = 
  {
    iostr = f.Ast.iostr;
    fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
