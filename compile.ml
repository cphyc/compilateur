exception Error of string
open Format
open Mips
open Tast

(********************* Variables globales ********************)
(* Ensemble des chaines de caractère *)
module Smap = Map.Make(String)
let dataMap = ref Smap.empty

(* Environnement global *)
(* associe à un identificateur un couple de label * taille_de_la_variable *)
let (genv : (string, string * int) Hashtbl.t) = Hashtbl.create 17

(* Table de hashage associant à un identificateur sa taille, son label et la taille 
   de ses args *)
let functionsTable: (string, int * string * typ list) Hashtbl.t = Hashtbl.create 17

let print_profile profile = List.iter (fun t -> match t with
  | TypNull -> printf "\t TypeNull@."
  | TypVoid -> printf "\t TypeVoid@."
  | TypInt -> printf "\t TypeInt@."
  | TypIdent s -> print_string ("\t Classe"^s);printf "@.";
  | TypPointer _ -> ()) profile

let eq_profile p1 p2 = 
  try
    List.for_all2 (fun t1 t2 -> t1 == t2) p1 p2
  with Invalid_argument _ -> false

(* "pushn size" empile "size" octets sur la pile *)
let pushn = sub sp sp oi
class methodObject l r t s v = object (self)
  val mutable lab:string option = l
  val is_ref:bool = r 
  val typ : typ = t
  val profile : typ list = s
  val virt : bool = v
  method get_profile = profile
  method print_profile = print_profile profile

  method get_lab : string = match lab with None -> assert false | Some s -> s
  method set_lab label_creator = match lab with 
  | None -> let newLab = label_creator () in lab <- Some newLab; newLab
  | Some lab -> lab
end

class consObject l t s = object
  val mutable lab:string option = l
  val typ:typ = t
  val profile:typ list = s
  method set_lab s = lab <- Some s
  method get_lab : text = match lab with None -> assert false | Some s -> label s 
end

class classObject classTable = object (self)
  val mutable initCode : text = nop
  val mutable map : (int*int) Smap.t = Smap.empty
  val mutable parents : classObject list = []
  val mutable declClass = {className = ""; supersOpt = None; memberList = []}
  val mutable size = 0
  val mutable methods : methodObject list Smap.t = Smap.empty
  val mutable cons : consObject list = []
  method print_methods = 
    Smap.iter (fun str objList -> print_string (str^" associé à :"); printf "@.";
      List.iter (fun obj -> printf "et@.";obj#print_profile) objList) methods
  method init = initCode
  method offs_var_map = Smap.map (fun (a,_) -> a) map
  method size = size
  method decl = declClass
  method offset s = fst (Smap.find s map)
  method add_method str m =
    (* On veut ajouter une méthode : on récupère la liste des méthodes de memes nom *)
    let mList = try Smap.find str methods with 
	Not_found -> [] in
    (* On lui ajoute la méthode m *)
    let list = m :: mList in
    (* On sauvegarde dans methods *)
    methods <- Smap.add str list methods
  method add_method_label str profile label_creator =
    (* On récupère la méthode associée *)
    printf "Recherche du profil :\t";print_string (str);print_profile profile;printf "@.";
    let met = match self#get_method str profile with None -> assert false | Some m -> m in
    (* On lui demande créer un nouveau label *)
    met#set_lab label_creator;
  method get_method str profile = (* On commence par chercher dans la classe, sinon
				     on explore les supers *)
    (* On récupère la liste des méthodes dont le nom est str *)
    try let metList = Smap.find str methods in
	(* Dans cette liste, y a t-il une methode qui a le bon profil ?*)
	Some (List.find (fun met ->
	  printf "element : @.";met#print_profile; 
	  if eq_profile met#get_profile profile then (printf "Match !@."; true)
	  else (printf "Fail@."; false)) metList)
    with Not_found -> 
      (* On explore tous les supers jusqu'à trouver la bonne *)
      try let dadysClass = 
	    (List.find (fun classObj -> 
	      match classObj#get_method str profile with
	      | None -> false 
	      | Some met -> true) parents) 
	  in
	  dadysClass#get_method str profile
      with Not_found -> None
  method add_cons str consObj = cons <- consObj :: cons
  method build c =
    let rec sizeof = function
      | TypNull -> assert false
      | TypVoid -> 4 
      | TypInt -> 4
      | TypIdent s -> (Hashtbl.find classTable s)#size
      | TypPointer t -> sizeof t
    in
    (* Construit l'environnement, le code d'initialisation, calcul la taille et 
       renvoie le tout *)
    (* On s'occupe des supers *)
    let rec supersRunner env = function
      | None -> Smap.empty
      | Some super -> assert false
    in
    let superEnv = supersRunner Smap.empty c.supersOpt in
       
    (* À l'aide de cet env, on s'occupe des membres *)
    let rec memberListRunner env = function
      | [] -> env
      | member::mlist -> ( match member with
	| MemberDeclVars dv -> 
	  (* On ajoute tous les copaines à l'environnement *)
	  List.fold_left (fun env var ->
	    Smap.add var.varIdent (sizeof var.varTyp) env)
	    (memberListRunner env mlist) dv
	| VirtualProto (virt, proto) -> 
	  (* On calcule son profile *)
	  let profile = 
	    List.map (fun arg -> arg.varTyp) proto.argumentList 
	  in
	  (* On ajoute soit un constructeur, soit une méthode *)
	  let _ =  match proto.protoVar with
	  | Function qvar -> (* Achtung si is_ref est vrai *)
	    let is_ref = qvar.qvarRef in
	    let typ = qvar.qvarTyp in
	    (
	      match qvar.qvarIdent with
		(* Constructeurs *)
	      | Ident s when s == declClass.className -> 
		(* On a un constructeur, on l'ajoute à la liste *)
		self#add_cons s (new consObject None typ profile);
	      | IdentIdent (s1, s2) when s2 == declClass.className ->
		(* On a un constructeur, on l'ajoute à la liste *)
		assert (s1 == declClass.className);
		self#add_cons s1 (new consObject None typ profile);

                (* Méthodes *)
	      | Ident s -> 
		self#add_method s 
		  (new methodObject None is_ref typ profile virt)
	      | IdentIdent (s1, s2) -> assert (s1 == declClass.className);
		self#add_method s2 
		  (new methodObject None is_ref typ profile virt)
	    )
	  | _ -> assert false
	  in
	  (* On continue à traiter mlist *)
	  memberListRunner env mlist
      )
    in 
    let memberListEnv = memberListRunner superEnv c.memberList in
       
    (* On transforme l'environnement donnant les tailles en un environnement donnant
       la position par rapport au début ET les tailles*)
    let first_free, offsEnv = Smap.fold (fun ident size (first_free, offsMap) -> 
      (first_free + size),
      Smap.add ident (first_free, size) offsMap) memberListEnv (0, Smap.empty)
    in
    (* On sauvegarde le tout en appelant éventuellement le constructeur (TODO)*)
    initCode <- Smap.fold (fun ident size code -> 
      code 
      ++  comment ("Membre "^ident)
      ++  pushn size) memberListEnv nop;
    map <- offsEnv;
    declClass <- c;
    size <- first_free;
    self#print_methods;
end
(* Une classe : une déclaration, une map des tailles*positions, un code 
   d'initialisation, une taille *)
let classTable: (string, classObject) Hashtbl.t = Hashtbl.create 17

(* Associe à variable de classe sa map *)
let classVars: (string, (int*int) Smap.t) Hashtbl.t = Hashtbl.create 17
let vrai = 1
let faux = 0

(********************* Utilitaires ********************)
(* compteur pour de belles étiquettes *)
let labelint = ref 0
let new_label () = labelint := !labelint + 1; 
  if !labelint = 42 then
    "label_of_the_answer"
  else
    "label_"^(string_of_int (!labelint))

(* renvoie la taille d'un type *)
let rec sizeof = function
| TypNull -> assert false
| TypVoid -> 4 
| TypInt -> 4
| TypIdent s -> (Hashtbl.find classTable s)#size
| TypPointer t -> sizeof t

let  get_ident q = match q.qvarIdent with
  | Ident s -> s
  | IdentIdent (_,s) -> s (*On ne renvoie pas le nom de la classe, déjà présent *)
 
(* () -> mips *)
let save_fp_ra = 
       comment " sauvegarde de fp" 
  ++   push fp
  ++   comment " sauvegarde de ra"
  ++   push ra
let restore_ra_fp = lw ra areg (-8, fp) ++ lw fp areg (-4, fp)

let print_int = 
      comment " print_int" 
  ++  li v0 1 
  ++  syscall

let print_label label = 
      comment (" print word at "^label) 
  ++  la a0 alab label 
  ++  li v0 4
  ++  syscall

(********************* Allocateurs ********************)
(* alloue de la mémoire pour les arguments d'un appel de fonction *)
let rec allocate_args shift = function
  | [] -> Smap.empty
  | v::vlist -> let new_shift = shift + sizeof v.varTyp in
		(Smap.add v.varIdent shift (allocate_args new_shift vlist))

(* alloue de la mémoire pour une variable en cherchant le plus petit offset (% à fp)
   et empile en dessous. Si l'env est vide, renvoie - 8 (car on a sauvé fp et sp).*)
let allocate_var v lenv = 
  let _, offset = 
    try 
      (* On prend le plus grand offset en val abs parmi les offs négatifs *)
      Smap.max_binding 
	(Smap.filter (fun _ off -> if off>=0 then false else true) lenv)
    with Not_found -> "", -8
  in
  Smap.add v.varIdent (offset - sizeof v.varTyp) lenv

type member = MemVar of string * typ | MemFun
(* transforme tous les membres en une belle liste, puis les place dans un Smap *)
let rec memberSize_list ml =
  let rec aux = function
    | MemberDeclVars [] -> []
    | MemberDeclVars (v::vlist) -> 
      MemVar (v.varIdent, v.varTyp) :: (aux (MemberDeclVars vlist))
    | VirtualProto _ -> assert false
  in
  let rec memList = function
    | [] -> []
    | member :: mlist -> List.append (aux member) (memList mlist)
  in
  List.fold_left 
    (fun map member ->
      let ident, size = (match member with
	| MemVar (id,t) -> id, sizeof t
	| MemFun -> assert false)
      in
      Smap.add ident size map) Smap.empty (memList ml)
    
let funQvar_to_ident q = match q.qvarIdent with
    | Ident s -> Some s, None
    | IdentIdent (s1, s2) -> Some s1, Some s2

(******************** Compilation ********************)
let rec compile_LVexpr lenv cenv = function
  | ExprQident q -> begin match q with
    | Ident s ->
      begin
	if Smap.mem s lenv then (* Variable locale *)
	  let pos = Smap.find s lenv in 
	       comment (" variable locale "^s) 
	    ++ li a0 pos 
	    ++ add a0 a0 oreg fp 
	    ++ push a0
	else if Smap.mem s cenv then (* Variable de classe *)
	  let offset = Smap.find s cenv in 
	  let this = Smap.find "this" lenv in
	      comment (" variable de classe "^s)
	  ++  comment "  récupération de this"
	  ++  li a0 this
	  ++  add a0 a0 oreg fp
	  ++  comment ("  récupération de la variable "^s)
	  ++  li a1 offset
	  ++  add a0 a0 oreg a1
	  ++  push a0
	else (* Variable globale *)
	  let lab, _ = try Hashtbl.find genv s with _ -> raise (Error "pas trouvé !") in
	  comment (" variable globale au label "^lab) 
	  ++ la a0 alab lab
	  ++ push a0
      end
    | IdentIdent (s1, s2) -> assert false
  end
  | ExprStar e -> assert false 
  | ExprDot (e,s) -> 
    (match e.exprTyp with 
    | TypIdent c -> (* On un directement une classe *)
      let offset = (Hashtbl.find classTable c)#offset s in
          comment (" Variable de class "^s)
      ++  compile_LVexpr lenv cenv e.exprCont
      ++  pop a0              (* on a l'adresse % fp, et l'offset *)
      ++  add a0 a0 oi offset (* on a dans a0 l'adresse de la variable *)
      ++  push a0             (* on pousse l'adresse variable *)
    | TypPointer (TypIdent c) -> assert false
    | _ -> assert false
    )
  | _ -> assert false

let rec compile_expr ex lenv cenv = match ex.exprCont with
(* Compile l'expression et place le résultat au sommet de la pile *)
  | ExprInt i -> li a0 i ++ push a0
  | This -> assert false
  | Null -> assert false
  | ExprQident q -> begin match q with 
    | Ident s -> 
      (* Pas la peine de vérifier que ça a été déclaré, on l'a déjà fait *)
      let instruction =
	if Smap.mem s lenv then (* Variable locale *)
	  let offset = Smap.find s lenv in
	  lw a0 areg (offset, fp)
	else if Smap.mem s cenv then (* Membre d'une classe *)
	  let this_offset = Smap.find "this" lenv in
	  let var_offset = Smap.find s cenv in
	      comment " position de this"
	  ++  add a0 fp oi this_offset
	  ++  comment " chargement de la variable"
	  ++  lw a0 areg (var_offset, a0)
	else
	  let lab, _ = try Hashtbl.find genv s with _ -> raise (Error ("pas trouvé "^s)) in 
	  lw a0 alab lab
      in
      comment (" chargement variable "^s) ++ instruction
      ++ push a0      
    | IdentIdent (s1,s2) -> assert false
  end
  | ExprStar e -> assert false
  | ExprDot (e,s) -> (* On récupère l'adresse de e comme une lvalue, puis on
			calcul l'offset de s *)
    (match e.exprTyp with 
    | TypIdent c -> 
      let offset = (Hashtbl.find classTable c)#offset s in
          comment (" Variable de class "^s)
      ++  compile_LVexpr lenv cenv e.exprCont
      ++  pop a0              (* on a l'adresse % fp, et l'offset *)
      ++  add a0 a0 oi offset (* on a dans a0 l'adresse de la variable *)
      ++  lw a0 areg (0, a0)  (* on charge la variable dans a0 *)
      ++  push a0
    | _ -> assert false
  )
  | ExprEqual (e1,e2) -> (* On compile l'expression e1, c'est une lvalue donc 
			    le résultat est son adresse *)
    compile_LVexpr lenv cenv e1.exprCont
    ++ comment " calcul de la valeur droite" ++ compile_expr e2 lenv cenv 
    ++ pop a1 ++ pop a0 
    ++ comment " sauvegarde de la valeur" ++ sw a1 areg (0, a0)
  | ExprApply (e,p,l) -> ( 		(* cadeau : p profil cherché *)
    match e.exprCont with
    | ExprQident (Ident s) -> (* appel de fonction *)
      (* On recherche le profil correspondant *)
      let size, funlab, typList = 
	List.find (fun (s, l, tl) -> tl == p)
	  (Hashtbl.find_all functionsTable s) in
      let codeArgs = 
	(* On fold à droite pour avoir dans le bon sens *)
	List.fold_right (fun expr code -> code ++ compile_expr expr lenv cenv) l nop
      in
          comment (" appel de la fonction "^s)
      ++  comment "  construction de la pile"
      ++  codeArgs
      ++  jal funlab
      ++  comment "  on met le résultat sur la pile"
      ++  push v0
    | ExprQident (IdentIdent (s1,s2)) when s1==s2 -> assert false(* appel de cons *)
    | ExprQident (IdentIdent (s1,s2)) -> assert false (* appel de méthode *)
    | ExprParenthesis e -> assert false (* compile_expr lenv {ExprApply (e,l)} *)
    | ExprDot (e,s) -> 
      (* On compile l'adresse de e *)
      (* let get_e_address = compile_LVexpr lenv cenv e.exprCont ++ pop a0 in *)
      (* On trouve la classe grace au type *)
      let className = match e.exprTyp with | TypIdent s -> s | _ -> assert false in
      (* On a la classe, on cherche alors le code de la méthode *)
      let classObj = Hashtbl.find classTable className in
      let metObject = match classObj#get_method s p with 
	| None -> assert false | Some m -> m in
      (* let offs_var_map = classObj#offs_var_map  in *)
      let metLab = metObject#get_lab in
      (* On compile la pile d'argument *)
      let codeArgs = 
	List.fold_right (fun expr code -> code ++ compile_expr expr lenv cenv) l nop
      in
          comment (" appel de la méthode "^s^" de "^className)
      ++  comment "  construction de la pile"
      ++  codeArgs
      ++  jal metLab
      ++  comment "  on met le résultat sur la pile"
      ++  push v0
    | _ -> assert false
  )
  | ExprNew (s, l) -> assert false
  | ExprRIncr e | ExprLIncr e -> (* On compile l'expression, on ajoute un et on la stocke *)
        compile_LVexpr lenv cenv e.exprCont
    ++  pop a0                (* adresse de l'expression *)
    ++  lw a1 areg (0, a0)
    ++  add a1 a1 oi 1 
    ++  sw a1 areg (0, a0)
    ++  push a1
  | ExprRDecr e 
  | ExprLDecr e -> compile_expr e lenv cenv
    ++ comment " Décrémentation" ++ pop a0 ++ sub a0 a0 oi 1 ++ push a0
  | ExprAmpersand e -> assert false
  | ExprExclamation e ->
    let lab2, lab1 = new_label (), new_label () in
    compile_expr e lenv cenv
    ++ comment " Négation logique" ++ pop a0 ++ beqz a0 lab1
    ++ comment "  cas non nul :" ++ li a0 faux ++ push a0 ++ b lab2
    ++ label lab1 ++ comment "  cas nul :" ++ li a0 vrai ++ push a0
    ++ label lab2
  | ExprMinus e -> compile_expr e lenv cenv ++ pop a0 
    ++ comment " Négation arithmétique" ++ neg a0 a0 ++ push a0
  | ExprPlus e -> compile_expr e lenv cenv
  | ExprOp (e1,o,e2) -> 
    begin
      let ce1, ce2 = compile_expr e1 lenv cenv, compile_expr e2 lenv cenv in
      let calc = ce1 ++ ce2 ++ pop a1 ++ pop a0 in
      let comp_op operator = calc ++ (operator a0 a0 a1) ++ push a0 in
      let arith_op operator = calc ++ (operator a0 a0 oreg a1) ++ push a0 in
      match o with
      | OpEqual -> comp_op seq
      | OpDiff -> comp_op sne
      | OpLesser ->  comp_op slt
      | OpLesserEqual -> comp_op sle
      | OpGreater -> comp_op sgt
      | OpGreaterEqual -> comp_op sge
      | OpPlus -> arith_op add
      | OpMinus -> arith_op sub
      | OpTimes -> arith_op mul
      | OpDivide -> arith_op div 
      | OpModulo -> arith_op rem (*TODO : ^*)
      | OpAnd -> (* Paresseux *)
	let label1, label2 = new_label (), new_label () in
	ce1 ++ pop a0 ++ beqz a0 label1 
	++ ce2 ++ pop a0 ++ beqz a0 label1
	++ li a0 vrai ++ push a0 ++ b label2 ++ label label1 
	++ push zero ++ label label2
      | OpOr -> 
	let label1, label2 = new_label (), new_label () in
	ce1 ++ pop a0 ++ bnez a0 label1 
	++ ce2 ++ pop a0 ++ bnez a0 label1
	++ push zero ++ b label2 ++ label label1 
	++ li a0 vrai ++ push a0 ++ label label2
    end
  | ExprParenthesis e -> compile_expr e lenv cenv

(* lenv est de type offset*registre Smap.t *)
let rec compile_ins lenv cenv sp = function
  | InsSemicolon -> nop, lenv
  | InsExpr e -> (* le résultat est placé en sommet de pile *)
      compile_expr e lenv cenv, lenv
  | InsDef (v, op) ->
    let comm = comment (" allocation de la variable "^v.varIdent) in
    let s = sizeof v.varTyp in
    let nlenv = allocate_var v lenv in
    let rhs = match op with
      | None -> pushn s
      | Some InsDefExpr e -> compile_expr e nlenv cenv
      | Some InsDefIdent (c, elist) -> (* Classe *)
	(Hashtbl.find classTable c)#init
    in
    comm ++ rhs, nlenv
  | InsIf (e,i) -> (* astuce de faineant *)
    compile_ins lenv cenv sp (InsIfElse(e,i,InsSemicolon))
  | InsIfElse (e,i1,i2) -> 
    let if_true, if_false, way_out = new_label (), new_label (), new_label () in
    let ins1, _ = compile_ins lenv cenv sp i1 in
    let ins2, _ = compile_ins lenv cenv sp i2 in
    compile_expr e lenv cenv ++ pop a0 ++ bnez a0 if_true 
    ++ comment " si vrai :" ++ label if_true ++ ins1 ++ b way_out
    ++ comment " si faux :" ++ label if_false ++ ins2
    ++ label way_out, lenv
  | InsWhile (e,i) -> 
    let way_in, way_out = new_label(), new_label() in
    let ins1, _ = compile_ins lenv cenv sp i in
    comment " entrée de la boucle while" ++ label way_in 
    ++ comment " test du while" ++ compile_expr e lenv cenv ++ pop a0 ++ beqz a0 way_out
    ++ comment " coeur du while" ++ ins1 ++ b way_in
    ++ comment " sortie de la boucle while" ++ label way_out, lenv
  | InsFor (l1,e,l2,i) -> 
    let compile_expr_list = 
      List.fold_left (fun code e -> code ++ compile_expr e lenv cenv) nop in
    let init = compile_expr_list l1 in
    let test = compile_expr e lenv cenv in
    let modify = compile_expr_list l2 in
    let core, _ = compile_ins lenv cenv sp i in
    let labtest, way_out = new_label (), new_label () in
    comment " initialisation de la boucle for" ++ init
    ++ comment " test de sa condition" ++ label labtest 
    ++ test ++ pop a0 ++ beqz a0 way_out
    ++ comment " coeur de la boucle for" ++ core
    ++ comment " modification des paramètres testés" ++ modify ++ b labtest
    ++ comment " sortie de la boucle for" ++ label way_out, lenv
  | InsBloc b ->
    let aux (code', lenv) ins =
      let inscode, nlenv = compile_ins lenv cenv sp ins in
      code' ++ inscode, nlenv
    in
    let inslistcode, nlenv = (List.fold_left aux (nop, lenv) b) in
    inslistcode, nlenv
  | InsCout l -> 
    let aux (code, lenv) = function
      | ExprStrExpr e -> 
	let newcode = 
	  (compile_expr e lenv cenv) ++ pop a0 ++ print_int
	in code ++ newcode, lenv
      | ExprStrStr s ->
	let lab = new_label () in
	dataMap := Smap.add lab s !dataMap;

	code ++	print_label lab, lenv
    in
    let inscode, nlenv = (List.fold_left aux (nop, lenv) l) in
    let comm = comment " cout" in
    comm ++ inscode, nlenv
  | InsReturn eopt -> 
    let expr = match eopt with 
      | Some e -> compile_expr e lenv cenv
      | None -> nop 
    in
        expr
    ++  comment " quitte la fonction, résultat dans v0"
    ++  pop v0
    ++  restore_ra_fp
    ++  jr ra, lenv
		 
let compile_decl codefun codemain = function
  | DeclVars vlist -> 
    let rec process = function
      | [] -> ()
      | var::vlist -> 
	Hashtbl.add genv var.varIdent (new_label (), sizeof var.varTyp);
	process vlist
    in
    process vlist;
    nop, nop
  | DeclClass c -> (* On se contente juste d'ajouter la classe dans la table *)
    let newClass = new classObject classTable in
    newClass#build c;
    Hashtbl.add classTable c.className newClass;
    nop, nop
 
  | ProtoBloc (p, b) -> 
    (
      let var, argList = p.protoVar, p.argumentList in
      match var with
      | Function qvar ->
	let typ = qvar.qvarTyp in
	let ident = funQvar_to_ident qvar in
	let by_ref = qvar.qvarRef in
	assert (not by_ref);
	(
	  (* ident est soit un string tout seul (une fonction), soit un couple de string (methode ou cons) *)
	  match ident with
	    (* Fonction main *)
	  | Some "main", None ->
	    if typ != TypInt then raise (Error "main doit avoir le type int")
	    else
	      let aux (code, lenv) ins = 
		let inscode, nlenv = compile_ins lenv Smap.empty 8 ins in
		code ++ inscode, nlenv
	      in
	      let lenv = allocate_args 0 p.argumentList in
	      let codemain', _ = 
		List.fold_left aux (codemain ++ save_fp_ra, lenv) b in
	      codefun, la ra alab "main" ++ codemain'

	  (* Fonction quelconque *)
	  | Some s, None -> 
	    (* On ajoute la fonction à la table des fonctions *)
	    let funLabel = new_label () in
	    let typList = List.map (fun arg -> arg.varTyp) argList in
	    Hashtbl.add functionsTable s (sizeof typ, funLabel, typList);
	
	    (* On compile le bloc *)
	    let aux (code, lenv) ins = 
	      let inscode, nlenv = compile_ins lenv Smap.empty 8 ins in
	      code ++ inscode, nlenv
	    in
	    (* On créée l'environnement local lié aux arguments *)
	    let funEnv = allocate_args 0 argList in
	    (* On produit le code de la fonction *)
	    let funCode, _ = List.fold_left aux (nop, funEnv) b in
	    let codefun = 
	          codefun
	      ++  comment (" fonction "^s)
	      ++  label funLabel
	      ++  move fp sp
	      ++  save_fp_ra
	      ++  funCode
	      ++  comment " restauration de ra et fp"
	      ++  restore_ra_fp
	      ++  comment " retour à la case départ"
	      ++  jr ra
	    in
	    (* On renvoie le tout *)
	    codefun, codemain

	  (* Méthode met de la classe cla *)
	  | Some cla, Some met -> (* Méthode *)
	    (* On récupère son profile *)
	    let profile = List.map (fun arg -> arg.varTyp) argList in
	    (* On récupère la classe *)
	    let classObj = Hashtbl.find classTable cla in
	    (* On initialise le label de la méthode *)
	    let metLabel = classObj#add_method_label met profile new_label in
	    (* On produit alors le code *)
	    (* On créée l'environnement liée à la classe.
	       nb : on a this dans les arguments ! *)
	    let classEnv = classObj#offs_var_map in
	    (* Mise à jour de l'env *)
	    let tempEnv = allocate_args 4 argList in
	    let metEnv = Smap.add "this" 0 tempEnv in
	    (* Code de la méthode *)
	    let aux (code, lenv) ins = 
	      let inscode, nlenv = compile_ins lenv classEnv 8 ins in
	      code ++ inscode, nlenv
	    in
	    let metCode, _ = List.fold_left aux (nop, metEnv) b in
	    let codefun = 
	          codefun
	      ++  comment (" méthode "^met^" de la classe "^cla)
	      ++  label metLabel
	      ++  move fp sp
	      ++  save_fp_ra
	      ++  metCode
	      ++  comment " restauration de ra et fp"
	      ++  restore_ra_fp
	      ++  comment " retour à la case départ"
	      ++  jr ra in
	    codefun, codemain
	  | _, _ -> assert false
	)
      | Method (cla, ident)  -> assert false
      | Cons cl-> assert false
    )

let compile p ofile =
  let aux (codefun, codemain) = compile_decl codefun codemain in 
  let codefun, code = List.fold_left aux (nop, nop) p.fichierDecl in
  let strings = Smap.fold 
    (fun lab word data -> data ++ label lab ++ asciiz word) !dataMap nop 
  and globalVars = 
    Hashtbl.fold 
      (fun str (lab, size) code -> code 
	++ comment (" nid douillet de la variable "^str) 
	++ label lab ++ dword [size]) genv nop
  in
  let p =
    { text =
	label "main"
    ++  move fp sp
    ++  code
    ++  comment "sortie du programme"
    ++  li v0 10
    ++  syscall
    ++  codefun;
      data = 
	strings
    ++  globalVars
    ++  label "newline"
    ++  asciiz "\n"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f
