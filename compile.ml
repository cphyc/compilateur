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

(* Table de hashage associant à un identificateur sa taille, son label 
   et la taille de ses args *)
let functionsTable: (string, int * string * (typ*bool) list) Hashtbl.t = 
  Hashtbl.create 17

(* Associe à (classe*méthode)*profile un label *)
let methodPosition: ((string*string)*(typ*bool) list, string) Hashtbl.t 
    = Hashtbl.create 17
(* Associe à (classe*profile un label *)
let consPosition: ((string)*(typ*bool) list, string) Hashtbl.t = Hashtbl.create 17

(* Tables des méthodes virtuelles du segment de donnée *)
(* classe, (liste des méthodes*position dans la vm)*label *)
let virtualMethodTable: (string, (string*int) list*string) Hashtbl.t = Hashtbl.create 7

let print_profile profile = List.iter (fun t -> match t with
  | TypNull -> printf "\t TypeNull@."
  | TypVoid -> printf "\t TypeVoid@."
  | TypInt -> printf "\t TypeInt@."
  | TypIdent s -> print_string ("\t Classe"^s);printf "@.";
  | TypPointer _ -> ()) profile

let rec eq_profile = Typer.eqProf 

(* "pushn size" empile "size" octets sur la pile *)
let pushn = sub sp sp oi

(* compteur pour de belles étiquettes *)
let labelint = ref 0
let new_label () = labelint := !labelint + 1; 
  if !labelint = 42 then
    "label_of_the_answer"
  else
    "label_"^(string_of_int (!labelint))

class methodObject l r t s v = object (self)
  val lab:string option = l
  val is_ref:bool = r 
  val typ : typ = t
  val profile : (typ*bool) list = s
  val virt : bool = v
  end

class consObject l t p = object
  val lab:string = l
  val typ:typ = t
  val profile:(typ*bool) list = p
  val mutable code : text option = None
  method profile = p
  method code = code
end

let consTable: (string, consObject) Hashtbl.t = Hashtbl.create 17

class classObject ident = object (self)
  val name : string = ident
  val fields = Hashtbl.find_all Typer.classFields ident
  val parents = Hashtbl.find_all Typer.classInheritances ident
  val methods = Hashtbl.find_all Typer.classMethods ident
  val constructors = Hashtbl.find_all Typer.classCons ident
  val mutable positionMap = Smap.empty
  val mutable size = 0
  method name = name
  method get_fields = fields
  method get_parents = parents
  method size = size
  (* Appelle le constructeur correspondant et tous les autres *)
  method init (profile : (Tast.typ * bool) list) = 
    (* Si on a défini un constructeur, on n'en a pas par défaut *)
    match Hashtbl.find_all consTable self#name with
    | [] -> (* Aucun constructeur, on renvoie le code par défaut, càd rien *)
      let superConstructor = nop in
          comment " constructeur par défaut : allocation de mémoire"
      ++  pushn self#size
      ++  comment " constructeur par défaut : appel des supers constructeurs en chaine"
      ++  superConstructor
    | consList -> (* Il y en a, on cherche le bon (n'échoue pas) *)
      let constructor = List.find (fun consObj -> eq_profile profile consObj#profile) consList in
      match constructor#code with None -> assert false | Some code -> 
	    comment (" appel d'un constructeur personnalisé:allocation 
                       de mémoire puis exécution du code")
	++  pushn size 
	++  code
  method map =
    (* Smap.iter (fun name (size,pos) -> print_string (name^" - size: "); print_int size; *)
    (*   print_string "  pos: "; print_int pos; printf "@.";) positionMap; *)
    positionMap 
  method no_size_map = Smap.map (fun (size,position) -> position) positionMap
   method build sizeof =
  (*   (\* On construit les tables des méthodes virtuelles *\) *)
  (*   let build_vb class_name =  *)
  (*     (\* On récupère toutes les méthodes de la classe *\) *)
  (*     let methods = Hashtbl.find_all Typer.classMethods class_name in *)
  (*     (\* On ne garde que celles qui sont virtuelles *\) *)
  (*     let virtual_methods = List.filter  *)
  (* 	(fun m -> (\* On récupère les classes qui sont virtuelles *\) *)
  (* 	  let(((_,virt),_),_) = Hashtbl.find Typer.methodsTable (class_name, m) in *)
  (* 	  virt) methods *)
  (*     in *)
  (*     (\* On transforme la liste de méthodes en une map de position*méthode *\) *)
  (*     let map = List.fold_left  *)
  (* 	(fun virtMet (map, position) -> Smap.add virtMet position map, position+4) *)
  (* 	Smap.empty virtual_methods  *)
  (*     in *)
  (*     (\* On alloue un nombre d'octets dans le segment de donnée, *)
  (* 	 c'est le nombre de méthodes virtuelles*\) *)
  (*     let vmLabel = new_label () in *)
  (*     Hashtbl.add virtualMethodTable class_name map *)
  (*   in *)
						
    (* On construit les positions des variables + du pointeur vers la table 
       de méthode virtuelle ! *)
    let rec explore first_free classe_name = 
      (* let _ = printf "@.Entrée de explore\tclasse:\t"; *)
      (* print_string classe_name; printf "@."; *)
      (* 	printf "\t\t\toffset:\t"; print_int first_free; printf "@."; *)
      (* 	printf "\t\t\tvariables:\t"; *)
      (* in *)
      let parents = Hashtbl.find_all Typer.classInheritances classe_name in
      (* let _ = printf "####>@."; *)
      (* 	List.iter (fun paren -> print_string paren; printf "@.") parents; *)
      (* 	printf "<####@."; *)
      (* in *)
      let fields = Hashtbl.find_all Typer.classFields classe_name in
      (* tant qu'il reste des parents, on les explore *)
      let rec explore_parent first_free = function 
	| [] -> first_free, Smap.empty
	| parent::list -> (* On alloue de la place pour le parent, 
			     puis on explore le reste *)
	  (* let _ = print_string ("Exploration de parent "^parent); printf "@." in *)
	  (* On créée la table des méthodes virtuelles de la classe *)
	  (* let () = build_vb parent in *)
	  let offset, map = explore first_free parent in
	  let ending_offset, map' = explore_parent offset list in
	  (* On fusionne les deux maps *)
	  (* let _ = print_string ("Fin de l'exploration de "^parent);printf "@." in *)
	  (* On ajoute un petit label à l'endroit où est ajoutée la classe pour s'y
	     retrouver :D *)
	  let mapWithLabelIsTotallyCool = Smap.add parent (sizeof false (TypIdent parent), 
							   first_free) map' in

	  ending_offset, Smap.fold Smap.add map mapWithLabelIsTotallyCool
      in		  
      let rec add_fields allocated_map first_free = function
	| [] -> first_free, allocated_map
	| (name, (typ,b))::list -> (* On recherche dans la map si on n'a pas déjà ajouté le champ, 
				      sinon on le fait *)
	  if Smap.mem name allocated_map then
	    (* let _ = printf "Ach nein !@." in *)
	    first_free, allocated_map 
	  else
	    (* On récupère la taille, on calcule le nouvel emplacement disponible *)
	    let size = sizeof b typ in
	    (* let _ = print_string (name^" "); print_int first_free; printf "\t" in *)
	    let new_first_free = first_free + size in
	    (* On récupère tout le touintouin avec comme premier emplacement dispo celui juste au dessus 
	       (on a gardé de la place pour la variable *)
	    let finally_free, map = 
	      add_fields (Smap.add name (size, first_free) allocated_map) new_first_free list in
	    finally_free, map
      in
      (* On explore effectivement les parents *)
      let offset, parent_map = explore_parent first_free parents in
      (* On ajoute alors les champs en prenant comme point de départ parent_map *)
      add_fields parent_map offset fields
    in
    let calculated_size, map = explore 0 self#name in
    positionMap <- map;
    size <- calculated_size;
    (* (\* On créée le code d'initialisation d'une classe *\) *)
    (* let code =  *)
    (* initCode <- Smap.fold (fun ident size code ->  *)
    (*   code  *)
    (*   ++  comment ("Membre "^ident) *)
    (*   ++  pushn size) memberListEnv nop; *)
    (* map <- offsEnv; *)
    (* declClass <- c; *)
    (* size <- first_free; *)
end

(* Une classe : une déclaration, une map des tailles*positions, un code 
   d'initialisation, une taille *)
let classTable: (string, classObject) Hashtbl.t = Hashtbl.create 17

(* Associe à variable de classe sa map *)
let classVars: (string, (int*int) Smap.t) Hashtbl.t = Hashtbl.create 17
let vrai = 1
let faux = 0

(********************* Utilitaires ********************)
(* renvoie la taille d'un type *)
let rec sizeof rf t = 
  if rf then 4
  else match t with
  | TypNull -> 4
  | TypVoid -> 0 
  | TypInt -> 4
  | TypIdent s -> (Hashtbl.find classTable s)#size
  | TypPointer t -> 4
 
(* On récupère 4 caractères, si c'est du type "\x hexa hexa", on remplace *)
let get_next_four s pos = 
  try Some ((String.sub s pos 2), (String.sub s (pos+2) 2))
  with Invalid_argument _ -> None


let rec find_xhexa pos s =
  match s.[pos] with
  | '\\' ->(
    match s.[pos+1] with
    | 'x' -> (
      let theEnd = String.sub s (pos+2) 2 in
	  (
	    let theCode = int_of_string ("0x"^theEnd) in
	    theCode
	  )
    )
    | _ -> raise (failwith "")
  )
  | _ -> raise (failwith "")

(* Transforme les codes \x__ dans le code ascii qui convient *)
let replace_xhexa string =
  let rec run pos before string =
      (* On vérifie qu'on n'est pas en bout de liste *)
      if pos < String.length string then
	(
      (* On est en pos, on teste replace_xhexa *)
	  let carac, new_pos = 
	    try String.make 1 (char_of_int 
				 (find_xhexa pos string)), pos+4
	    with _ -> String.make 1 string.[pos], pos+1
	  in
	  let before = before ^ carac in
	  (run new_pos before string)
	)
      else before  
  in
  run 0 "" string
  
let save_fp_ra = 
      comment " sauvegarde de fp" 
  ++  push fp
  ++  comment " sauvegarde de ra"
  ++  push ra
let restore_ra_fp = 
      comment " restauration de ra"
  ++  lw ra areg (-8, fp) 
  ++  comment " restauration de fp"
  ++  lw fp areg (-4, fp)

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
let allocate_args use_this argList =
  let init = 
    if use_this then Smap.singleton "this" 0, 4
    else Smap.empty, 0
  in
  let env, _ = List.fold_left 
    (fun (env, first_free) var -> 
      let new_first_free = first_free + (sizeof var.varRef var.varTyp) in
      Smap.add var.varIdent first_free env, new_first_free)
    init argList in
  env

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
  Smap.add v.varIdent (offset - (sizeof v.varRef v.varTyp)) lenv
    
let funQvar_to_ident q = match q.qvarIdent with
    | Ident s -> Some s, None
    | IdentIdent (s1, s2) -> Some s1, Some s2

(******************** Compilation ********************)
let rec compile_LVexpr lenv cenv ex = match ex.exprCont with
  | ExprQident (rf, q) -> begin match q with
    | Ident s when rf -> 
      (* Lâche copier-coller *)
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
    | Ident s ->
      begin
	if Smap.mem s lenv then (* Variable locale *)
	  let pos = Smap.find s lenv in 
	       comment (" variable locale "^s) 
	    ++ add a0 fp oi pos 
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
  | ExprStar e -> 
    compile_expr lenv cenv e 
  | ExprDot (e,s) -> 
    (match e.exprTyp with 
    | TypIdent c -> (* On a une classe *)
      let _, offset = Smap.find s (Hashtbl.find classTable c)#map in
          comment (" Variable de class "^s)
      ++  compile_LVexpr lenv cenv e
      ++  pop a0              (* on a l'adresse % fp, et l'offset *)
      ++  add a0 a0 oi offset (* on a dans a0 l'adresse de la variable *)
      ++  push a0             (* on pousse l'adresse variable *)
    | TypPointer (TypIdent c) -> assert false
    | _ -> assert false
    )
  | _ -> assert false

and compile_expr lenv cenv ex = match ex.exprCont with
(* Compile l'expression et place le résultat au sommet de la pile *)
  | ExprInt i -> li a0 i ++ push a0
  | This -> assert false
  | Null -> 
        li a0 0
    ++  push a0
  | ExprQident (rf, q) -> begin match q with 
    | Ident s when rf -> 
      (* Lâche copier-coller *)
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
	  let lab, _ = try Hashtbl.find genv s with _ -> 
	    raise (Error ("pas trouvé "^s)) in 
	  lw a0 alab lab
      in
      comment (" chargement variable "^s) ++ instruction
      ++ push a0   
      ++ pop a1
      ++ lw a0 areg (0, a1)
      ++ push a0
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
  | ExprStar e -> 
    compile_expr lenv cenv e
    ++ pop a1
    ++ lw a0 areg (0, a1)
    ++ push a0
  | ExprDot (e,s) -> (* On récupère l'adresse de e comme une lvalue, puis on
			calcul l'offset de s *)
    (match e.exprTyp with 
    | TypIdent c -> 
      let _, offset = Smap.find s (Hashtbl.find classTable c)#map in
          comment (" Variable de class "^s)
      ++  compile_LVexpr lenv cenv e
      ++  pop a0              (* on a l'adresse % fp, et l'offset *)
      ++  add a0 a0 oi offset (* on a dans a0 l'adresse de la variable *)
      ++  lw a0 areg (0, a0)  (* on charge la variable dans a0 *)
      ++  push a0
    | _ -> assert false
    )
  | ExprEqual (e1,e2) -> (* On compile l'expression e1, c'est une lvalue donc 
			    le résultat est son adresse *)
    compile_LVexpr lenv cenv e1
    ++ comment " calcul de la valeur droite" ++ compile_expr lenv cenv e2
    ++ pop a1 ++ pop a0 
    ++ comment " sauvegarde de la valeur" ++ sw a1 areg (0, a0)
  | ExprApply (e, p, l) -> ( 		(* cadeau : p profil cherché *)
    match e.exprCont with
    | ExprQident (rf, Ident s) -> (* appel de fonction *)
      (* On recherche le profil correspondant *)
      let size, funlab, typList = 
	List.find (fun (s, l, tl) -> eq_profile tl p)
	  (Hashtbl.find_all functionsTable s) in
      let rec sizeArg = function
	| [] -> 0
	| (t,r)::l -> (sizeof r t) + sizeArg l
      in
      let rec codeArgs prof exprl = match (prof, exprl) with
	| [], [] -> nop
	| (_,rf)::prof', e::exprl' ->
	  let code = codeArgs prof' exprl' in
	  if rf 
	  then code ++ (compile_LVexpr lenv cenv e)
	  else code ++ (compile_expr lenv cenv e)
	| _ -> assert false
      in
      let to_push_or_not_to_push = 
	if Typer.typEq e.exprTyp TypVoid then
	  nop
	else push v0
      in
          comment (" appel de la fonction "^s)
      ++  comment "  construction de la pile"
      ++  codeArgs p l
      ++  save_fp_ra
      ++  comment " saut vers la fonction"
      ++  jal funlab
      ++  comment " on est revenu, on dépile par rapport à l'ancien fp"
      ++  add sp fp oi (sizeArg typList)
      ++  restore_ra_fp
      ++  comment "  on met le résultat sur la pile"
      ++  to_push_or_not_to_push
    | ExprQident (rf, IdentIdent (s1,s2)) when s1==s2 -> assert false(* appel de cons *)
    | ExprQident (rf, IdentIdent (s1,s2)) -> assert false (* appel de méthode *)
    | ExprParenthesis e -> assert false (* compile_expr lenv {ExprApply (e,l)} *)
    | ExprDot (e,s) -> 
      (* On compile l'adresse de e *)
      (* let get_e_address = compile_LVexpr lenv cenv e.exprCont ++ pop a0 in *)
      (* On trouve la classe grace au type *)
      let className = match e.exprTyp with | TypIdent s -> s | _ -> assert false in
      (* On récupère directement le label de la méthode *)
      let metLab = Hashtbl.find methodPosition ((className, s), p) in
      (* On compile la pile d'argument *)
      let codeArgs = 
	List.fold_right 
	  (fun expr code -> code ++ compile_expr lenv cenv expr) l nop
      in
          comment (" appel de la méthode "^s^" de la classe "^className)
      ++  comment "  construction de la pile"
      ++  codeArgs
      ++  comment "  sauvegarde de this (compile l'expression de gauche et laisse
                     le résultat sur la pile"
      ++  compile_LVexpr lenv cenv e
      ++  comment "  sauvegarde de fp, sp"
      ++  move fp sp
      ++  save_fp_ra
      ++  comment "  code de la méthode"
      ++  jal metLab
      ++  comment "  restauration de ra et fp"
      ++  restore_ra_fp
      ++  comment "  résultat sur la pile"
      ++  push v0
    | _ -> assert false
  )
  | ExprNew (s, l) -> assert false
  | ExprRIncr e ->(* compile e, le renvoie puis l'incrémente *)
        compile_LVexpr lenv cenv e
    ++  comment " adresse de l'expression"
    ++  pop a0
    ++  comment " on récupère la valeur"
    ++  lw a1 areg (0, a0)
    ++  comment " on la place sur la pile"
    ++  push a1
    ++  comment " on incrémente"
    ++  add a1 a1 oi 1 
    ++  sw a1 areg (0, a0)
  | ExprLIncr e -> (* compile e, l'incrément puis le renvoie *)
        compile_LVexpr lenv cenv e
    ++  comment " adresse de l'expression"
    ++  pop a0                (* adresse de l'expression *)
    ++  comment " on récupère la valeur"
    ++  lw a1 areg (0, a0)
    ++  comment " on incrémente"
    ++  add a1 a1 oi 1 
    ++  sw a1 areg (0, a0)
    ++  comment " on la place sur la pile"
    ++  push a1
  | ExprRDecr e ->
        compile_LVexpr lenv cenv e
    ++  comment " adresse de l'expression"
    ++  pop a0
    ++  comment " on récupère la valeur"
    ++  lw a1 areg (0, a0)
    ++  comment " on la place sur la pile"
    ++  push a1
    ++  comment " on décrémente"
    ++  sub a1 a1 oi 1 
    ++  sw a1 areg (0, a0)

  | ExprLDecr e -> (* On compile l'expression, on ajoute un et on la stocke *)
        compile_LVexpr lenv cenv e
    ++  comment " adresse de l'expression"
    ++  pop a0
    ++  comment " on récupère la valeur"
    ++  lw a1 areg (0, a0)
    ++  comment " on décrémente"
    ++  sub a1 a1 oi 1 
    ++  sw a1 areg (0, a0)
    ++  comment " on la place sur la pile"
    ++  push a1
  | ExprAmpersand e -> 
    compile_LVexpr lenv cenv e
  | ExprExclamation e ->
    let lab2, lab1 = new_label (), new_label () in
    compile_expr lenv cenv e
    ++ comment " Négation logique" ++ pop a0 ++ beqz a0 lab1
    ++ comment "  cas non nul :" ++ li a0 faux ++ push a0 ++ b lab2
    ++ label lab1 ++ comment "  cas nul :" ++ li a0 vrai ++ push a0
    ++ label lab2
  | ExprMinus e -> compile_expr lenv cenv e ++ pop a0 
    ++ comment " Négation arithmétique" ++ neg a0 a0 ++ push a0
  | ExprPlus e -> compile_expr lenv cenv e
  | ExprOp (e1,o,e2) -> 
    begin
      let ce1, ce2 = compile_expr lenv cenv e1, compile_expr lenv cenv e2 in
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
  | ExprParenthesis e -> compile_expr lenv cenv e

(* lenv est de type offset*registre Smap.t *)
let rec compile_ins lenv cenv sp = function
  | InsSemicolon -> nop, lenv
  | InsExpr e -> 
       compile_expr lenv cenv e 
    ++ popn (sizeof false e.exprTyp), lenv
  | InsDef (v, option) when v.varRef ->
    let comm = comment (" allocation de la reference "^v.varIdent) in
    let nlenv = allocate_var v lenv in
    let rhs = match option with
      | None -> pushn 4
      | Some InsDefExpr e -> compile_LVexpr nlenv cenv e
      | Some InsDefIdent (c, elist) -> (* Appel du constructeur *)
	let profile = List.map (fun e -> (e.exprTyp, false)) elist in
	(Hashtbl.find classTable c)#init profile
    in
    comm ++ rhs, nlenv   
  | InsDef (v, option) ->
    let comm = comment (" allocation de la variable "^v.varIdent) in
    let s =  sizeof v.varRef v.varTyp in
    let nlenv = allocate_var v lenv in
    let rhs = match option with
      | None -> pushn s
      | Some InsDefExpr e -> compile_expr nlenv cenv e
      | Some InsDefIdent (c, elist) -> (* Appel du constructeur *)
 	let profile = List.map (fun e -> (e.exprTyp, false)) elist in
	(Hashtbl.find classTable c)#init profile
    in
    comm ++ rhs, nlenv
  | InsIf (e,i) -> (* astuce de faineant *)
    compile_ins lenv cenv sp (InsIfElse(e,i,InsSemicolon))
  | InsIfElse (e,i1,i2) -> 
    let if_true, way_out = new_label (), new_label () in
    let ins1, _ = compile_ins lenv cenv sp i1 in
    let ins2, _ = compile_ins lenv cenv sp i2 in
        compile_expr lenv cenv e
    ++  pop a0 
    ++  bnez a0 if_true 
    ++  comment " si faux :" 
    ++  ins2
    ++  b way_out
    ++  comment " si vrai :" 
    ++  label if_true 
    ++  ins1 
    ++  label way_out, lenv
  | InsWhile (e,i) -> 
    let way_in, way_out = new_label(), new_label() in
    let ins1, _ = compile_ins lenv cenv sp i in
    comment " entrée de la boucle while" ++ label way_in 
    ++ comment " test du while" ++ compile_expr lenv cenv e ++ pop a0 ++ beqz a0 way_out
    ++ comment " coeur du while" ++ ins1 ++ b way_in
    ++ comment " sortie de la boucle while" ++ label way_out, lenv
  | InsFor (l1,e,l2,i) -> 
    let compile_expr_list = 
      List.fold_left (fun code e -> code ++ compile_expr lenv cenv e) nop in
    let init = compile_expr_list l1 in
    let test = compile_expr lenv cenv e in
    let modify = compile_expr_list l2 in
    let core, pouet = compile_ins lenv cenv sp i in
    (* print_string "i :"; Format.print_int (Smap.find "i" pouet); printf "@."; *)
    (* print_string "cpt :"; Format.print_int (Smap.find "cpt" pouet); printf "@."; *)
    (* print_string "j :"; Format.print_int (Smap.find "j" pouet); printf "@."; *)
    
    let labtest, way_out = new_label (), new_label () in
       comment " initialisation de la boucle for" 
    ++ init
    ++ comment " test de sa condition" 
    ++ label labtest 
    ++ test 
    ++ pop a0 
    ++ beqz a0 way_out
    ++ comment " coeur de la boucle for" 
    ++ core
    ++ comment " modification des paramètres testés" 
    ++ modify 
    ++ b labtest
    ++ comment " sortie de la boucle for" 
    ++ label way_out, lenv
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
	  (compile_expr lenv cenv e) ++ pop a0 ++ print_int
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
      | Some e -> compile_expr lenv cenv e
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
	Hashtbl.add genv var.varIdent (new_label (), sizeof var.varRef var.varTyp);
	process vlist
    in
    process vlist;
    nop, nop
  | DeclClass c -> (* On se contente juste d'ajouter la classe dans la table *)
    let newClass = new classObject c.className in
    newClass#build sizeof;
    Hashtbl.add classTable c.className newClass;
    nop, nop
 
  | ProtoBloc (p, b) -> 
    (
      let var, argList = p.protoVar, p.argumentList in
      match var with
      | Qvar qvar ->
	let typ = qvar.qvarTyp in
	let ident = funQvar_to_ident qvar in
	let by_ref = qvar.qvarRef in
	assert (not by_ref);
	(
	  (* ident est soit un string tout seul (une fonction),
	     soit un couple de string (methode ou cons) *)
	  match ident with
	    (* Fonction main *)
	  | Some "main", None ->
	    if typ != TypInt then raise (Error "main doit avoir le type int")
	    else
	      let aux (code, lenv) ins = 
		let inscode, nlenv = compile_ins lenv Smap.empty 8 ins in
		code ++ inscode, nlenv
	      in
	      let lenv = allocate_args false p.argumentList in
	      let codemain', _ = 
		List.fold_left aux (save_fp_ra ++ codemain, lenv) b in
	      codefun, codemain'

	  (* Fonction quelconque *)
	  | Some s, None -> 
	    (* On ajoute la fonction à la table des fonctions *)
	    let funLabel = new_label () in
	    let typList = 
	      List.map (fun arg -> (arg.varTyp, arg.varRef)) argList in
	    Hashtbl.add functionsTable s (sizeof qvar.qvarRef typ, 
					  funLabel, typList);
	
	    (* On compile le bloc *)
	    let aux (code, lenv) ins = 
	      let inscode, nlenv = compile_ins lenv Smap.empty 8 ins in
	      code ++ inscode, nlenv
	    in
	    (* On créée l'environnement local lié aux arguments *)
	    let funEnv = allocate_args false argList in
	    (* On produit le code de la fonction *)
	    let funCode, _ = List.fold_left aux (nop, funEnv) b in
	    let codefun = 
	          codefun
	      ++  comment (" fonction "^s)
	      ++  label funLabel
	      ++  comment " on a sauvegardé fp et ra, on a donc sp à 8 de fp, c'est ce qu'on dit !"
	      ++  add fp sp oi 8
	      ++  funCode
	      ++  comment " retour à la case départ"
	      ++  jr ra
	    in
	    (* On renvoie le tout *)
	    codefun, codemain

	  (* Méthode met de la classe cla *)
	  | Some cla, Some met -> (* Méthode *)
	    (* On récupère son profile *)
	    let profile = 
	      List.map (fun arg -> (arg.varTyp, arg.varRef)) argList in
	    (* On initialise le label de la méthode *)
	    let metLabel = new_label () in
	    (* On l'ajoute dans la table *)
	    Hashtbl.add methodPosition ((cla,met),profile) metLabel;
	    (* On produit alors le code *)
	    (* On créée l'environnement liée à la classe.
	       nb : on a this dans les arguments ! *)
	    let classEnv = (Hashtbl.find classTable cla)#no_size_map in
	    (* Mise à jour de l'env *)
	    let tempEnv = allocate_args true argList in
	    let metEnv = Smap.add "this" 0 tempEnv in
	    (* Code de la méthode *)
	    let metCode, _ = List.fold_left 
	      (fun (code, lenv) ins -> 
		let inscode, nlenv = compile_ins lenv classEnv 8 ins in
		code ++ inscode, nlenv) (nop, metEnv) b
	    in   
	    let new_codefun = 
	          codefun
	      ++  comment (" méthode "^met^" de la classe "^cla)
	      ++  label metLabel
	      ++  metCode
	      ++  comment " retour à la case départ"
	      ++  jr ra in
	    new_codefun, codemain
	  | _, _ -> assert false
	)
      | Tident s -> (* Constructeur *)
	(* On récupère son profil *)
	let profile = List.map (fun arg -> arg.varTyp,arg.varRef) argList in
	(* On initialise le label du constructeur *)
	let consLabel = new_label () in
	(* On l'ajoute dans la table *)
	Hashtbl.add consPosition (s, profile) consLabel;
	let classEnv = (Hashtbl.find classTable s)#no_size_map in
	(* Mise à jour de l'env *)
	let tempEnv = allocate_args true argList in
	let consEnv = Smap.add "this" 0 tempEnv in
	(* On produit le code *)
	let consCode, _ = List.fold_left
	  ( fun (code, lenv) ins ->
	    let inscode, nlenv = compile_ins lenv classEnv 8 ins in
	    code ++ inscode, nlenv) (nop, consEnv) b
	in
	let new_codefun =
	      codefun
	  ++  comment (" constructeur de "^s)
	  ++  label consLabel
	  ++  consCode
	  ++  comment " retour à la case départ"
	  ++  jr ra 
	in
	new_codefun, codemain
    )

let compile p ofile =
  let aux (codefun, codemain) = compile_decl codefun codemain in 
  let codefun, code = List.fold_left aux (nop, nop) p.fichierDecl in
  let strings = Smap.fold 
    (fun lab word data -> 
      let unescaped = replace_xhexa word in
      data ++ label lab ++ asciiz unescaped) !dataMap nop 
  and globalVars = 
    Hashtbl.fold 
      (fun str (lab, _) code -> code 
	++ comment (" nid douillet de la variable "^str) 
	++ label lab 
	++ dword [0]) genv nop
  in
  (* let virtualTableCode =  *)
  (*   Hashtbl.fold *)
  (*     (fun str (methodList, lab) code ->  *)
  (* 	(\* On construit une liste de 0 de la taille le nombre de méthode *\) *)
  (* 	let liste = List.fold_left (fun l _ -> 0::l) [] methodList in *)
  (* 	   code *)
  (* 	++ comment (" table de méthode virtuelle de la classe "^str) *)
  (* 	++ label lab *)
  (* 	++ dword liste) virtualMethodTable nop *)
  (* in *)
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
	comment " Chaines de caractères"
    ++  strings
    ++  label "newline"
    ++  asciiz "\n"
    ++  comment " Variables globales"
    ++  globalVars
    (* ++  comment " Tables des méthodes virtuelles" *)
    (* ++  virtualTableCode *)
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f
