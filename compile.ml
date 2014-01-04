exception Error of string
open Format
open Mips
open Tast

(* Environnement global *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Ensemble des chaines de caractère *)
module SMap = Map.Make(String)
let stringMap = ref SMap.empty

(* compteur pour de belles étiquettes *)
let labelint = ref 0
let new_label () = labelint := !labelint + 1; 
  if !labelint = 42 then
    "label_of_the_answer"
  else
    "label_"^(string_of_int (!labelint))

let rec sizeof = function
| TypNull -> assert false
| TypVoid -> assert false
| TypInt -> 4
| TypIdent s -> assert false
| TypPointer t -> sizeof t

(* Méthode générique d'allocation. On alloue dans un sens ou dans l'autre
   selon que l'on traite une variable ou un argument *)
let rec generic_allocate op sp = function
  | [] -> SMap.empty
  | v::vlist -> let s = sizeof v.varTyp in
		let new_sp = op sp (sizeof v.varTyp) in
		SMap.add v.varIdent (sp, s) (generic_allocate op new_sp vlist)
let allocate_args = generic_allocate (fun a b -> a + b)
let allocate_var = generic_allocate (fun a b -> a - b) 

let rec compile_expr ex = match ex.exprCont with
(* Compile l'expression et place le résultat au sommet de la pile *)
| ExprInt i -> li a0 i ++ push a0
| This -> assert false
| False -> assert false
| True -> assert false
| Null -> assert false
| ExprQident e -> assert false
| ExprStar e -> assert false
| ExprDot (e,s) -> assert false
| ExprArrow (e,s) -> assert false
| ExprEqual (e1,e2) -> assert false
| ExprApply (e,l) -> assert false
| ExprNew (s, l) -> assert false
| ExprLIncr e -> assert false
| ExprLDecr e -> assert false
| ExprRIncr e -> assert false
| ExprRDecr e -> assert false
| ExprAmpersand e -> assert false
| ExprExclamation e -> assert false
| ExprMinus e -> assert false
| ExprPlus e -> assert false
| ExprOp (e1,o,e2) -> 
  begin
    let ce1, ce2 = compile_expr e1, compile_expr e2 in
    let calc = ce1 ++ ce2 ++ pop a1 ++ pop a0 in
    let comp_op operator = calc ++ (operator a0 a0 a1) ++ push a0 in
    let arith_op operator = calc ++ (operator a0 a0 oreg a1) ++ push a0 in
    match o with
    | OpEqual -> comp_op seq
    | OpDiff -> comp_op sne
    | OpLesser -> comp_op slt
    | OpLesserEqual -> comp_op sle
    | OpGreater -> comp_op sgt
    | OpGreaterEqual -> comp_op sge
    | OpPlus -> arith_op add
    | OpMinus -> arith_op sub
    | OpTimes -> arith_op mul
    | OpDivide -> arith_op div (*TODO : traiter le cas où e2 est nul -> pas sûr que ce soit nécessaire*)
    | OpModulo -> arith_op rem (*TODO : ^*)
    | OpAnd -> (*Paresseux*)
      let label1, label2 = new_label (), new_label () in
      ce1 ++ pop a0 ++ beqz a0 label1 
      ++ ce2 ++ pop a0 ++ beqz a0 label1
      ++ li a0 1 ++ push a0 ++ b label2 ++ label label1 
      ++ push zero ++ label label2
    | OpOr -> 
      let label1, label2 = new_label (), new_label () in
      ce1 ++ pop a0 ++ bnez a0 label1 
      ++ ce2 ++ pop a0 ++ bnez a0 label1
      ++ push zero ++ b label2 ++ label label1 
      ++ li a0 1 ++ push a0 ++ label label2
  end
| ExprParenthesis e -> compile_expr e

(* sig : code -> lenv -> sp -> code, lenv
   Renvoie le code completé de celui de l'instruction *)
let rec compile_ins code lenv sp = function
(* Nous sommes dans le meilleur des mondes, tout a été typé correctement.
   Les déclarations sont ajoutées à l'environnement local. *)
  | InsSemicolon -> code, lenv
  | InsExpr e -> code ++ compile_expr e, lenv
  | InsDef (t,v, None) -> (* On ajoute alloue de la place pour la variable *)
    let s = sizeof t in
    code ++ popn s, SMap.add v.varIdent (sp, s) lenv
  | InsDef (t,v, Some i) -> assert false
  | InsIf (e,i) -> assert false
  | InsIfElse (e,i1,i2) -> assert false
  | InsWhile (e,i) -> assert false
  | InsFor (l1,e,l2,i) -> assert false
  | InsBloc b -> 
    (* reçoit un couple de code et d'environnement et le met à jour selon ins *)
    let aux (code', lenv) ins =
      let inscode, nlenv = compile_ins code lenv sp ins in
      code' ++ inscode, nlenv
    in
    let inslistcode, nlenv = (List.fold_left aux (nop, lenv) b) in
    code ++ inslistcode, nlenv
  | InsCout l -> 
    let aux (code, lenv) = function
      | ExprStrExpr e -> 
	let newcode = 
	  (compile_expr e) ++ pop a0 ++ jal "print_int"	
	in code ++ newcode, lenv
      | ExprStrStr s ->
	(* TODO : vérifier qu'on n'a pas déjà stocké le string *)
	let lab = new_label () in
	stringMap := SMap.add lab s !stringMap;
	(* Il faut maintenant l'afficher *)
	code ++ la a0 alab lab ++ li v0 4 ++ syscall, lenv
    in
    let inscode, nlenv = (List.fold_left aux (nop, lenv) l) in
    code ++ inscode, nlenv
  | InsReturn e -> assert false
		
let compile_decl codefun codemain = function
  | DeclVars _ -> assert false
  | DeclClass _ -> assert false
  | ProtoBloc (p, b) -> 
    begin
      match p.protoVar with
      | Qvar (typ, QvarQident Ident "main") -> 
	if typ != TypInt then raise (Error "main doit avoir le type int")
	else
	  let aux (code, lenv) = 
	    (* On continue en dessous de fp et sp (offset de 8) *)
	    compile_ins code lenv 8 in
	  (* On ajoute à l'env local les arguments et la taille donne l'offset. *)
	  let lenv = allocate_args 0 p.argumentList in
	  let codemain, _ = List.fold_left aux (codemain, lenv) b in
	  codefun, codemain
      | Qvar _ -> assert false
      | Tident _ -> assert false
      | TidentTident _ -> assert false
    end

let compile p ofile =
  let aux (codefun, codemain) = compile_decl codefun codemain in 
  let codefun, code = List.fold_left aux (nop, nop) p.fichierDecl in
  let p =
    { text =
	label "main"
    ++  move fp sp
    ++  code
    ++  li v0 10
    ++  syscall
    ++  label "print_int"
    ++  li v0 1
    ++  syscall
    ++  jr ra
    ++  codefun;
      data =
    	(* TODO : imprimer tous les string ici *)
	SMap.fold 
	  (fun lab word data -> data ++ label lab ++ asciiz word) !stringMap nop
    ++  label "newline"
    ++  asciiz "\n"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f
