open Format
open Mips
open Tast

(* Environnement global *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17
(* Ensemble des chaines de caractère *)
module SMap = Set.Make(String)
let stringMap = ref SMap.empty

(*compteur pour de belles étiquettes*)
let labelint = ref 0
let new_label () = labelint := !labelint + 1; 
  "label_"^(string_of_int (!labelint))

let rec compile_expr = function
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


let compile_ins code = function
  | InsSemicolon -> assert false
  | InsExpr e -> assert false
  | InsDef (t,v,d) -> assert false
  | InsIf (e,i) -> assert false
  | InsIfElse (e,i1,i2) -> assert false
  | InsWhile (e,i) -> assert false
  | InsFor (l1,e,l2,i) -> assert false
  | InsBloc b -> assert false
  | InsCout l -> 
    let aux code = function
      | ExprStrExpr e -> 
	let newcode = 
	  (compile_expr e) ++ pop a0 ++ jal "print_int"	in
	code ++ newcode
      | ExprStrStr s -> 
	stringMap := SMap.add s !stringMap;
	(* Il faut maintenant l'afficher *)
	la a0 alab s ++ li v0 4 ++ syscall	
    in
    code ++ (List.fold_left aux nop l)
  | InsReturn e -> assert false

let compile_decl (codefun, codemain) = function
  | DeclVars _ -> assert false
  | DeclClass _ -> assert false
  | ProtoBloc (p, b) -> begin
    match p.protoVar with
    | Qvar (TypInt, QvarQident Ident "main") -> 
       codefun, (List.fold_left compile_ins codemain b)
    | Qvar _ -> assert false
    | Tident _ -> assert false
    | TidentTident _ -> assert false
  end

let compile p ofile =
  let codefun, code = List.fold_left compile_decl (nop, nop) p.fichierDecl in
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
	label "newline"
    ++  asciiz "\n"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f
