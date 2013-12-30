
open Format
open Mips
open Tast

let rec compile_expr = function
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
  let ce1, ce2 = compile_expr e1, compile_expr e2 in
  let op = match o with
    | OpEqual -> assert false
    | OpDiff -> assert false
    | OpLesser -> assert false
    | OpLesserEqual -> assert false
    | OpGreater -> assert false
    | OpGreaterEqual -> assert false
    | OpPlus -> ce1 ++ move a0 a1 ++ ce2 ++ add a0 a0 oreg a1
    | OpMinus -> assert false
    | OpTimes -> assert false
    | OpDivide -> assert false
    | OpModulo -> assert false
    | OpAnd -> assert false
    | OpOr -> assert false
  in
  op
| ExprParenthesis e -> assert false


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
      |ExprStrExpr e -> 
	let newcode = (compile_expr e) ++ pop a0 
	  ++ jal "print_int"
	in
	code ++ newcode
      |ExprStrStr s -> assert false
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
	label "newline"
      ++  asciiz "\n"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f
