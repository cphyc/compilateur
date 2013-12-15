(* TODO : récupérer la position dans l'erreur *)
exception Error of string
open Ast

(* On créée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
type env = typ Smap.t

(* Simple fonction effectuant la conversion entre Ast et Tast *)
let rec typConverter = function
  | TypVoid -> Tast.TypVoid
  | TypInt -> Tast.TypInt
  | TypIdent s -> Tast.TypIdent s

let rec qidentTyper = function
  | Ident s -> Tast.Ident s
  | IdentIdent (s1, s2) -> assert false

let rec qvarTyper = function
  | QvarQident qident -> Tast.QvarQident (qidentTyper qident)
  | QvarPointer qvar -> assert false
    (* Tast.QvarPointer (qvarTyper qvar) *)
  | QvarReference qvar -> assert false
    (* Tast.QvarReference (qvarTyper qvar) *)

let protoVarTTyper = function
  | Qvar (typ, qvar) -> Tast.Qvar (typConverter typ.typCont, qvarTyper qvar.qvarCont)
  | Tident s -> assert false
  | TidentTident (s1, s2) -> assert false

(* let varTyper v = assert false *)

let argumentTyper arg = assert false
(*   { *)
(*     Tast.argumentTyp = typConverter arg.Ast.argumentTyp; *)
(*     Tast.argumentVar = varTyper arg.Ast.argumentVar; *)
(*     Tast.argumentLoc = arg.Ast.argumentLoc *)
(*   } *)

(* let protoTyper proto =  *)
(*   { *)
(*     Tast.protoVar = protoVarTTyper proto.Ast.protoVar; *)
(*     Tast.argumentList = List.map argumentTyper proto.Ast.argumentList; *)
(*     Tast.protoLoc = proto.Ast.protoLoc  *)
(*   } *)
(* (\* t est un type, v une variable.*\) *)
(* let rec declVarTyper t v = match v.Ast.varCont with *)
(*   | Ast.VarIdent s ->  *)
(*     { *)
(*       Tast.varTyp = { Tast.typCont = ( *)
(* 	match t.Ast.typCont with *)
(* 	| Ast.TypVoid -> Tast.TypVoid *)
(* 	| Ast.TypInt -> Tast.TypInt *)
(* 	| Ast.TypIdent st -> Tast.TypIdent st ) ;  *)
(* 		     Tast.typLoc = t.Ast.typLoc }; *)
(*       Tast.varIdent = s; *)
(*       Tast.varRef = false;  *)
(*       Tast.varLoc = v.Ast.varLoc *)
(*     } *)
(*   | Ast.VarPointer p ->  *)
(*     let a = declVarTyper t p in *)
(*     { *)
(*       Tast.varTyp = { Tast.typCont = Tast.TypPointer a.Tast.varTyp; *)
(* 		     Tast.typLoc = a.Tast.varTyp.Tast.typLoc }; *)
(*       Tast.varIdent = a.Tast.varIdent; *)
(*       Tast.varRef = a.Tast.varRef; *)
(*       Tast.varLoc = a.Tast.varLoc *)
(*     } *)
(*   | Ast.VarReference r ->  *)
(*     let a = declVarTyper t r in *)
(*     match a.Tast.varTyp.Tast.typCont with *)
(*     | Tast.TypIdent _ -> *)
(*       if a.Tast.varRef then raise (Error "Double reference") else *)
(*       { *)
(* 	Tast.varTyp = a.Tast.varTyp; *)
(* 	Tast.varIdent = a.Tast.varIdent; Tast.varRef = true; *)
(* 	Tast.varLoc = a.Tast.varLoc *)
(*       } *)
(*     | _ -> raise (Error "Type mal forme") *)

(* (\* t est un type, vlist une liste de variable *\) *)
(* let declVarsTyper t vlist = *)
(*   List.map (declVarTyper t) vlist *)

let rec exprTyper e = match e.Ast.exprCont with
  | ExprInt i -> Tast.ExprInt i
  | This -> assert false
  | False -> assert false
  | True -> assert false
  | Null -> Tast.Null
  | ExprQident q -> assert false
  | ExprStar e -> assert false
  | ExprDot (e, s) -> assert false
  | ExprArrow (e, s) -> assert false
  | ExprEqual (e1, e2) -> assert false
  | ExprApply (e, el) -> assert false
  | ExprNew (s, el) -> assert false
  | ExprLIncr e -> assert false
  | ExprLDecr e -> assert false
  | ExprRIncr e -> assert false
  | ExprRDecr e -> assert false
  | ExprAmpersand e -> assert false
  | ExprExclamation e -> assert false
  | ExprMinus e -> assert false
  | ExprPlus e -> assert false
  | ExprOp (e1, op, e2) -> assert false
  | ExprParenthesis e -> 
    Tast.ExprParenthesis (exprTyper e)

let expr_strTyper = function
  | ExprStrExpr e -> Tast.ExprStrExpr (exprTyper e)
  | ExprStrStr s -> Tast.ExprStrStr s

let insTyper ins = match ins.Ast.insCont with
  | InsSemicolon -> assert false
  | InsExpr e -> assert false
  | InsDef (typ, var, insDef) -> assert false
  | InsIf (e, ins) -> assert false
  | InsIfElse (e, i1, i2) -> assert false
  | InsWhile (e, i) -> assert false
  | InsFor (el1, eopt, el2, i) -> assert false
  | InsBloc b -> assert false
  | InsCout s -> 
    Tast.InsCout (List.map expr_strTyper s)
  | InsReturn eopt -> assert false

(* t est un type, d une déclaration.*)
let declTyper = function
  | Ast.DeclVars v -> assert false
    (* Tast.DeclVars *)
    (*   { *)
    (* 	Tast.varList = declVarsTyper v.Ast.declVarsTyp v.Ast.varList ; *)
    (* 	Tast.declVarsLoc = v.Ast.declVarsLoc *)
    (*   } *)
  | Ast.DeclClass c -> assert false
    (* Tast.DeclClass *)
    (*   { *)
    (* 	Tast.className = c.Ast.className; *)
    (* 	Tast.supersOpt = c.Ast.supersOpt; *)
    (* 	(\* On a une member list, on la décore avec les types. On traite les *)
    (* 	   différents cas en fonction du type de member qu'on a. *\) *)
    (* 	Tast.memberList = List.map *)
    (* 	  (fun member -> match member with *)
    (* 	  | MemberDeclVars dv -> *)
    (* 	    (\* On décore l'arbre avec les types, puis on reconstruit *\) *)
    (* 	    let vList = declVarsTyper dv.Ast.declVarsTyp dv.Ast.varList in *)
    (* 	    Tast.MemberDeclVars *)
    (* 	      { varList = vList; declVarsLoc = dv.Ast.declVarsLoc } *)
    (* 	  | VirtualProto (virt, proto) -> *)
    (* 	    Tast.VirtualProto (virt, protoTyper proto) *)
	    
    (* 	  ) c.Ast.memberList; *)
    (* 	Tast.declClassLoc = c.Ast.declClassLoc; *)
    (*   } *)
  | Ast.ProtoBloc (p, b) -> 
    (* On type le prototype puis on analyse le bloc *)
    Tast.ProtoBloc 
      ( {
	  Tast.protoVar = protoVarTTyper p.Ast.protoVar ;
	  Tast.argumentList = List.map argumentTyper p.Ast.argumentList;
        },
	List.map insTyper b.Ast.blocCont;
      )

let file f = 
  {
    Tast.iostr = f.Ast.iostr;
    Tast.fichierDecl = List.map declTyper f.Ast.fichierDecl;
  }
   
