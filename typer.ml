(* TODO : récupérer la position dans l'erreur *)
exception Error of string
open Ast

(* On créée un dictionnaire associant à chaque variable son type *)
module Smap = Map.Make(String)
type env = typ Smap.t

(* t est un type, v une variable.*)
let rec declVarTyper t v = match v.Ast.varCont with
  | Ast.VarIdent s -> 
    {
      Tast.varTyp = { Tast.typCont = (
	match t.Ast.typCont with
	| Ast.TypVoid -> Tast.TypVoid
	| Ast.TypInt -> Tast.TypInt
	| Ast.TypIdent st -> Tast.TypIdent st ) ; 
		     Tast.typLoc = t.Ast.typLoc };
      Tast.varIdent = s;
      Tast.varRef = false; 
      Tast.varLoc = v.Ast.varLoc
    }
  | Ast.VarPointer p -> 
    let a = declVarTyper t p in
    {
      Tast.varTyp = { Tast.typCont = Tast.TypPointer a.Tast.varTyp;
		     Tast.typLoc = a.Tast.varTyp.Tast.typLoc };
      Tast.varIdent = a.Tast.varIdent;
      Tast.varRef = a.Tast.varRef;
      Tast.varLoc = a.Tast.varLoc
    }
  | Ast.VarReference r -> 
    let a = declVarTyper t r in
    match a.Tast.varTyp.Tast.typCont with
    | Tast.TypIdent _ ->
      if a.Tast.varRef then raise (Error "Double reference") else
      {
	Tast.varTyp = a.Tast.varTyp;
	Tast.varIdent = a.Tast.varIdent; Tast.varRef = true;
	Tast.varLoc = a.Tast.varLoc
      }
    | _ -> raise (Error "Type mal forme")

(* t est un type, vlist une liste de variable *)
let declVarsTyper t vlist = 
  List.map (declVarTyper t) vlist

(* t est un type, d une déclaration.*)
let declTyper t d = function
  | Ast.DeclVars v -> 
    Tast.DeclVars 
      {
	Tast.varList = declVarsTyper v.Ast.declVarsTyp v.Ast.varList ;
	Tast.declVarsLoc = v.Ast.declVarsLoc
      }
  | Ast.DeclClass c -> 
    Tast.DeclClass
      {
	Tast.className = c.Ast.className;
	Tast.supersOpt = c.Ast.supersOpt;
	(* On a une member list, on la décore avec les types *)
	Tast.memberList = List.map
	  (fun member -> match member with
	  | MemberDeclVars dv -> 
	    let vList = declVarsTyper dv.Ast.declVarsTyp dv.Ast.varList in
	    (* On décore l'arbre avec les types, puis on reconstruit *)
	    Tast.MemberDeclVars 
	      {varList = vList; declVarsLoc = dv.Ast.declVarsLoc }
	  | VirtualProto (virt, proto) when virt -> assert false
	  | VirtualProto (_, proto) -> assert false
	    
	  ) c.Ast.memberList;
	Tast.declClassLoc = c.Ast.declClassLoc;
      }
  | Ast.ProtoBloc (p,b) -> assert false

let file f = assert false
