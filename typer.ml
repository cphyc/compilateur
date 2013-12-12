exception Error of string

let rec declVarsTyper t v = match v.Ast.varCont with
  |Ast.VarIdent s -> 
    {
      Tast.varTyp = {Tast.typCont = (
	match t.Ast.typCont with
	|Ast.TypVoid -> Tast.TypVoid
	|Ast.TypInt -> Tast.TypInt
	|Ast.TypIdent st -> Tast.TypIdent st) ; 
		     Tast.typLoc = t.Ast.typLoc};
      Tast.varIdent = s; Tast.varRef = false; 
      Tast.varLoc = v.Ast.varLoc
    }
  |Ast.VarPointer p -> 
    let a = declVarsTyper t p in
    {
      Tast.varTyp = {Tast.typCont = Tast.TypPointer a.Tast.varTyp;
		     Tast.typLoc = a.Tast.varTyp.Tast.typLoc};
      Tast.varIdent = a.Tast.varIdent; Tast.varRef = a.Tast.varRef;
      Tast.varLoc = a.Tast.varLoc
    }
  |Ast.VarReference r -> 
    let a = declVarsTyper t r in
    match a.Tast.varTyp.Tast.typCont with
    |Tast.TypIdent _ ->
      if a.Tast.varRef then raise (Error "Double reference") else
      {
	Tast.varTyp = a.Tast.varTyp;
	Tast.varIdent = a.Tast.varIdent; Tast.varRef = true;
	Tast.varLoc = a.Tast.varLoc
      }
    |_ -> raise (Error "Type mal forme")

let declTyper = function
  | Ast.DeclVars v -> 
    Tast.DeclVars {
      Tast.varList = List.map (declVarsTyper v.Ast.declVarsTyp) v.Ast.varList ;
      Tast.declVarsLoc = v.Ast.declVarsLoc
    }
  | Ast.DeclClass c -> assert false
  | Ast.ProtoBloc (p,b) -> assert false

let file f = assert false
