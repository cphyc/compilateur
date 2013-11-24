%{
  open Ast
  open Tokens
%}

/* Point d'entrée de la grammaire */
%start file

/* Type retourné par le lexer */
%type <Ast.fichier> file

%parameter<Hack: sig
  exception New_ident of string
end>

%{
  open Hack
%}
%%

/* Type général d'un fichier */
file:
 | IOSTREAM; d= decl*;  EOF 
    { {iostr= true; fichierDecl= d; 
      fichierLoc= $startpos, $endpos} } 
 | d= decl*;  EOF 
    { {iostr= false; fichierDecl= d; 
      fichierLoc= $startpos, $endpos} } 
; 

/* Déclarations */
decl:
| e= decl_vars {DeclVars e}
| e= decl_class {DeclClass e}
| p= proto; b= bloc {ProtoBloc (p, b)}
;

decl_vars:
  t= typ; vlist= separated_nonempty_list(COMMA, var) SEMICOLON
   { {declVarsTyp= t; varList= vlist; declVarsLoc= $startpos, $endpos} }

;

decl_class: 
| CLASS; i=IDENT; LBRACE; PUBLIC; COLON; m=member*; RBRACE; SEMICOLON
<<<<<<< HEAD
  {raise (New_ident i)}
| CLASS; i=TIDENT; LBRACE; PUBLIC; COLON; m=member*; RBRACE; SEMICOLON
    {{className= i; supersOpt=None; memberList=m; 
     declClassLoc=$startpos, $endpos} }
=======
   { {className= i; supersOpt=None; memberList=m; 
      declClassLoc=$startpos, $endpos}
      
   }
>>>>>>> 8f842588067c3b0128e1c94a2afcae19b823ef6d
| CLASS; i=IDENT; s0 = supers LBRACE; PUBLIC; COLON; m=member*; 
  RBRACE; SEMICOLON; 
   {raise (New_ident i)}
| CLASS; i=TIDENT; s0 = supers LBRACE; PUBLIC; COLON; m=member*; 
  RBRACE; SEMICOLON; 
   {{className= i; supersOpt= s0; memberList=m; 
      declClassLoc=$startpos, $endpos} }
;

supers:
  COLON; slist= separated_nonempty_list(COMMA, pubtident); { Some slist } 
;

%inline pubtident:
  PUBLIC; t= TIDENT; { t }
;

member:
| e= decl_vars {MemberDeclVars e}
| VIRTUAL; p= proto; SEMICOLON {VirtualProto (true, p)}
| p= proto; SEMICOLON {VirtualProto (false, p)}
;

proto:
| t= typ; q= qvar; LPAREN; args= separated_list(COMMA, argument); RPAREN 
   { {protoVar= Qvar (t, q); argumentList= args;
      protoLoc= $startpos, $endpos} }
| t= TIDENT; LPAREN; args= separated_list(COMMA, argument); RPAREN 
   { {protoVar= Tident t; argumentList= args;
      protoLoc= $startpos, $endpos} }
| t= TIDENT; DCOLON; tmem= TIDENT; 
  args= separated_list(COMMA, argument); RPAREN 
  { {protoVar= TidentTident (t, tmem); argumentList= args; 
     protoLoc= $startpos, $endpos} }
;

/* Déclarations de variables et identificateurs */
typ:
| VOID { {typCont= TypVoid; typLoc= $startpos, $endpos} }
| INT { {typCont= TypInt; typLoc= $startpos, $endpos} }
| s= TIDENT { {typCont= TypIdent s; typLoc= $startpos, $endpos} }
;

argument:
  t= typ; v= var 
   { {argumentTyp= t; argumentVar= v; argumentLoc= $startpos, $endpos} }
;

var:
| i= IDENT { {varCont= VarIdent i; varLoc= $startpos, $endpos} }
| STAR v= var { {varCont= VarPointer v; varLoc= $startpos, $endpos} }
| AMP v= var { {varCont= VarReference v; varLoc= $startpos, $endpos} }
;

qvar:
| q= qident { {qvarCont= QvarQident q; qvarLoc= $startpos, $endpos} }
| STAR q= qvar { {qvarCont= QvarPointer q; qvarLoc= $startpos, $endpos} }
| AMP q= qvar { {qvarCont= QvarReference q; qvarLoc= $startpos, $endpos} }
;

qident:
| i= IDENT {Ident i}
| t= TIDENT DCOLON i= IDENT {IdentIdent (t,i)}
;

/* Expressions */
expr:
| i= CST { {exprCont= ExprInt i; exprLoc= $startpos, $endpos} }
| THIS { {exprCont= This; exprLoc= $startpos, $endpos} }
| FALSE { {exprCont= False; exprLoc= $startpos, $endpos} }
| TRUE { {exprCont= True; exprLoc= $startpos, $endpos} }
| NULL { {exprCont= Null; exprLoc= $startpos, $endpos} }
| q= qident { {exprCont= ExprQident q; exprLoc= $startpos, $endpos} }
| e= expr DOT i= IDENT { {exprCont=ExprDot (e,i); exprLoc= $startpos, $endpos} }
| e= expr POINTER i= IDENT 
   { {exprCont= ExprArrow (e,i); exprLoc= $startpos, $endpos} }
| e1= expr EQ e2= expr 
   { {exprCont= ExprEqual (e1,e2); exprLoc= $startpos, $endpos } }
| e= expr LPAREN elist= separated_list(COMMA, expr) RPAREN
   { {exprCont= ExprApply (e,elist); exprLoc= $startpos, $endpos} }
| NEW t= TIDENT LPAREN elist= separated_list(COMMA, expr) RPAREN
   { {exprCont= ExprNew (t,elist); exprLoc= $startpos, $endpos} }
| DPLUS e= expr { {exprCont= ExprLIncr e; exprLoc= $startpos, $endpos} }
| DMINUS e= expr { {exprCont= ExprLDecr e; exprLoc= $startpos, $endpos} }
| e= expr DPLUS { {exprCont= ExprRIncr e; exprLoc= $startpos, $endpos} }
| e= expr DMINUS { {exprCont= ExprRDecr e; exprLoc= $startpos, $endpos} }
| AMP e= expr { {exprCont= ExprAmpersand e; exprLoc= $startpos, $endpos} }
| EXCL e= expr { {exprCont= ExprExclamation e; exprLoc= $startpos, $endpos} }
| STAR e= expr { {exprCont= ExprStar e; exprLoc= $startpos, $endpos} } 
  %prec USTAR
| MINUS e= expr { {exprCont= ExprMinus e; exprLoc= $startpos, $endpos} } 
  %prec UMINUS
| PLUS e= expr { {exprCont= ExprPlus e; exprLoc= $startpos, $endpos} } 
  %prec UPLUS
    (*On distingue ici les opérateurs unaires.*)
| e1= expr op= operator e2= expr
    { {exprCont= ExprOp(e1,op,e2); exprLoc= $startpos, $endpos} }
| LPAREN e= expr RPAREN 
   { {exprCont= ExprParenthesis e; exprLoc= $startpos, $endpos} }
;

%inline operator:
| DEQ { {opCont= OpEqual; opLoc= $startpos, $endpos} }
| NEQ { {opCont= OpDiff; opLoc= $startpos, $endpos} }
| LT { {opCont= OpLesser; opLoc= $startpos, $endpos} }
| GT { {opCont= OpLesserEqual; opLoc= $startpos, $endpos} }
| LE { {opCont= OpGreater; opLoc= $startpos, $endpos} }
| GE { {opCont= OpGreaterEqual; opLoc= $startpos, $endpos} }
| PLUS { {opCont= OpPlus; opLoc= $startpos, $endpos} }
| MINUS { {opCont= OpMinus; opLoc= $startpos, $endpos} }
| STAR { {opCont= OpTimes; opLoc= $startpos, $endpos} }
| DIV { {opCont= OpDivide; opLoc= $startpos, $endpos} }
| MOD { {opCont= OpModulo; opLoc= $startpos, $endpos} } 
| AND { {opCont= OpAnd; opLoc= $startpos, $endpos} }
| OR { {opCont= OpOr; opLoc= $startpos, $endpos} }
;

/* Définition d'une instruction */
instruction: 
| SEMICOLON { {insCont= InsSemicolon; insLoc= $startpos, $endpos} }
| e= expr SEMICOLON { {insCont= InsExpr e; insLoc= $startpos, $endpos} } 
| t= typ; v= var 
   { {insCont= InsDef (t, v, None); insLoc= $startpos, $endpos} }
| t= typ; v= var; EQ; e= expr SEMICOLON 
   { {insCont= InsDef (t, v, Some (InsDefExpr e)); insLoc= $startpos, $endpos} }
| t= typ; v= var; EQ; ti= TIDENT;
   LPAREN elist= separated_list(COMMA, expr) RPAREN SEMICOLON 
   { {insCont= InsDef(t, v, Some (InsDefIdent (ti, elist)));
     insLoc= $startpos, $endpos} }
| IF LPAREN e= expr RPAREN i= instruction %prec THEN
   { {insCont= InsIf (e,i); insLoc= $startpos, $endpos} }
| IF LPAREN e= expr RPAREN i1= instruction ELSE i2= instruction
   { {insCont= InsIfElse (e,i1,i2); insLoc= $startpos, $endpos} }
| WHILE LPAREN e= expr RPAREN i= instruction 
   { {insCont= InsWhile (e,i); insLoc= $startpos, $endpos} }
| FOR LPAREN elist1= separated_list(COMMA, expr) SEMICOLON e2= expr? 
   SEMICOLON elist3= separated_list(COMMA, expr) RPAREN i= instruction 
   { {insCont= InsFor (elist1, e2, elist3, i); insLoc= $startpos, $endpos} }
| b= bloc { {insCont= InsBloc b; insLoc= $startpos, $endpos} }
| COUT elist= preceded(DLT, separated_nonempty_list(DLT, expr_str)) SEMICOLON 
   { {insCont= InsCout (elist); insLoc= $startpos, $endpos} }
| RETURN e= expr? SEMICOLON 
   { {insCont= InsReturn e; insLoc= $startpos, $endpos} }
;
 
expr_str: 
| e= expr {ExprStrExpr e}
| s= STRING {ExprStrStr s}
;

/* Définition d'un bloc */
bloc:
  LBRACE; ilist= instruction*; RBRACE
    { {blocCont= ilist; blocLoc= $startpos,$endpos} }
;
