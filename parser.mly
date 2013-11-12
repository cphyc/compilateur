%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token CLASS ELSE FALSE FOR IF INT NEW NULL PUBLIC RETURN THIS TRUE VIRTUAL VOID
%token WHILE ASSIGN OR AND EQ NEQ LT LE GT GE PLUS MINUS STAR DIV MOD NEG INCR
%token DECR LPAREN RPAREN DOT CALL POINTER AMP COMMA COLON SEMICOLON EOF

/* Associativity, priority */
%left OR
%left AND
%nonassoc NOT
%left MINUS PLUS
%left TIMES DIV
%nonassoc THEN
%nonassoc ELSE

/* Entry point for grammary */
%start prog

/* Type of the lexer's returned values */
%type <Ast.declaration list> file
%%

file:
  "#include <iostream>)"?; d = decl*;  EOF { d }
; 

decl:
| e = decl_vars {}
| e = decl_class {}
| e = proto ; bloc
;

decl_vars:
  t = typ; vlist = separated_list(COMMA, var) { }

;
 
decl_class: 
  CLASS; i = ident; supers? LBRACE PUBLIC COLON m = member* RBRACE SEMICOLON
   {}
;

supers:
  COLON slist = separated_list(COMMA, PUBLIC TIDENT) {}
;

member:
| e = decl_vars {}
| VIRTUAL? p = proto SEMICOLON {}
;

proto: 
  (typ qvar | tident | tident COLON COLON tiden) LPAREN 
  a = separated_list(COMMA, argument) {}
;

typ:
| VOID {}
| INT {}
| tiden {}
;

argument:
  t = typ; v = var {}
;

var:
| i = ident {}
| STAR v = var {}
| AMP v = var {}
;

qvar:
| q = qident {}
| STAR q = qvar {}
| AMP q = qvar {}
;

qident:
| i = ident {}
| t = tident COLON COLON i = ident {}
;

expr:
| i = int {}
| THIS
| FALSE
| TRUE
| NULL
| q = qident {}
| STAR e = expr {}
| e = expr DOT i = ident {}
| e = expr POINTER i = ident {}
| e1 = expr ASSIGN e2 = expr {}
| e = expr LPAREN elist = separated_list(COMMA, expr) RPAREN {}
| NEW t = tident LPAREN elist = separated_list(COMMA, expr) RPAREN {}
| DPLUS e = expr {} 
| DMINUS e = expr {}
| e = expr DPLUS {}
| e = expr DMINUS {}
| AMP e = expr {}
| NEG e = expr {}
| MINUS e = expr {}
| PLUS e = expr {}
| e1 = expr; op = operator; e2 = expr {}
| LPAREN e = expr RPAREN {}
;

operator:
| EQ {}
| NEQ {}
| LT {}
| GT {}
| LE {}
| GE {}
| PLUS {}
| MINUS {}
| STAR {}
| DIV {}
| MOD {} 
| AND {}
| OR {}
;

instruction:
| SEMICOLON {}
| e = expr {}
| t = typ; v = var; 
(EQ e = expr | EQ t = tident LPAREN elist = separated_list(COMMA, EXPR))? SEMICOLON
{}
| IF LPAREN e = expr RPAREN i = instruction {}
| IF LPAREN e = expr RPAREN i1 = instruction ELSE i2 = instructin {}
| WHILE LPAREN e = expr RPAREN i = instruction {}
| FOR LPAREN elist1 = separated_list(COMMA, expr) SEMICOLON e2 = expr? 
   SEMICOLON elist3 = separated_list(COMMA, expr) RPAREN i = instruction {}
| b = block {}
| "std::cout" elist = separated_list(LE LE, expr_str) SEMICOLON {}
| RETURN e = expr? SEMICOLON {}
;

expr_str:
| e = expr {}
| s = string {}
;

block:
  LBRACE i = instruction* RBRACE
;
