%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token <string> TIDENT
%token <string> STRING
%token IOSTREAM COUT
%token CLASS ELSE FALSE FOR IF INT NEW NULL PUBLIC RETURN THIS TRUE VIRTUAL VOID
%token WHILE ASSIGN OR AND EQ NEQ LT LE GT GE DLT PLUS DPLUS DMINUS MINUS STAR DIV
%token MOD NEG INCR DECR LPAREN RPAREN LBRACE RBRACE DOT CALL POINTER AMP COMMA 
%token COLON SEMICOLON EOF

/* Associativity, priority */
%left OR
%left AND
%nonassoc NOT
%left MINUS PLUS
%left TIMES DIV
%nonassoc THEN
%nonassoc ELSE

/* Entry point for grammary */
%start file

/* Type of the lexer's returned values */
%type <Ast.declaration list> file
%%

file:
  IOSTREAM?; d = decl*;  EOF { d }
; 

decl:
| e = decl_vars {}
| e = decl_class {}
| e = proto block {}
;

decl_vars:
  t = typ; vlist = separated_list(COMMA, var) { }

;
 
decl_class: 
  CLASS; i = TIDENT; supers?; LBRACE; PUBLIC; COLON; m = member*; RBRACE; SEMICOLON;
   {}
;

supers:
  COLON; slist = separated_nonempty_list(COMMA, pubtident) {}
;

pubtident:
 PUBLIC t = TIDENT {}
;


member:
| e = decl_vars {}
| VIRTUAL? p = proto SEMICOLON {}
;

proto: 
| typ qvar proto_end {}
| TIDENT proto_end {}
| TIDENT COLON COLON TIDENT proto_end {}
;

%inline proto_end:
 LPAREN a = separated_list(COMMA, argument) RPAREN {}
;

typ:
| VOID {}
| INT {}
| TIDENT {}
;

argument:
  t = typ; v = var {}
;

var:
| i = TIDENT {}
| STAR v = var {}
| AMP v = var {}
;

qvar:
| q = qident {}
| STAR q = qvar {}
| AMP q = qvar {}
;

qident:
| i = IDENT {}
| t = TIDENT COLON COLON i = IDENT {}
;

expr:
| i = INT {}
| THIS {}
| FALSE {}
| TRUE {}
| NULL {}
| q = qident {}
| STAR e = expr {}
| e = expr DOT i = IDENT {}
| e = expr POINTER i = IDENT {}
| e1 = expr ASSIGN e2 = expr {}
| e = expr LPAREN elist = separated_list(COMMA, expr) RPAREN {}
| NEW t = TIDENT LPAREN elist = separated_list(COMMA, expr) RPAREN {}
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
| t = typ; v = var {}
| t = typ; v = var; EQ; e = expr SEMICOLON {}
| ty = typ; v = var; EQ ti = TIDENT;
   LPAREN elist = separated_list(COMMA, expr) RPAREN SEMICOLON {}
| IF LPAREN e = expr RPAREN i = instruction {}
| IF LPAREN e = expr RPAREN i1 = instruction ELSE i2 = instruction {}
| WHILE LPAREN e = expr RPAREN i = instruction {}
| FOR LPAREN elist1 = separated_list(COMMA, expr) SEMICOLON e2 = expr? 
   SEMICOLON elist3 = separated_list(COMMA, expr) RPAREN i = instruction {}
| b = block {}
| COUT elist = separated_nonempty_list(DLT, expr_str) SEMICOLON {}
| RETURN e = expr? SEMICOLON {}
;

expr_str:
| e = expr {}
| s = STRING {}
;

block:
  LBRACE ilist = instruction* RBRACE {}
;
