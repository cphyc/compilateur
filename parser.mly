%{
%}

%token <string> IDENT
%token <int> CST
%token CLASS ELSE FALSE FOR IF INT NEW NULL PUBLIC RETURN THIS TRUE VIRTUAL VOID
%token WHILE ASSIGN OR AND EQ NEQ LT LE GT GE PLUS MINUS TIMES DIV MOD NEG INCR
%token DECR LPAREN RPAREN DOT CALL POINTER COMMA COLON SEMICOLON EOF
%start <Ast.fichier> fichier
%%

fichier:
e = EOF {{Ast.iostr = false; Ast.decl=[]}}
; 
