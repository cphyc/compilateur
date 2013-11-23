(* Analyseur lexical pour miniC++ *)

{
  open Lexing
  open Parser
   
  exception Lexing_error of string

  (* tables des mots-clés *)
  let kwd_tbl = 
    [
      "class", CLASS; "else", ELSE; "false", FALSE;
      "for", FOR; "if", IF; "int", INT; "new", NEW;
      "NULL", NULL; "public", PUBLIC; "return", RETURN;
      "this", THIS; "true", TRUE; "virtual", VIRTUAL;
      "void", VOID; "while", WHILE
    ]
  
  (* TODO: vérifier qu'on n'a pas un nom de classe dynamique *)
  (* détermine si on a un identifieur ou un mot clé *)
  let id_or_kwd = 
    let h = Hashtbl.create 17 in
    (* Remplissage de la table de hashage *)
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s -> (* on cherche s, sinon en renvoie juste l'identificateur *)
      try List.assoc s kwd_tbl with _ -> IDENT s

  (* va à la ligne suivante en incrémentant la référence de ligne *)
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let digit = ['0'-'9']
let alpha = ['a'-'Z' 'a'-'Z']
let ident = (alpha | '_') (alpha | '_' | digit)*
let tident = (alpha | '_') (alpha | '_' | digit)*
let octal_digit = ['0'-'7']
let hexa_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = '0' | ['1'-'9'] digit* | '0' octal_digit+ | "0x" hexa_digit+
(* Il faut enlever le '\' et le '"' *)
let carac = ['\032'-'\033' '\035'-'\038' '\040'-'\091'
  '\093'-'\127'] | "\\\\" | "\\\"" | "\\\'" 
  |"\\x" hexa_digit hexa_digit
  |"\\n" |"\\t"
let string = '\"' carac* '\"'
let space = [' ' '\t']

rule token = parse
  | "#include <iostream>" { IOSTREAM }
  | "std::cout" { COUT }
  | "\n"    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '='     { EQ }
  | "||"    { OR }
  | "&&"    { AND }
  | "=="    { DEQ }
  | "!="    { NEQ }
  | '<'     { LT }
  | "<="    { LE }
  | '>'     { GT }
  | ">="    { GE }
  | "<<"    { DLT }
(* Il faut intégrer le support des opérateurs unaires '+', '-' et '*' *)
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { STAR }
  | '/'     { DIV }
  | '%'     { MOD }
  | '!'     { EXCL }
  | "++"    { DPLUS }
  | "--"    { DMINUS }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '.'     { DOT }
  | "->"    { POINTER }
  | '&'     { AMP }
  | ','     { COMMA }
  | ':'     { COLON }
  | "::"    { DCOLON }
  | ";"     { SEMICOLON }
  | "/*"    { comment lexbuf }
  | "//"    { newline lexbuf; token lexbuf }
  | integer as s { CST (int_of_string s) }
  | eof     { raise (Lexing_error "reached end of file") }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*/"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
