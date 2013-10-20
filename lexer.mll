
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

  let id_or_kwd = 
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s -> 
      let s = String.lowercase s in (* la casse n'est pas significative *)
      try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter+ digit*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '='     { ASSIGN }
  | "||"    { OR }
  | "&&"    { AND }
  | "=="    { EQ}
  | "!="    { NEQ }
  | '<'     { LT }
  | "<="    { LE }
  | '>'     { GT }
  | ">="    { GE }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '!'     { NEG }
  | "++"    { INCR }
  | "--"    { DECR }
  | '*'     {}
  | '+'     {}
  | '-'     {}
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '.'     { DOT }
  | "()"    {}
  | "->"    {}
  | ','     { COMMA }
  | ':'     { COLON }
  | ";"     { SEMICOLON }
  | "/*"    { comment lexbuf }
  | integer as s { CST (int_of_string s) }
  | eof     { raise (Lexing_error "reached end of file") }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* note : les commentaires ne sont pas imbriqués en Pascal *)
and comment = parse
  | "*/"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
