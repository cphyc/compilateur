(* Fichier Principal du compilateur de C++ *)
open Format
open Compile

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant minic++ --help *)
let options = 
  ["--parse-only", Arg.Set parse_only, 
   "  Pour faire uniquement la phase d'analyse syntaxique";
  "--type-only", Arg.Set type_only, 
   "  Pour faire la phase d'analyse syntaxique et lexicale"]

let usage = "usage: minic++ [option] file.c"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation (p1,p2) =
  let l1 = p1.Lexing.pos_lnum and l2 = p2.Lexing.pos_lnum in
  let c1 = p1.Lexing.pos_cnum - p1.Lexing.pos_bol + 1 in
  let c2 = p2.Lexing.pos_cnum - p2.Lexing.pos_bol + 1 in
  if l1 = l2
  then eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l1 c1 c2
  else eprintf 
    "File \"%s\", from line %d, character %d to line %d, character %d:\n" 
    !ifile l1 c1 l2 c2

let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .cpp *)
  if not (Filename.check_suffix !ifile ".cpp") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .cpp\n@?";
    Arg.usage options usage;
    exit 1
  end;
  
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
  
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  
  (* Tentative d'analyse lexicale et grammaticale *)
  try
    let tree = Parser.file Lexer.token buf in
    close_in f;
    if (not !parse_only) then 
	let tree = Typer.file tree in ();
	if (not !type_only) then
	  Compile.compile tree ((Filename.chop_suffix !ifile ".cpp")^".s");
  with 
  | Lexer.Lexing_error c ->
      (* Erreur lexicale. On récupère sa position absolue et 
         on la convertit en numéro de ligne *)
    localisation ((Lexing.lexeme_start_p buf), (Lexing.lexeme_end_p buf));
    eprintf "Erreur dans l'analyse lexicale: %s.@." c;
    exit 1
  | Parser.Error -> 
      (* Erreur syntaxique. On récupère sa position absolue et on la 
	 convertit en numéro de ligne *)
    localisation ((Lexing.lexeme_start_p buf), (Lexing.lexeme_end_p buf));
    eprintf "Erreur dans l'analyse syntaxique@.";
    exit 1
  | Typer.Error (c,loc) ->
    localisation loc;
    eprintf "Erreur de type: %s@." c;
    exit 1
