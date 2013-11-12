(* Fichier Principal du compilateur arithc *)

open Format

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options = 
  ["-parse-only", Arg.Set parse_only, 
   "  Pour faire uniquement la phase d'analyse syntaxique"]

let usage = "usage: minic++ [option] file.c"

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
 
  (*TODO: Il faut faire quelque chose du fichier, maintenant qu'il est ouvert,
    mais la flemme, plus tard.*)
  ();
