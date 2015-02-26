(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast


(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)

type instr = DEFINISSEZ_LES_INSTRUCTIONS


let string_of_instr : instr -> string = failwith "peut-etre une fonction d'impression ?"


(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf


(* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : in_channel) : instr -> unit = failwith "assemblage d'une instruction"


(* Fonction d'assemblage d'une liste d'instructions *)
let rec assemble (buf : out_channel) : instr list -> unit = failwith "assemblage d'une liste d'instruction"


(* Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier *)
let assemble_filename (name : string) (is : instr list) : unit = 
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf
  end





(* fonction de desassemblage: stub *)
let rec disassemble (buf : in_channel) : instr list =
  (* Get the next char, and make sure to capture the end of the file *)
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  (* Test if there were a char *)
  match inc with
  | None   -> []  (* Nope: end of the file *)
  | Some c ->     (* Yep ! Carry on *)
     (*
 
     ici, vous avez recupere un entier 8 bits (la valeur c) qui encode
     une instruction. Derriere, d'autre octets donnent potentiellement
     les arguments. A vous de les recuperer et d'en faire une
     instruction.
      
      *)
     failwith "a vous de continuer"



(* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list = 
  let buf = open_in_bin name in
  let insts = disassemble buf in
  let _ = close_in buf in
  insts




(**************************************************************)
(* Machine virtuelle                                          *)
(**************************************************************)

type tag = UNE_DEFINITION_DU_TAG_PAR_EXEMPLE_CHAR_OU_INT

type mot = 
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))


let rec string_of_mot : mot -> string = failwith "peut-etre un pretty-printer?"


type mv_state = L_ETAT_DE_LA_MACHINE_VIRTUELLE


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot = failwith "l'accumulateur de l'etat"


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state = failwith "etat de depart de la machine"


(* Peut-etre une fonction d'impression ? *)
let print_state (s : mv_state) : unit = failwith "pretty-printing de l'etat de la machine"
               


(* La fonction d'execution de la machine *)
let machine (s : mv_state) : mv_state = failwith "execution de la machine"


(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s




(**************************************************************)
(* Compilation                                                *)
(**************************************************************)

type env = UN_ENV_DE_COMPILATION

let empty_env = failwith "l'environement vide"


(* La fonction de compilation *)
let rec compil : env * expr -> instr list = failwith "compilation"





(* Pour lire le codex *)
let lire_codex () = 
  print_string (string_of_mot (eval (disassemble_filename "codex.tm")))
               
               
(* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
let ex_compil () =
  print_string (string_of_mot (eval (compil (empty_env, parse "let x = 1 in x + 2"))))


