(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast

(** Expressions of Tyrme. *)

type var = string

type value =
  | ValVar of var
  | Int of int
  | Bool of bool
  | String of string
  | Unit

type binop = Add | Eq | And | Cat | App


                            
type expr =
(*  | Var of var*)
  | Const of value
  | Binop of binop * expr * expr
 (* | If of expr * expr * expr
  | Let of var * expr * expr
  | Letf of var * var * expr * expr
  | Print of expr * expr
  | Pair of expr * expr
  | Fst of expr 
  | Snd of expr*)

(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)

type instr = Push | Const of int | Pop | Add

(* Affiche les chaines associées au instructions *)
let string_of_instr : instr -> string = function
    | Push -> "Push ;"
    | Add -> "Add ;"
    | Const n -> "Const "^(string_of_int n)^" ;"



(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf


(* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : out_channel) : instr -> unit = failwith "assemblage d'une instruction"


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


type mv_state = {
  mutable acc: int;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: int array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}


(* retourne l'accumulateur de l'etat donne en argument *)
(*let get_acc (s : mv_state) : mot = failwith "Not implemented yet"*)
let get_acc (s : mv_state) : int = s.acc

(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr array) : mv_state =
  { 
    code = c; 
    stack = Array.make 1000 0;
    pc = 0;
    sp = -1;   
    acc = 52 }


(* Peut-etre une fonction d'impression ? *)
let print_state (s : mv_state) : unit = failwith "pretty-printing de l'etat de la machine"
               


(* La fonction d'execution de la machine *)
(*let machine (s : mv_state) : mv_state = failwith "execution de la machine"*)
let machine s = 
  while s.pc < Array.length s.code do
    print_string("PC : ");
    print_int(s.pc);print_endline("");
    print_endline( string_of_instr(s.code.(s.pc)) );
    begin
      match s.code.(s.pc) with
	| Const n ->
	  s.acc <- n
	| Push ->
	  s.sp <- s.sp + 1;
	  s.stack.(s.sp) <- s.acc
	| Pop ->
	  s.sp <- s.sp -1
	| Add ->
	  s.acc <- s.stack.(s.sp) + s.acc;
    end;
    print_string("PC : "); print_int(s.pc);print_endline("");
    print_string("ACC : "); print_int(s.acc);print_endline("\n");
    s.pc <- s.pc + 1;
  done; s


let ex_instru = [|Const 1; Push; Const 2;Add; Pop|]

(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
(*let eval (c : instr array) : mot =*)
let eval (c : instr array) : int =  
  let s = machine (init c) in get_acc s



let x = eval ex_instru



(**************************************************************)
(* Compilation                                                *)
(**************************************************************)

(* Le type environnement *)
type env = (var * int) list


(* L'environnement d'utilisation des variables *)
let empty_env = []


(* Représentation d'un type *)
let repr = function
  | Int i      -> i
  | Bool true  -> 1
  | Bool false -> 0


(* Incrémente la position d'une variable dans la pile *)
let succ sv = match sv with
  | s, i -> (s,(i+1))


(* Applique la fonction succ sur tous les éléments de la liste *)
let envsucc e = List.map succ e


(* La fonction de compilation *)
let rec compil : env * expr -> instr list = function
  | _,Const v -> [Const (repr v)]
  | env,Binop (o,e1,e2) -> compil (env,e1) @
    [Push] @
    compil ((envsucc env),e2) @
    [op o; Pop]


(* Pour lire le codex *)
let lire_codex () = 
  print_string (string_of_mot (eval (disassemble_filename "codex.tm")))
               
               
(* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
let ex_compil () =
  print_string (string_of_mot (eval (compil (empty_env, parse "let x = 1 in x + 2"))))


