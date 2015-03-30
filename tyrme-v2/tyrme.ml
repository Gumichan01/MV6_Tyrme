(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast




(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : Ast.expr = Parser.main Lexer.token (Lexing.from_string s)
(* FIX *)

(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)

type instr = Push | Consti of int | Pop | Bin_op of int;;


Binop(Add,Const(Int(2)),Const(Int(1)));;

(* Ne traite que les opcode des opérations binaires *)
let string_of_opcode_binop = function
  | 13 -> "Add"
  | _ -> failwith "TODO"



(* Affiche les chaines associées au instructions *)
let string_of_instr : instr -> string = function
    | Push -> "Push ;"
    | Bin_op v -> "Binop "^(string_of_opcode_binop v)^" ;"
    | Consti n -> "Const "^(string_of_int n)^" ;"
    | Pop -> "Pop ;"




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


let rec string_of_mot : mot -> string = function
  | MotInt n -> string_of_int n
  | PointString s -> s
  | _ -> failwith "mot non traite"



type mv_state = {
  mutable acc: mot;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: mot array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot = s.acc
(*let get_acc (s : mv_state) : int = s.acc*)



(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr array) : mv_state =
  { 
    code = c; 
    stack = Array.make 1000 (MotInt(0));
    pc = 0;
    sp = -1;   
    acc = MotInt(52) }



(* Affiche l'état de la pile *)
let print_stack st sp =
  let rec print_stack_rec stack pointer ac =
    if( ac > pointer)
    then
      ()
    else
      begin
	print_string(string_of_mot(stack.(ac)));
	print_stack_rec stack pointer (ac+1)
      end
  in print_stack_rec st sp 0;;



(*  *)
let print_state (s : mv_state) : unit =
  print_string("PC : ");
  print_int(s.pc);print_endline("");
  print_string("STACK : ");print_stack s.stack s.sp; print_endline("");
  print_endline( string_of_instr(s.code.(s.pc)) )
               


(* La fonction d'execution de la machine *)
let machine (s : mv_state) : mv_state =
  while s.pc < Array.length s.code do
    print_state s;
    begin
      match s.code.(s.pc) with
	| Consti n ->
	  s.acc <- MotInt(n)
	| Push ->
	  s.sp <- s.sp + 1;
	  s.stack.(s.sp) <- s.acc
	| Pop ->
	  s.sp <- s.sp -1
	| Bin_op n ->
	  begin 
	    (
	      match n with
		| 13 -> 
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> s.acc <- MotInt((a+b))
		      | _ -> failwith "addition non valide"
		  end
		| _ -> 
		  failwith "Ce binop n'est pas supporte"
	    );
   	      s.sp <- s.sp -1(* On pop après chaque opération *)
	  end
    end;
    s.pc <- s.pc + 1;
  done; s



(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr array) : mot =  
  let s = machine (init c) in get_acc s






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
  | _ -> failwith "repr non supporte"

(* Incrémente la position d'une variable dans la pile *)
let succ sv = match sv with
  | s, i -> (s,(i+1))


(* Applique la fonction succ sur tous les éléments de la liste *)
let envsucc e = List.map succ e



(* Convertit un binop  en opcode *)
let opcode i = match i with
  | Add -> 13
  | _ -> failwith "pas traite"



(* La fonction de compilation *)
let rec compil : env * expr -> instr list = function
  | _,Const v -> [Consti (repr v)]
  | env,Binop (o,e1,e2) -> compil (env,e1) @
    [Push] @
    compil ((envsucc env),e2) @
    [Bin_op (opcode o); Pop]
  | _,_ -> failwith "pas encore exp"

(* Pour lire le codex *)
let lire_codex () = 
  print_string (string_of_mot (eval (Array.of_list(disassemble_filename "codex.tm"))))
               
               
(* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
let ex_compil () =
  print_string (string_of_mot 
		  (eval 
		     (Array.of_list(compil(empty_env, parse "let x = 1 in x + 2")))
		  )
  )

(** TEST *)

(* addition *)
let ex_instru = [|Consti 1; Push; Consti 2; Bin_op 13; Pop|]

let x = eval ex_instru