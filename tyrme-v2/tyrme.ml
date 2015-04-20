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
(*********************************)

(** Types tag et mot **)
type tag = Float of int | String of int | Fun of int | Tab of int;;

type mot = 
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))



type instr = 
  | Halt 
  | Push 
  | Print
  | Acc of int 
  | Consti of int 
  | Pop of int 
  | BranchIf of int
  | Branch of int
  | Makeblock of tag * int
  | Str of string 
  | Bin_op of int;;



let string_of_tag = function
  | Float n -> "=(Tag) Float= "^(string_of_int n)^" ; "
  | String n -> "=(Tag) String= "^(string_of_int n)^" ; "
  | Fun n -> "=[Tag] Fonction= "^(string_of_int n)^" ; "
  | Tab n -> "=[Tag] Tableau/n-uplet= "^(string_of_int n)^" ; "


(* Ne traite que les opcode des opérations binaires *)
let string_of_opcode_binop = function
  | 0 -> "Halt"
  | 1 -> "Push"
  | 2 -> "Print"
  | 3 -> "Apply"
  | 4 -> "Acc"
  | 5 -> "Consti"
  | 6 -> "Return"
  | 7 -> "Pop"
  | 8 -> "BranchIf"
  | 9 -> "Branch"
  | 10 -> "Getblock"
  | 11 -> "Makeblock"
  | 12 -> "Closure"
  | 13 -> "Binop"
  | 14 -> "Str"
  | 15 -> "Add"
  | 16 -> "Sub"
  | 17 -> "Mul"
  | 18 -> "Div"
  | 19 -> "Eqi"
  | 20 -> "Cat"
  | _ -> failwith "TODO"



(* Affiche les chaines associées au instructions *)
let string_of_instr : instr -> string = function
  | Halt -> "Halt ;"
  | Push -> "Push ;"
  | Print -> "Print ;"
  | Acc n -> "Acc "^(string_of_int n)^" ;"
  | Bin_op v -> "Binop "^(string_of_opcode_binop v)^" ;"
  | Consti n -> "Const "^(string_of_int n)^" ;"
  | Pop n -> "Pop "^(string_of_int n)^" ;"
  | BranchIf n -> "BranchIf "^(string_of_int n)^" ;"
  | Branch n -> "Branch "^(string_of_int n)^" ;"
  | Makeblock(_,_) -> "Makeblock TAG * MOT LIST ;"
  | Str ss -> "Str "^ss^" ;"



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


let rec string_of_mot : mot -> string = function
  | MotInt n -> string_of_int n^" ; "
  | PointString s -> s^" ;"
  | PointBloc(t,bloc) -> "["^(string_of_tag t)^(list_to_string bloc)^"]"
and list_to_string (li : mot list) : string =
  let rec list_to_str_rec (l : mot list) (res : string) : string =
    match l with
      | [] -> res
      | t::q -> list_to_str_rec q (res^(string_of_mot t))
  in list_to_str_rec li "" ;;

string_of_mot (PointBloc(Tab 0,[MotInt 5;PointString "toto"; PointBloc(Tab 0, [MotInt 0; MotInt 1])]));;

string_of_mot (PointBloc(Tab 0,[MotInt 1]));;

let int_equal (x : int) (y : int ) : int =
  if(x = y) then 1 else 0;;



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




let make_block (n : int) (pile : mot array) (taille : int) : mot list =
   assert( n >= 0 && n <= taille);
  let rec make_block_rec (m : int) (p : mot array) (t : int) 
      (ac : int) (li : mot list ) : mot list =
    begin
    if(ac = taille)
    then
      li
    else
      make_block_rec m p t (ac+1) (p.(taille - ac - 1)::li)
    end
  in make_block_rec n pile taille 0 [];;


(** Tester make_block **)
(*make_block 4 [|MotInt 2; MotInt 18; MotInt 3; MotInt 21; MotInt 1024|] 4;;*)


(* Afficher l'état de la pile *)
let print_state (s : mv_state) : unit =
  print_string("PC : ");
  print_int(s.pc);print_endline("");
  print_endline( string_of_instr(s.code.(s.pc)) )
 


(* La fonction d'execution de la machine *)
let machine (s : mv_state) : mv_state =
  while s.pc < Array.length s.code do
    print_state s;
    begin
      match s.code.(s.pc) with
	| Halt -> 
	  raise Exit
	| Print ->
	  begin
	    match s.acc with
	      | PointString(a) -> print_string(a)
	      | _ -> failwith "Print no supporté" 
	  end
	| Acc n -> 
	  assert(n > 0 && n <= s.sp);
	  s.acc <- s.stack.(s.sp - n)
	| Consti n ->
	  s.acc <- MotInt(n)
	| Push ->
	  s.sp <- s.sp + 1;
	  s.stack.(s.sp) <- s.acc
	| Pop n ->
	  assert( n <= (s.sp + 1) );
	  s.sp <- s.sp -n
	| BranchIf n -> 
	  assert( ((s.pc + (n-1) ) > 0) && ((s.pc + (n-1) ) < Array.length s.code) );
	  begin
	    match s.acc with
	      | MotInt(i) -> s.pc <- s.pc + (if (i = 0) then n - 1  else 0 )
	      | _ -> failwith "Branchement non valide\n"
	  end
	| Branch n ->
	  assert( (s.pc + n) > 0 && (s.pc + n) < Array.length s.code );
	  s.pc <- s.pc + n -1 
	| Makeblock(t,n) ->
	  s.acc <- (PointBloc(t,(make_block n s.stack (s.sp + 1) )));
	  s.sp <- s.sp - n
	| Str st -> 
	  s.acc <- PointString(st)
	| Bin_op n ->
	  begin 
	    (
	      match n with
		| 15 -> 
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> s.acc <- MotInt((a+b))
		      | _ -> failwith "Addition non valide"
		  end  
		| 16 ->
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> s.acc <- MotInt((a-b))
		      | _ -> failwith "Soustraction non valide"  
		  end
		| 17 ->
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> s.acc <- MotInt((a*b))
		      | _ -> failwith "Multiplication non valide"  
		  end
		| 18 ->
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> assert(b <> 0);s.acc <- MotInt((a/b))
		      | _ -> failwith "Division non valide"  
		  end
		| 19 ->
		  begin
		    match s.stack.(s.sp), s.acc with
		      | MotInt(a),MotInt(b) -> s.acc <- MotInt(int_equal a b)
		      | _ -> failwith "Egalité non valide"  
		  end
		| 20 ->
		  begin
		    match s.stack.(s.sp), s.acc with
		      | PointString(a),PointString(b) -> s.acc <- PointString(a^b)
		      | _ -> failwith "Concatenation non valide"  
		  end
		| _ -> 
		  failwith "Ce binop n'est pas supporte"
	    );
   	      s.sp <- s.sp -1(* On pop après chaque opération *)
	  end
    end;
    print_string("ACC : ");print_string(string_of_mot(s.acc) );print_endline("");
    print_string("STACK : ");print_stack s.stack s.sp; print_endline("\n");
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
    [Bin_op (opcode o); Pop 1]
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
let ex_instru1 = [|Consti 3; Push; Consti 2; Bin_op 15|];;
let ex_instru2 = [|Consti 3; Push; Consti 2; Bin_op 16|];;
let ex_instru3 = [|Consti 3; Push; Consti 2; Bin_op 17|];;
let ex_instru4 = [|Consti 24; Push; Consti 6; Bin_op 18|];;
let ex_instru5 = [|Consti 0; Push; Consti 0; Bin_op 19|];;
let ex_instru6 = [|Str "B\n"; Push; Str "A"; Bin_op 20|];;
let ex_instru7 = [|Str "Alpha\n"; Print|];;
let ex_instru8 = [|Str "HALT\n"; Halt; Consti 2|];;
let ex_instru9 = [|Str "STRING ATTENDUE\n"; Push; Consti 2; Push ; Consti 1; Push; Acc 2;Pop 3|];;
let ex_instru10 = [|Str "str"; Push; Consti 2; Push ; Consti 1; Push; Acc 2;Pop 2; Str "cat\n"; Bin_op 20|];;

let ex_instru11 = [|Consti 1; Push; Consti 0; BranchIf 3; Consti 5; Branch 2;Consti 24; Pop 1|];;


print_string("\nResultat I add ->  "^string_of_mot(eval ex_instru1)^"\n\n");;
print_endline("");;

print_string("\nResultat II sub ->  "^string_of_mot(eval ex_instru2)^"\n\n");;
print_endline("");;

print_string("\nResultat III mul ->  "^string_of_mot(eval ex_instru3)^"\n\n");;
print_endline("");;

print_string("\nResultat IV div ->  "^string_of_mot(eval ex_instru4)^"\n\n");;
print_endline("");;

print_string("\nResultat V eqi ->  "^string_of_mot(eval ex_instru5)^"\n\n");;
print_endline("");;

print_string("\nResultat VI cat ->  "^string_of_mot(eval ex_instru6)^"\n\n");;
print_endline("");;

print_string("\nResultat VII Str ->  "^string_of_mot(eval ex_instru7)^"\n\n");;

try 
  begin
    print_string("\nResultat VIII Halt ->  "^string_of_mot(eval ex_instru8)^"\n\n");
    print_endline("");
  end
with Exit -> print_string("OK halt\n");;


print_string("\nResultat IX Add + Pop ->  "^string_of_mot(eval ex_instru9)^"\n\n");;
print_endline("");;

print_string("\nResultat X Add + Pop + cat ->  "^string_of_mot(eval ex_instru10)^"\n\n");;
print_endline("");;


print_string("\nResultat XI BranchIf + Branch  ->  "^string_of_mot(eval ex_instru11)^"\n\n");;
print_endline("");;
