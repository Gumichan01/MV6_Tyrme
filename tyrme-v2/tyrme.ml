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

(** Types tag et mot **)
type tag = int;;

type mot = 
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))


(** Les instructions machines **)
type instr = 
  | Halt 
  | Push 
  | Print
  | Apply
  | Acc of int 
  | Consti of int 
  | Return of int
  | Pop of int 
  | BranchIf of int
  | Branch of int
  | GetBlock of int
  | Makeblock of tag * int
  | Closure of int * int
  | Str of string 
  | Bin_op of int;;



(** Renvoie l'instruction associée à l'opcode **)
let string_of_opcode = function
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



(** Affiche les chaines associées au instructions **)
let string_of_instr : instr -> string = function
  | Halt -> "Halt ;"
  | Push -> "Push ;"
  | Print -> "Print ;"
  | Apply -> "Apply ;"
  | Acc n -> "Acc "^(string_of_int n)^" ;"
  | Bin_op v -> "Binop "^(string_of_opcode v)^" ;"
  | Consti n -> "Const "^(string_of_int n)^" ;"
  | Return n -> "Return "^(string_of_int n)^" ;"
  | Pop n -> "Pop "^(string_of_int n)^" ;"
  | BranchIf n -> "BranchIf "^(string_of_int n)^" ;"
  | Branch n -> "Branch "^(string_of_int n)^" ;"
  | GetBlock n -> "GetBlock "^(string_of_int n)^" ;"
  | Makeblock(_,_) -> "Makeblock TAG * MOT LIST ;"
  | Closure(_,_) -> "Closure INT * INT;"
  | Str ss -> "Str "^ss^" ;"



(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(** Fonctions de lecture et d'ecriture d'entier 8bits et 32bits **)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf

(** Conversion string/char[] **)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(** Conversion char[]/string **)
let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;


(** Ecrit une string dans un fichier **)
let rec out_str (buf : out_channel) (s : string) : unit =
  let ch = explode s in
  let rec out_lst b l =
    match l with
      | [] -> ()
      | h::q -> output_char b h; out_lst b q
  in out_lst buf ch;;


(** Lis une string dans un fichier **)
let rec in_str (buf : in_channel) : string =
  let len = in_i32 buf in
  begin
    let rec in_chr l acc =
      if(acc == len)
      then
	implode l
      else 
	in_chr (l@[input_char buf]) (acc+1)
    in in_chr [] 0
  end;;


(** Fonction d'assemblage d'instruction **)
let assemble_instr (buf : out_channel) : instr -> unit = function
  | Halt -> out_i8 buf 0
  | Push -> out_i8 buf 1
  | Print -> out_i8 buf 2
  | Apply -> out_i8 buf 3
  | Acc n -> out_i8 buf 4; out_i32 buf n
  | Consti n -> out_i8 buf 5; out_i32 buf n
  | Return n -> out_i8 buf 6; out_i32 buf n
  | Pop n -> out_i8 buf 7; out_i32 buf n
  | BranchIf n -> out_i8 buf 8; out_i32 buf n
  | Branch n -> out_i8 buf 9; out_i32 buf n
  | GetBlock n -> out_i8 buf 10; out_i32 buf n
  | Makeblock(t,n) -> out_i8 buf 11; out_i8 buf t; out_i32 buf n
  | Closure(n,o) -> out_i8 buf 12; out_i32 buf n; out_i32 buf o
  | Bin_op b -> out_i8 buf 13; out_i8 buf b
  | Str s -> out_i8 buf 14; out_i32 buf (String.length s); out_str buf s
;;


(** Fonction d'assemblage d'une liste d'instructions **)
let rec assemble (buf : out_channel) : instr list -> unit = function
  | [] -> ()
  | h::q -> assemble_instr buf h; assemble buf q
;;



(** Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier **)
let assemble_filename (name : string) (is : instr list) : unit = 
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf
  end;;



(** Fonction de desassemblage **)
let rec disassemble (buf : in_channel) : instr list =
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  match inc with
  | None   -> []
  | Some c ->
    match c with
      | 0 -> (disassemble buf)@[Halt]
      | 1 -> (disassemble buf)@[Push]
      | 2 -> (disassemble buf)@[Print]
      | 3 -> (disassemble buf)@[Apply]
      | 4 -> (disassemble buf)@[Acc (in_i32 buf)]
      | 5 -> (disassemble buf)@[Consti (in_i32 buf)]
      | 6 -> (disassemble buf)@[Return (in_i32 buf)]
      | 7 -> (disassemble buf)@[Pop (in_i32 buf)]
      | 8 -> (disassemble buf)@[BranchIf (in_i32 buf)]
      | 9 -> (disassemble buf)@[Branch (in_i32 buf)]
      | 10 -> (disassemble buf)@[GetBlock (in_i32 buf)]
      | 11 ->
	begin
	  let t = in_i8 buf in 
	  let n = in_i32 buf in
	  (disassemble buf)@[Makeblock(t,n)]
	end
      | 12 ->
	begin
	  let n = in_i32 buf in 
	  let o = in_i32 buf in
	  (disassemble buf)@[Closure(n,o)]
	end
      | 13 -> (disassemble buf)@[Bin_op (in_i8 buf)]
      | 14 -> (disassemble buf)@[Str (in_str buf)]
      | _ -> failwith "Code-octet non reconnu"
;;


(* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list = 
  let buf = open_in_bin name in
  let insts = List.rev(disassemble buf) in
  let _ = close_in buf in
  insts;;



(**************************************************************)
(* Machine virtuelle                                          *)
(**************************************************************)

(** Renvoie la chaine associé à un mot, Si on a un tag,
alors on a une recursion croisée avec list_to_string **)
let rec string_of_mot : mot -> string = function
  | MotInt n -> string_of_int n^" "
  | PointString s -> s^" "
  | PointBloc(t,bloc) -> "[ TAG "^(string_of_int t)^" : "^(list_to_string bloc)^"] "
and list_to_string (li : mot list) : string =
  let rec list_to_str_rec (l : mot list) (res : string) : string =
    match l with
      | [] -> res
      | t::q -> list_to_str_rec q (res^(string_of_mot t))
  in list_to_str_rec li "" ;;



(** Egalité (pas utile) **)
let int_equal (x : int) (y : int ) : int =
  if(x = y) then 1 else 0;;



type mv_state = {
  mutable acc: mot;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: mot array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}


(** Retourne l'accumulateur de l'etat donne en argument **)
let get_acc (s : mv_state) : mot = s.acc



(** Pour construire l'etat de la machine au debut de l'execution **)
let init (c : instr array) : mv_state =
  { 
    code = c; 
    stack = Array.make 1000 (MotInt(0));
    pc = 0;
    sp = -1;   
    acc = MotInt(52) }



(** Affiche l'état de la pile **)
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



(** Créer un bloc relatif **)
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



(** Afficher l'état de la pile **)
let print_state (s : mv_state) : unit =
  print_string("PC : ");
  print_int(s.pc);print_endline("");
  print_endline( string_of_instr(s.code.(s.pc)) );
  print_string("ACC : ");print_string(string_of_mot(s.acc) );print_endline("");
  print_string("STACK : ");print_stack s.stack s.sp; print_endline("\n");;
 

(** On ecrit le bloc relatif à la fonction appelée dans la pile lors **)
let writeStack (li : mot list) (stack : mot array) (sp : int) : int =
  let ar = Array.of_list li in
  begin
    let it = ref ((Array.length ar) - 1) and st = ref sp in
    begin
      while( !it > 0 )
      do
	stack.(!st) <- ar.(!it);
	st := !st + 1;
	it := !it - 1
      done; !st
    end
  end;;
  

(** La fonction d'execution de la machine **)
let machine (s : mv_state) : mv_state =
  let stop = ref false in
  while s.pc < Array.length s.code && !stop == false do
    print_string("==== State BEFORE ====\n");
    print_state s;
    begin
      match s.code.(s.pc) with
	| Halt ->
	  s.sp <- -1;
	  stop := true
	| Print ->
	  begin
	    match s.acc with
	      | PointString(a) -> print_string(a)
	      | _ -> failwith "Print non supporté"
	  end
	| Apply ->
	  begin
	    let sav = s.stack.(s.sp) in
	    begin
	      s.stack.(s.sp) <- MotInt(s.pc);
	      begin
		match s.acc with
		  | PointBloc(a,b) ->
		    begin
		      if(a == 88)
		      then  
			begin
			  s.sp <- s.sp + 1;
			  s.sp <- writeStack b s.stack s.sp;
			  s.stack.(s.sp) <- sav;
			  s.pc <- match List.hd b with
			    | MotInt i -> i
			    | _ -> failwith "erreur apply"
			end
		      else
			failwith ("Mauvais TAG")
		    end
		  | _ -> failwith "Mauvais appel"
	      end
	    end
	  end
	| Acc n -> 
	  begin
	    assert(n >= 0 && n <= s.sp);
	    s.acc <- s.stack.(s.sp - n)
	  end
	| Consti n ->
	  s.acc <- MotInt(n)
	| Return n ->
	  assert( n >= 0);
	  if(n < s.sp +1 )
	  then
	    begin
	      s.sp <- s.sp - n;
	      if( s.sp >= 0 )
	      then
		begin
		  match s.stack.(s.sp) with
		    | MotInt(v) -> s.pc <- v; s.sp <- s.sp - 1
		    | _ -> failwith "PC non valide"
		end
	      else ()
	    end
	  else ()
	| Push ->
	  s.sp <- s.sp + 1;
	  s.stack.(s.sp) <- s.acc
	| Pop n ->
	  assert( n <= (s.sp + 1) );
	  s.sp <- s.sp - n
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
	| GetBlock n -> assert( n >= 0);
	  begin
	    match s.acc with
	      | PointBloc(_,l) -> 
		begin 
		  assert(n < List.length l);
		  s.acc <- ((Array.of_list l).(n))
		end
	      | _ -> failwith("GetBlock s'applique sur un type non reconnu")
	  end
	| Makeblock(t,n) ->
	  s.acc <- (PointBloc(t,(make_block n s.stack (s.sp + 1) )));
	  s.sp <- s.sp - n
	| Closure(n,o) ->
	  s.acc <- (PointBloc(88, [MotInt(s.pc+o)]@(make_block (n) s.stack (s.sp + 1) )));
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
    print_string("==== State AFTER ====\n");
    print_state s;
    print_endline("");
    s.pc <- s.pc + 1;
  done; s;;



(** La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation **)
let eval (c : instr array) : mot =  
  let s = machine (init c) in get_acc s





(**************************************************************)
(* Compilation                                                *)
(**************************************************************)

(** Le type environnement **)
type env = (Ast.var * int) list


(** L'environnement d'utilisation des variables **)
let empty_env = []


(** Représentation d'un type **)
let repr (v: Ast.value) = match v with
  | Int i      -> i
  | Bool true  -> 1
  | Bool false -> 0
  | Unit -> 0
  | _ -> failwith "repr non supporte"


(** Incrémente la position d'une variable dans la pile **)
let succ sv = match sv with
  | s, i -> (s,(i+1))


(** Applique la fonction succ sur tous les éléments de la liste **)
let envsucc e = List.map succ e



(** Convertit un binop en opcode **)
let opcode (i : Ast.binop) = match i with
  | Add -> 15
  | Sub -> 16
  | Mult -> 17
  | Div -> 18
  | Eq -> 19
  | Cat -> 20
  | _ -> failwith "pas traite";;


(** Selon le type de binop revoie l'instruction d'application 
    ou d'opérateur binaire **)
let creer_instr (i : Ast.binop) = match i with
  | App -> [Apply]
  | _ -> [Bin_op(opcode i)];;



(** La fonction de compilation **)
let rec compil : env * Ast.expr -> instr list = function
  | env,Var s -> [Acc (List.assoc s env)]
  | _,Const v -> 
    begin 
      match v with
	| String s -> [Str s]
	| _ -> [Consti (repr v)]
    end
  | env,Binop (App,e2,e1) -> compil (env,e1) @
    [Push] @
    compil ((envsucc env),e2) @(creer_instr(App))
  | env,Binop (o,e1,e2) -> compil (env,e1) @
    [Push] @
    compil ((envsucc env),e2) @(creer_instr(o))
  | env, If(cond,e1,e2) -> let i1 = compil(env,e1) in
			   let i2 = compil(env,e2) in
			   compil (env,cond) @
			     [ BranchIf(2 + List.length i2) ]@
			     i2 @
			     [Branch (1 + List.length i1)] @
			     i1
  | env, Let(s,e1,e2) -> let new_env = (s,0)::(envsucc env)
			 in compil (env,e1) @ [Push] @ compil (new_env,e2) @ [Pop 1]
  | env, Letf(f,p,e1,e2) -> let n_env = (p,0)::(envsucc env) 
			    in let nn_env = (f,0)::(envsucc n_env) 
			       in let c1 = compil(n_env, e1) 
			       and c2 = compil(nn_env,e2) in
				  [Closure(0,(List.length c2)+3)]@[Push]@c2@[Pop 1]
				  @[Halt]@c1@[Return 1]
  | env, Ast.Print(Ast.Var(s),e1) -> [Str s; Print]@(compil(env,e1)) 
  | env, Ast.Pair(fs,sd) -> (compil(env,fs))@[Push]@(compil(env,sd))@[Push]@[Makeblock(0,2)]
  | env, Ast.Fst(p) -> 
    begin 
      match p with
	| Ast.Pair(_,_) -> (compil(env,p))@[GetBlock 0]
	| Ast.Const(Int(n)) -> assert(n == 0 || n == 1 );
	  [GetBlock n]
	| _ -> failwith("Pas support pour ce Fst")
    end
  | env, Ast.Snd(p) -> 
    begin 
      match p with
	| Ast.Pair(_,_) -> (compil(env,p))@[GetBlock 1]
	| Ast.Const(Int(n)) -> assert(n == 0 || n == 1 );
	  [GetBlock n]
	| _ -> failwith("Pas support pour ce Snd")
    end
  | _,_ -> failwith "TODO";;




(** Pour lire le codex **)
let lire_codex () = 
  print_string (string_of_mot (eval (Array.of_list(disassemble_filename "codex.tm"))));;


           
(** Exemple de compilation qui doit marcher et rendre la valeur 3 **)
let ex_compil () =
  print_string (string_of_mot 
		  (eval 
		     (Array.of_list(compil(empty_env, parse "let f x = x * x  in f 2")))
		  )
  );;


(** ****** ** 
     TEST 
 ** ****** **)

(** Assemblage *)
(*assemble_filename "RESULT.txt" [Str "ZZZ";Print] ;;
assemble_filename "RESULT.txt" [Consti 1] ;;
assemble_filename "RESULT.txt" [Consti 1; Push; Pop 1] ;;
assemble_filename "RESULT.txt" [Consti 1; Push; Consti 4; Bin_op 15] ;;
assemble_filename "RESULT.txt" [Consti 8; Push; Consti 6; Bin_op 16] ;;
assemble_filename "RESULT.txt" [Consti 6; Push; Consti 4; Bin_op 17] ;;
assemble_filename "RESULT.txt" [Consti 1; Push; Consti 4; Bin_op 19] ;;
assemble_filename "RESULT.txt" [Str "B\n"; Push; Str "A"; Bin_op 20];;
assemble_filename "RESULT.txt" [Str "HALT\n"; Halt; Consti 2];;
assemble_filename "RESULT.txt" [Str "STRING ATTENDUE\n"; Push; 
				Consti 2; Push ; Consti 1; Push; Acc 2;Pop 3];;
assemble_filename "RESULT.txt" [Str "str"; Push; Consti 2; Push ; 
				Consti 1; Push; Acc 2;Pop 2; Str "cat\n"; Bin_op 20];;
assemble_filename "RESULT.txt" [Consti 1; Push; Consti 0; Bin_op 19; 
				BranchIf 3; Consti 24; Branch 2; Consti 5];;

assemble_filename "RESULT.txt" [Consti 24; Push; Consti 8; Push;
				Consti 1993; Push; Makeblock(0,3); GetBlock 2;];;

assemble_filename "RESULT.txt" [Branch 6; Consti 2; Push; Acc 1; Bin_op 15; Return 1; 
				Closure (0, -6); Push; Consti 1; Push; Acc 1; Apply; Pop 1];;*)

(*disassemble_filename "RESULT.txt";;*)


(*print_string (string_of_mot 
		(eval 
		   (Array.of_list(compil(empty_env, parse "let f x = x * x  in f 2")))
		)
);;*)


(** Compil *)

(** Print *)
(*compil(empty_env,Ast.Print(Ast.Var("Hello\n"),
			(Ast.Binop
			   (Ast.Add,Ast.Const(Int(1)),Ast.Const(Int(4)))
			)));;*)

(*compil(empty_env, Ast.Snd( Ast.Pair(Ast.Const(Int(2)),Ast.Const(Int(1))) ) );;*)


(** Fonction simple *)
(*compil(empty_env, Letf("f","p", 
		       Binop(Ast.Add,Ast.Const(Int(2)),Ast.Var("p")),
		       Binop(Ast.App,Ast.Var("f"),Const(Int(1)))));;

(** Opérateur *)
compil (empty_env,(Ast.Binop(Ast.Add,Ast.Const(Int(1)),Ast.Const(Int(4)))));;
compil (empty_env,(Ast.Binop(Ast.Sub,Ast.Const(Int(8)),Ast.Const(Int(6)))));;
compil (empty_env,(Ast.Binop(Ast.Mult,Ast.Const(Int(6)),Ast.Const(Int(4)))));;
compil (empty_env,(Ast.Binop(Ast.Eq,Ast.Const(Int(1)),Ast.Const(Int(4)))));;

compil (empty_env, (Ast.Binop(Ast.Cat,
			      Ast.Const(Ast.String("hello ")),
			      Ast.Const(String(" world!")))));;
(** If *)
compil (empty_env, Ast.If(Ast.Binop(Ast.Eq,Ast.Const(Int(1)),
				    Ast.Const(Int(0))),
				    Ast.Const(Int(5)),Ast.Const(Int(24))));;
(** Let *)
compil(empty_env,Let("x",
	   Const(Int(1)),
	   Binop(Ast.Add,Ast.Var("x"),Const(Int(3))) ) );;

compil(empty_env,Let("x",
		     Ast.If(Ast.Binop(Ast.Eq,Ast.Const(Int(1)),
				      Ast.Const(Int(0))),
			    Ast.Const(Int(5)),Ast.Const(Int(24))),
		     Binop(Ast.Add,Ast.Var("x"),Const(Int(3))) ) );;*)

(** addition *)
(*let ex_instru1 = [|Consti 3; Push; Consti 2; Bin_op 15|];;
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

let ex_instru11bis = [|Consti 1; Push; Consti 0; Bin_op 19; BranchIf 3; Consti 24; Branch 2;
 Consti 5|];;

let ex_instru12 = [|Consti 24; Push; Consti 8; Push; Consti 1993; Push; Makeblock(0,3); Push; Str "Paris 7"; Push; Makeblock(0,2)|];;

let ex_instru13 = [|Consti 1; Push; Consti 3; Push; Acc 1; Bin_op 15; Pop 1|];;

let ex_instru14 = [|Consti 1; Push; Consti 0; Bin_op 19; BranchIf 3; Consti 24; Branch 2;
 Consti 5; Push; Acc 0; Push; Consti 3; Bin_op 15; Pop 1|];;

let ex_instru15 = [|Consti 24; Push; Consti 8; Push; Consti 1993; Push; Makeblock(0,3); GetBlock 2|];;


let ex_instru16 = [|Branch 6;Acc 0; Push; Consti 2; Bin_op 15; Return 1; Closure(0,-6);
		    Push; Consti 2; Push; Acc 1; Apply; Pop 1; Push; Consti 9; Bin_op 17|];;


let ex_instru17 = [|Branch 17;
		    Branch 6;Acc 0; Push; Consti 2; Bin_op 15; Return 1; Closure(0,-6);
		    Push; Consti (-1); Push; Acc 1; Apply; Pop 1; Push; Consti 9; Bin_op 17; Return 1;
		    Closure(0,-17); Push; Consti (-1); Push; Acc 1; Apply; Pop 1|];;

let ex_instru18 = [|Branch 6; Consti 2; Push; Acc 1; Bin_op 15; Return 1; 
                    Closure (0, -6); Push; Consti 1; Push; Acc 1; Apply; Pop 1|];;

let ex_instru19 = [|Closure (0, 7); Push; Consti 1; Push; Acc 1; Apply; Pop 1;Halt;
		    Consti 2; Push; Acc 1; Bin_op 15; Return 1;|];;

let ex_instru19bis = Array.of_list(compil(empty_env, Letf("f","p", 
					    Binop(Ast.Add,Ast.Const(Int(2)),Ast.Var("p")),
					    Binop(Ast.App,Ast.Var("f"),Const(Int(1))))));;



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

print_string("\nResultat XI BIS Branch  ->  "^string_of_mot(eval ex_instru11bis)^"\n\n");;
print_endline("");;

print_string("\nResultat XII MakeBlock  ->  "^string_of_mot(eval ex_instru12)^"\n\n");;
print_endline("");;

print_string("\nResultat XIII Let  ->  "^string_of_mot(eval ex_instru13)^"\n\n");;
print_endline("");;

print_string("\nResultat XIV Let + If  ->  "^string_of_mot(eval ex_instru14)^"\n\n");;
print_endline("");;

print_string("\nResultat XV MakeBlock + GetBlock  ->  "^string_of_mot(eval ex_instru15)^"\n\n");;
print_endline("");;

print_string("\nResultat XVI Closure 1 ->  "^string_of_mot(eval ex_instru16)^"\n\n");;
print_endline("");;

print_string("\nResultat XVII Closure 2  ->  "^string_of_mot(eval ex_instru17)^"\n\n");;
print_endline("");;


print_string("\nResultat XVIII Closure 3  ->  "^string_of_mot(eval ex_instru18)^"\n\n");;
print_endline("");;

print_string("\nResultat XIX Closure 4  ->  "^string_of_mot(eval ex_instru19)^"\n\n");;
print_endline("");;

print_string("\nResultat XX Closure 5  ->  "^string_of_mot(eval ex_instru19bis)^"\n\n");;
print_endline("");;*)


(** Test complet *)

print_string("\n ======== Test parse/compil/assemble/disassemble/eval ======== \n\n");;
let inst = compil(empty_env, parse "let f x = x * x  in f 10");;

assemble_filename "test-tyrme-jeanpier.txt" inst;;
let x = string_of_mot
	       (eval
		  (Array.of_list(disassemble_filename "test-tyrme-jeanpier.txt"))
	       )
;;
print_endline("Resultat de l'évaluation : "^x);;
