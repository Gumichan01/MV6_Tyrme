(** Expressions of Tyrme. *)

type var = string

type value =
  | ValVar of var
  | Int of int
  | Bool of bool
  | String of string
  | Unit

type binop = Add | Sub | Mult | Div | Leq | Eq | And | Cat | App
                            
type expr =
  | Var of var
  | Const of value
  | Binop of binop * expr * expr
  | If of expr * expr * expr
  | Let of var * expr * expr
  | Letf of var * var * expr * expr
  | Print of expr * expr
  | Pair of expr * expr
  | Fst of expr 
  | Snd of expr



