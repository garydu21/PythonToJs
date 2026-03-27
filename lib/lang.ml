open Auxdefs

(* Definition of source language data structures *)

(* variable names *)
type vname = string
  [@@deriving show]

(* function names *)
type fname = string
  [@@deriving show]

(* binary arithmetic operators *)
type barith = BAadd | BAsub | BAmul | BAdiv | BAmod
  [@@deriving show]

(* Boolean operations, Python style *)
type bbool = BBand | BBor
  [@@deriving show]
  
  (* binary comparison operators: =, >=, >, <=, <, != *)
type bcompar = BCeq | BCge | BCgt | BCle | BClt | BCne
  [@@deriving show]

  (* binary operators, combining all of the above *)
type binop =
  BArith of barith
| BBool of bbool
| BCompar of bcompar
  [@@deriving show]
    
  (* TODO: recheck that all these values can really be read in *)
type value =
    BoolV of bool
  | IntV of int
  | FloatV of float
  | NoneV
  | StringV of string
  [@@deriving show]

  (* Types *)
type base_tp = BoolT | IntT | FloatT | NoneT | StringT
  [@@deriving show]
type tp = UnionT of base_tp list
  [@@deriving show]

  (* UnionT contains a list of sorted elements of base_tp (duplicates removed).
    This invariant has to be maintained for all instances of tp.
  *)
let mk_norm_tp = function 
| [] -> failwith "empty union type"
| ts -> UnionT (remove_duplicates_sorted (List.sort compare ts))

let numeric_tp = function
    IntT -> true
  | _ -> false

(* variable / parameter declaration *)
type vardecl = Vardecl of vname * tp
  [@@deriving show]

let tp_of_vardecl (Vardecl (_, t)) = t
let name_of_vardecl (Vardecl (vn, _)) = vn

(* expresssions *)
type expr = 
    Const of value                           (* constant *)
  | VarE of vname                            (* variable *)
  | BinOp of  binop * expr * expr            (* binary operation *)
  | CallE of vname * (expr list)             (* function call *)
  [@@deriving show]
      
type stmt =
    Block of stmt list
  | Assign of vname * expr                   (* Variable assignment: x = e *)
  | Cond of expr * stmt * stmt               (* if .. then .. else *)
  | While of expr * stmt                  
  | Return of  expr
  | CallS of vname * (expr list)             (* procedure call *)
  [@@deriving show]


(* function declaration: function name; parameter declarations; return type *)
type fundecl = Fundecl of fname * (vardecl list) * tp
  [@@deriving show]

(* function definition: function declaration; local var decls; function body *)
type fundefn = Fundefn of fundecl * (vardecl list) * stmt
  [@@deriving show]

(* program: function definitions; global var decls; statement *)
type prog = Prog of fundefn list * (vardecl list) * stmt
[@@deriving show]
