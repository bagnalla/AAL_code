open Batteries
open BatFormat

exception Internal_error of string

type ty =
  | TyInt
  | TyBool
  | TyString
  | TyUnit
  | TyArray of ty
  | TyT

val is_arith_ty : ty -> bool

val is_array_ty : ty -> bool

val string_of_ty : ty -> string

val ty_of_array : ty -> ty

type id =
  | Id of string

val string_of_id : id -> string

type tid = 
  { id_of : id;
    ty_of : ty }

val mk_tid : id -> ty -> tid

type argid =
  { tid_of : tid;
    by_ref : bool }

val mk_argid : tid -> bool -> argid

type unop =
  | UMinus
  | UNot

val string_of_unop : unop -> string

type binop =
  | BPlus
  | BMinus
  | BTimes
  | BDiv
  | BAnd
  | BOr
  | BLt
  | BGt
  | BLe
  | BGe
  | BEq
  | BNeq

val string_of_binop : binop -> string

(* type fundef =  *)
(*   { nm  : id; *)
(*     args : argid list; *)
(*     ret_ty : ty; *)
(*     body : exp; *)
(*   } *)

(* type 'exp prog = *)
(*   { *)
(*     fundefs : ('exp fundef) list; *)
(*     result  : 'exp; *)
(*   } *)
    
val pp_id : formatter -> id -> unit
(* val pp_label : formatter -> label -> unit *)
val pp_int : formatter -> int -> unit
val pp_stringconst : formatter -> string -> unit
val pp_ty : formatter -> ty -> unit
val pp_tid : formatter -> tid -> unit
val pp_unop : formatter -> unop -> unit
val pp_binop : formatter -> binop -> unit
val pp_list : formatter ->
  (formatter -> 'a -> unit) -> (formatter -> unit -> unit) ->
  'a list ->
  unit
val pp_comma : formatter -> unit -> unit
val pp_semi : formatter -> unit -> unit
val pp_nl : formatter -> unit -> unit
(* val pp_fundef : formatter -> *)
(*   (formatter -> 'exp -> unit) -> *)
(*   (formatter -> ty -> unit) -> *)
(*   'exp fundef -> *)
(*   unit *)
(* val pp_prog : formatter -> *)
(*   (formatter -> 'exp -> unit) -> *)
(*   (formatter -> ty -> unit) -> *)
(*   (formatter -> unit -> unit) -> *)
(*   'exp prog -> *)
  (* unit *)
(* val pp_val : formatter -> value -> unit *)
val pp_to_string : (formatter -> unit) -> string
