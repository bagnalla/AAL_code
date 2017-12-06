open Batteries
open BatFormat
open Lexing
open AST

type raw_exp =
  | EInt of int32
  | EString of string
  | EId of id
  | ECall of id * exp list * ty (* type parameter *)
  | EUnop of unop * exp
  | EBinop of binop * exp * exp
  | EUnit
  | ETrue
  | EFalse
  | EArray of exp list

and exp = 
  { start_of : Lexing.position;
    end_of : Lexing.position;
    exp_of : raw_exp;
    ety_of : ty }

type raw_com =
  | CCall of id * exp list * ty (* type parameter *)
  | CIf of exp * com list * com list
  | CAss of id * exp
  | CWhile of exp * com list
  | CReturn of exp

and com =
  { start_of : Lexing.position;
    end_of : Lexing.position;
    com_of : raw_com }

type fundef = 
  { nm  : id;
    args : argid list;
    ret_ty : ty;
    body : com list;
    start_of : Lexing.position;
    end_of : Lexing.position }

type prog =
  {
    fundefs : fundef list;
    main  : com list;
  }

(** Check whether two types are equal *)
val ty_eq : ty -> ty -> bool

(** A "smart constructor" for [ESeq], implementing the following equations: 
- [exp_seq (ESeq l1) (ESeq l2) = ESeq (l1 ^ l2)]
- [exp_seq e1 (ESeq l2)        = ESeq (e1 :: l2)]
- [exp_seq (ESeq l1) e2        = ESeq (l1 ^ [e2])]
- [exp_seq e1 e2               = ESeq (e1 :: e2 :: nil)] *)
(* val exp_seq : exp -> exp -> raw_exp *)

(** Rename id [old_id] to [new_id] in [e], being careful to 
  avoid capture by inner [ELet]s *)
val subst_var_exp : id -> id -> exp -> exp

val subst_var_com : id -> id -> com -> com

val pp_com : formatter -> com -> unit
val pp_com_list : formatter -> com list -> unit

(** Pretty-print an expression *)
val pp_exp : formatter -> exp -> unit

(** Pretty-print a type-annotated expression *)
val pp_texp : formatter -> exp -> unit

(** Pretty-print a source-code position *)
val pp_pos_com : formatter -> com -> unit
val pp_pos : formatter -> exp -> unit

val pp_fundef : formatter ->
  (formatter -> com list -> unit) ->
  (formatter -> ty -> unit) ->
  fundef -> unit
val pp_prog : formatter ->
  (formatter -> com list -> unit) ->
  (formatter -> ty -> unit) ->
  (formatter -> unit -> unit) ->
  prog -> unit
