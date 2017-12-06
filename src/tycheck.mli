open AST
open Exp

(** [Ty_error err]: A type error, with error message [err] *)
exception Ty_error of string

val tycheck_prog : prog -> prog
