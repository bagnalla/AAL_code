open AST

(** The [Symtab] module defines a symbol table data structure. *)

(** The abstract type of symbol tables mapping identifiers [id] to 
    values of type ['a]. *)
type 'a t

(** Create an empty symbol table. *)
val create : unit -> 'a t

(** Get the value associated with an [id], if one exists. *)			
val get : id -> 'a t -> 'a option

(** Set the value associated with an [id]. *)
val set : id -> 'a -> 'a t -> 'a t

val iter : (id -> 'a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
