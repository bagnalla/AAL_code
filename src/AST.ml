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

let is_arith_ty (t : ty) = 
  match t with
  | TyInt | TyT -> true
  | _ -> false

let is_array_ty (t : ty) =
  match t with
  | TyArray _ -> true
  | _ -> false

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyString -> "string"
  | TyUnit -> "unit"
  | TyArray t -> string_of_ty t ^ " array"
  | TyT -> "T"

let ty_of_array = function
  | TyArray t -> t
  | _ -> raise (Internal_error "expected array type in ty_of_array")

type id =
  | Id of string

let string_of_id i = match i with Id s -> s

type tid = 
  { id_of : id;
    ty_of : ty }

let mk_tid (i : id) (t : 'a) : tid =
  { id_of = i;
    ty_of = t }

type argid =
  { tid_of : tid;
    by_ref : bool }

let mk_argid (ti : tid) (byref : bool) =
  { tid_of = ti;
    by_ref = byref }

type unop =
  | UMinus
  | UNot

let string_of_unop = function
  | UMinus -> "UMinus"
  | UNot -> "UNot"

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

let string_of_binop = function
  | BPlus -> "BPlus"
  | BMinus -> "BMinus"
  | BTimes -> "BTimes"
  | BDiv -> "BDiv"
  | BAnd -> "BAnd"
  | BOr -> "BOr"
  | BLt -> "BLt"
  | BGt -> "BGt"
  | BLe -> "BLe"
  | BGe -> "BGe"
  | BEq -> "BEq"
  | BNeq -> "BNeq"

(* type 'exp fundef =  *)
(*   { nm  : id; *)
(*     args : argid list;       *)
(*     ret_ty : ty; *)
(*     body : 'exp; *)
(*   } *)

(* type 'exp prog = *)
(*   { fundefs : ('exp fundef) list; *)
(*     result  : 'exp; *)
(*   } *)

let pp_id ppf (i : id) : unit =
  match i with
  | Id s -> fprintf ppf "%s" s

(* let pp_label ppf (l : label) : unit = *)
(*   match l with *)
(*   | Label x -> pp_id ppf x *)

let pp_int ppf (i : int) : unit =
  fprintf ppf "%d" i

let pp_stringconst ppf (s : string) : unit =
  fprintf ppf "%s" s

let rec pp_ty ppf (t : ty) : unit =
  match t with
  | TyInt -> fprintf ppf "int"
  | TyString -> fprintf ppf "string"
  | TyBool -> fprintf ppf "bool"
  | TyArray t2 -> fprintf ppf "%a@ array" pp_ty t2
  | TyUnit -> fprintf ppf "unit"
  | TyT -> fprintf ppf "T"
		 
let pp_tid ppf (i : tid) : unit = 
  fprintf ppf "%a" pp_id i.id_of

let pp_unop ppf (u : unop) : unit = 
  match u with
  | UMinus -> fprintf ppf "-"
  | UNot -> fprintf ppf "!"

let pp_binop ppf (b : binop) : unit =
  match b with
  | BPlus -> fprintf ppf "+"
  | BMinus -> fprintf ppf "-"
  | BTimes -> fprintf ppf "*"
  | BDiv -> fprintf ppf "/"
  | BAnd -> fprintf ppf "&&"
  | BOr -> fprintf ppf "||"
  | BLt -> fprintf ppf "<"
  | BGt -> fprintf ppf ">"
  | BLe -> fprintf ppf "<="
  | BGe -> fprintf ppf ">="
  | BEq -> fprintf ppf "=="
  | BNeq -> fprintf ppf "!="

let rec pp_list ppf pp sep l : unit =
  match l with
  | [] -> ()
  | x :: [] -> fprintf ppf "%a" pp x
  | x :: l' ->
     fprintf ppf "%a%a" pp x sep ();
     pp_list ppf pp sep l'

let pp_string ppf s : unit -> unit =
  fun _ -> fprintf ppf s

let pp_comma ppf = pp_string ppf ", "
			     
let pp_semi ppf = pp_string ppf "; "
			    
let pp_nl ppf = pp_string ppf "@\n"			    
	  
(* let pp_fundef ppf pp ppty f : unit = *)
(*   fprintf ppf "@[<1>def %a(%a)@ :@ %a {@\n@[%a@]@]@\n}@," *)
(* 	  pp_id f.nm *)
(* 	  (fun ppf0 -> *)
(* 	   pp_list ppf0 (fun ppf1 x -> *)
(*                let ti = x.tid_of in *)
(* 	       fprintf ppf1 "%a : %a" pp_id ti.id_of ppty ti.ty_of) pp_comma) *)
(*           f.args ppty f.ret_ty pp f.body *)

(* let pp_prog ppf pp ppty sep p = *)
(*   fprintf ppf "@[<0>%a%a@[%a@]@]@\n@." *)
(* 	  (fun ppf0 -> BatList.iter (pp_fundef ppf0 pp ppty)) p.fundefs *)
(* 	  sep () *)
(* 	  pp p.result *)

(* let pp_val ppf (v : value) : unit = *)
(*   match v with *)
(*   | VInt n -> pp_int ppf (Int32.to_int n) *)
(*   | VFloat f -> pp_float ppf f *)
(*   | VUnit -> pp_string ppf "255" () (\*the low-order bits of -1*\) *)
(*   | VBool true -> pp_string ppf "1" () *)
(*   | VBool false -> pp_string ppf "0" () *)
(*   | VLoc l -> fprintf ppf "VLoc %a" pp_id l			      *)

let pp_to_string pp : string =
  pp str_formatter;
  flush_str_formatter ()
