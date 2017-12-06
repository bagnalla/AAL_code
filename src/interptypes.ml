open BatList
open AST
open Exp
open Ais

exception Unbound_identifier of string
exception Expected_unit of string
exception Expected_bool of string
exception Expected_int of string
exception Expected_string of string
exception Expected_array of string
exception Expected_id of string
exception Binop_error of string
exception Impossible

type value =
  | UnitVal
  | IntVal of int32
  | BoolVal of bool
  | StringVal of string
  | ArrayVal of value list

let rec default_value = function
  | TyInt -> IntVal Int32.zero
  | TyBool -> BoolVal false
  | TyString -> StringVal ""
  | TyArray t -> ArrayVal []
  | _ -> raise Impossible

let rec string_of_value = function
  | UnitVal -> "unit"
  | IntVal i -> Int32.to_string i
  | BoolVal b -> string_of_bool b
  | StringVal s -> s
  | ArrayVal a ->
     if length a = 0 then "[]" else
       "[" ^ fold_left
               (fun acc x -> acc ^ (string_of_value x) ^ ", ") ""
               (take (length a - 1) a)
       ^ (string_of_value (last a)) ^ "]"

type function_locals_env = (tid list) Symtab.t
type value_env = (value ref * anim_id * bool) Symtab.t
type function_env = fundef Symtab.t

let assert_unit = function
  | UnitVal -> ()
  | _ -> raise (Expected_unit "in assert_unit")

let assert_bool = function
  | BoolVal b -> b
  | _ -> raise (Expected_bool "in assert_bool")

let assert_int32 = function
  | IntVal i -> i
  | _ -> raise (Expected_int "in assert_int")

let assert_int = function
  | IntVal i -> Int32.to_int i
  | _ -> raise (Expected_int "in assert_int")

let assert_string = function
  | StringVal s -> s
  | _ -> raise (Expected_string "in assert_string")

let assert_array = function
  | ArrayVal a -> a
  | _ -> raise (Expected_array "in assert_array")

let rec anim_value_of_value = function
  | UnitVal -> AAtom "unit"
  | IntVal i -> AAtom (Int32.to_string i)
  | BoolVal b -> AAtom (string_of_bool b)
  | StringVal s -> AAtom s
  | ArrayVal vs -> AArray (BatList.map anim_value_of_value vs)

let is_logical = function
  | BAnd | BOr -> true
  | _          -> false
