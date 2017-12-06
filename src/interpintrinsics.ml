open AST
open Interptypes

let create t = ArrayVal (BatList.make 0 (default_value t))

let clear frame id =
  match Symtab.get id frame with
  | Some (a, _, _) ->
     let av = assert_array !a in
     a := ArrayVal (BatList.take 0 av)
  | None -> raise (Unbound_identifier "intrinsic clear")

let at (arr : value) =
  assert_array arr |> BatList.at

let set (arr : value ref) i v =
  let a = assert_array !arr in
  arr := ArrayVal (BatList.modify_at i (fun _ -> v) a)

let swap (arr : value ref) i1 i2 =
  let a = assert_array !arr in
  let v1 = BatList.at a i1 in
  let v2 = BatList.at a i2 in
  let a' = BatList.modify_at i1 (fun _ -> v2) a in
  arr := ArrayVal (BatList.modify_at i2 (fun _ -> v1) a')

let insert (arr : value ref) i v =
  let a = assert_array !arr in
  arr := ArrayVal (BatList.take i a @ (v :: BatList.drop i a))

let delete (arr : value ref) i =
  arr := ArrayVal (assert_array !arr |> BatList.remove_at i)

let size (arr : value) =
  IntVal (assert_array arr |> BatList.length |> Int32.of_int)

let concat (arr1 : value) (arr2 : value) =
  ArrayVal (assert_array arr1 @ assert_array arr2)

let clone (arr : value ref) =
  !arr

let subarray (arr : value ref) start length =
  ArrayVal (assert_array !arr |> BatList.drop start |> BatList.take length)

(* let read (t : ty) (s : string) = *)
  
