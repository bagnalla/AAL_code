open BatList
open AST
open Exp
open Tree

let rec tree_of_exp e =
  match e.exp_of with
  | EInt i -> Leaf ("EInt " ^ Int32.to_string i)
  | EString s -> Leaf ("EString " ^ s)
  | EId id -> Leaf ("EId " ^ string_of_id id)
  | ECall (id, e_list, t) ->
     Node ("ECall " ^ string_of_id id, map tree_of_exp e_list)
  | EUnop (u, e1) ->
     Node (string_of_unop u, [tree_of_exp e1])
  | EBinop (b, e1, e2) ->
     Node (string_of_binop b, [tree_of_exp e1; tree_of_exp e2])
  | EUnit -> Leaf "EUnit"
  | ETrue -> Leaf "ETrue"
  | EFalse -> Leaf "EFalse"
  | EArray e_list ->
     Node ("EArray", map tree_of_exp e_list)

let rec tree_of_com c =
  match c.com_of with
  | CCall (id, e_list, t) ->
     Node ("CCall " ^ string_of_id id, map tree_of_exp e_list)
  | CIf (e1, c_list1, c_list2) ->
     let lbranch = Node ("", map tree_of_com c_list1) in
     let rbranch = Node ("", map tree_of_com c_list2) in
     Node ("CIf", [tree_of_exp e1; lbranch; rbranch])
  | CAss (id, e1) ->
     Node ("CAss " ^ string_of_id id, [tree_of_exp e1])
  | CWhile (e1, c_list) ->
     let while_body = Node ("", map tree_of_com c_list) in
     Node ("CWhile", [tree_of_exp e1; while_body])
  | CReturn e1 -> Node ("CReturn", [tree_of_exp e1])

let tree_of_fundef f =
  let f_body = Node ("", map tree_of_com f.body) in
  Node (string_of_id f.nm, [f_body])

let tree_of_prog p =
  let main = Node ("", map tree_of_com p.main) in
  Node ("prog", map tree_of_fundef p.fundefs @ [main])
