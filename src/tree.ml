open BatList

exception Sexp_parse_error

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree list

let rec map_tree (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Leaf a -> Leaf (f a)
  | Node (a, t_list) ->
     Node (f a, map (map_tree f) t_list)

let rec sexp_of_tree (t : string tree) =
  match t with
  | Leaf s -> "(" ^ s ^ ")"
  | Node (s, t_list) ->
     "(" ^ s ^ (fold_left (^) "" (map sexp_of_tree t_list)) ^ ")"

(* let rec tree_of_sexp (s : string) : string tree = *)
  (* let rec parse_sexp l = *)
  (*   if length l = 0 or at l 0 <> '(' then *)
  (*     raise Sexp_parse_error *)
  (*   else *)
  (*     let l' = drop_while BatChar.is_whitespace (tl l) in *)
  (*     () *)
      
  (* and go s' acc = *)
  (*   let s'' = String.trim s' in *)
    
  (* (\* let l = BatString.to_list s in *\) *)
  (* let s' = String.trim s in *)
  (* if String.length s' = 0 then raise Sexp_parse_error else *)
  (*   String.get s' 0 = '(' then  *)

let write_tree_to_file filename (t : 'a tree) =
  let outchan = open_out filename in
  Marshal.to_channel outchan t []

let read_tree_from_file filename : 'a tree =
  let inchan = open_in filename in
  Marshal.from_channel inchan

