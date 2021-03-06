open Batteries

open AST

type 'a t = (id, 'a) BatMap.t
		      
let create _ = BatMap.empty

let get k m =
  try Some (BatMap.find k m)
  with Not_found -> None

let set k v m = BatMap.add k v m

let iter (f : id -> 'a -> unit) m =
  BatMap.iter f m

let fold f m d =
  BatMap.fold f m d
