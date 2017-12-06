open BatList
open AST
open Exp
open Interptypes
open Interpintrinsics
open Ais
open Lexing

let function_locals : function_locals_env ref = ref (Symtab.create ())
let functions : function_env ref = ref (Symtab.create ())

let anim_commands : (anim_com list ref) = ref []

let anim_id_counter = ref 0
let fresh_anim_id () =
  let id = !anim_id_counter in
  anim_id_counter := !anim_id_counter + 1;
  id

let reset_anim () =
  anim_commands := [];
  anim_id_counter := 0

let add_anim_com c =
  anim_commands := c :: !anim_commands

let anim_create (t : anim_type) (lbl : string) (lnum : int )=
  let aid = fresh_anim_id () in
  add_anim_com (CStep ([ICreate(aid, t, lbl)], lnum));
  aid

let anim_destroy (frame : value_env) id (lnum : int) =
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([IDestroy aid], lnum))
  | None -> raise (Unbound_identifier "in anim_destroy")

let anim_insert frame id i v lnum =
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     let v' = anim_value_of_value v in
     add_anim_com (CStep ([IInsert(aid, i, v')], lnum))
  | None -> raise (Unbound_identifier "in anim_insert")

let anim_delete frame id i lnum =
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([IDelete(aid, i)], lnum))
  | None -> raise (Unbound_identifier "in anim_delete")

let anim_clear frame id lnum =
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([IClear(aid)], lnum))
  | None -> raise (Unbound_identifier "in anim_clear")

let anim_assign_var frame id v lnum =
  let v' = anim_value_of_value v in
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([IAssign(LVar(aid), v')], lnum))
  | None -> raise (Unbound_identifier "in anim_assign_var")

let anim_assign_array frame id i v lnum =
  let v' = anim_value_of_value v in
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([IAssign(LArrayCell(aid, i), v')], lnum))
  | None -> raise (Unbound_identifier "in anim_assign_array")

let anim_swap frame id i1 i2 lnum =
  match Symtab.get id frame with
  | Some (_, aid, _) ->
     add_anim_com (CStep ([ISwap (aid, i1, i2)], lnum))
  | None -> raise (Unbound_identifier "in anim_swap")

let lookup_id frame i =
    match Symtab.get i frame with
  | Some (v, _, _) -> v
  | _ -> raise (Unbound_identifier "in lookup_id")

let lookup_aid frame i =
    match Symtab.get i frame with
  | Some (_, aid, _) -> aid
  | _ -> raise (Unbound_identifier "in lookup_aid")

let lookup_fun i =
  match Symtab.get i !functions with
  | Some f -> f
  | _ -> raise (Unbound_identifier "in lookup_fun")

let lookup_locals i =
  match Symtab.get i !function_locals with
  | Some f -> f
  | _ -> raise (Unbound_identifier "in lookup_locals")

let assert_id_exp e =
  match e.exp_of with
  | EId i -> i
  | _ -> raise (Expected_id
           "Expected id as first argument to intrinsic function.")

(* Create a stack frame with local variables given initial values *)
let init_frame locals lnum : value_env =
  let frame = ref (Symtab.create ()) in
  let instrs = map (fun ti ->
                   let aid = fresh_anim_id () in
                   frame := Symtab.set ti.id_of
                              (ref (default_value ti.ty_of), aid, false) !frame;
                   ICreate(aid, (if is_array_ty ti.ty_of then
                                 ATArray else ATVar), string_of_id ti.id_of))
                 locals in
  add_anim_com (CStep (instrs, lnum));
  !frame

let cleanup_frame (frame : value_env) lnum =
  (* Deallocate stack frame *)
  let instrs = Symtab.fold (fun v acc ->
                   match v with
                   | (_, aid, by_ref) ->
                      if not by_ref then IDestroy aid :: acc
                      else acc
                 ) frame [] in
  add_anim_com (CStep (instrs, lnum))

let rec list_eq eq l1 l2 =
  match l1 with
  | [] ->
     (match l2 with
      | [] -> true
      | _ -> false)
  | h1 :: t1 ->
     (match l2 with
      | [] -> false
      | h2 :: t2 ->
         eq h1 h2 && (list_eq eq t1 t2))

(* cleaner code but doesn't short-circuit *)
(* let list_eq' eq l1 l2 = *)
(*   fold_right (fun (el1, el2) acc -> acc && eq el1 el2) (combine l1 l2) true *)

let rec val_eq v1 v2 =
  match v1 with
  | UnitVal -> assert_unit v2; true
  | BoolVal b1 -> b1 = (assert_bool v2)
  | IntVal i1 -> i1 = (assert_int32 v2)
  | StringVal s1 -> s1 = (assert_string v2)
  | ArrayVal a1 ->
     let a2 = assert_array v2 in
     BatList.length a1 = BatList.length a2 &&
       list_eq val_eq a1 a2

let rec val_plus v1 v2 =
  match v1 with
  | IntVal i ->
     IntVal (Int32.add (assert_int32 v1) (assert_int32 v2))
  | StringVal s ->
     StringVal (assert_string v1 ^ (assert_string v2))
  | _ -> raise (Binop_error "Expected either int or string operands")

let rec interp_com (frame : value_env) (c : com)
        : (value * int) =
  match c.com_of with
  | CCall (id, e_lis, t) ->
     let _ = interp_call frame id e_lis t c.start_of.pos_lnum in
     (UnitVal, -1)
  | CIf (e, c1, c2) ->
     let v = interp_exp frame e in
     let b = assert_bool v in
     interp_com_block frame (if b then c1 else c2)
  | CAss (id, e) -> interp_ass frame id e
  | CWhile (e, c1) ->
     if assert_bool (interp_exp frame e) then
       (let (v, pos) = interp_com_block frame c1 in
        if v = UnitVal then interp_com frame c
        else (v, pos))
     else (UnitVal, -1)
  | CReturn e -> (interp_exp frame e, c.start_of.pos_lnum)

and interp_com_block (frame : value_env)
      (block : com list) : (value * int) =
  match block with
  | [] -> (UnitVal, -1)
  | c :: tl ->
     let (v, lnum) = interp_com frame c in
     if v = UnitVal then interp_com_block frame tl
     else (v, lnum)

and interp_exp (frame : value_env) (e : exp) : value =
  match e.exp_of with
  | EInt i -> IntVal i
  | EString s -> StringVal s
  | EId i -> !(lookup_id frame i)
  | ECall (i, e_lst, t) -> interp_call frame i e_lst t e.start_of.pos_lnum
  | EUnop (u, e1) -> interp_unop frame u e1
  | EBinop (b, e1, e2) -> interp_binop frame b e1 e2
  | EUnit -> UnitVal
  | ETrue -> BoolVal true
  | EFalse -> BoolVal false
  | EArray e_lst -> ArrayVal (map (interp_exp frame) e_lst)

and interp_call (frame : value_env) id e_lst t lnum =
  let arg_vals = BatList.map (interp_exp frame) e_lst in
  let i = string_of_id id in
  match i with
  | "create" -> create t
  | "at" ->
     let i = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     at (hd arg_vals) i
  | "set" ->
     (* Pass by reference *)
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     let i = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     let v = (BatList.at arg_vals 2) in
     anim_assign_array frame id' i v lnum;
     set arr i v; UnitVal
  | "swap" ->
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     let i1 = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     let i2 = (Int32.to_int (assert_int32 (BatList.at arg_vals 2))) in
     anim_swap frame id' i1 i2 lnum;
     swap arr i1 i2; UnitVal
  | "clear" ->
     let id' = assert_id_exp (hd e_lst) in
     anim_clear frame id' lnum;
     clear frame id'; UnitVal
  | "insert" ->
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     let i = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     let v = (BatList.at arg_vals 2) in
     anim_insert frame id' i v lnum;
     insert arr i v; UnitVal
  | "delete" ->
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     let i = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     anim_delete frame id' i lnum;
     delete arr i; UnitVal
  | "size" -> size (hd arg_vals)
  | "concat" ->
     concat (hd arg_vals) (BatList.at arg_vals 1)
  | "clone" -> 
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     clone arr
  | "subarray" ->
     let id' = assert_id_exp (hd e_lst) in
     let arr = lookup_id frame id' in
     let start = (Int32.to_int (assert_int32 (BatList.at arg_vals 1))) in
     let length = (Int32.to_int (assert_int32 (BatList.at arg_vals 2))) in
     subarray arr start length
  | _ ->
     let f = lookup_fun id in
     add_anim_com (CFrameBegin (string_of_id f.nm, lnum));
     let locals = lookup_locals f.nm in
     let new_frame = init_frame_with_args frame locals e_lst f
                       f.start_of.pos_lnum in
     let (res, _) = interp_com_block new_frame f.body in
     let lnum' = f.end_of.pos_lnum in
     cleanup_frame new_frame lnum';
     add_anim_com (CFrameEnd lnum');
     res

and interp_access frame i e =
  let arr = assert_array !(lookup_id frame i) in
  let index = assert_int32 (interp_exp frame e) in
  BatList.at arr (Int32.to_int index)

and interp_ass frame id e =
  let var = lookup_id frame id in
  let v = interp_exp frame e in
  anim_assign_var frame id v e.start_of.pos_lnum;
  var := v; (UnitVal, e.start_of.pos_lnum)

and interp_unop frame u e =
  match u with
  | UMinus ->
     IntVal (interp_exp frame e |> assert_int32 |> Int32.neg)
     (* let v = interp_exp frame e in *)
     (* IntVal (Int32.neg (assert_int32 v)) *)
  | UNot ->
     BoolVal (interp_exp frame e |> assert_bool |> not)
     (* let v = interp_exp frame e in *)
     (* BoolVal (not (assert_bool v)) *)

and interp_binop frame b e1 e2 =
  if is_logical b then
    match b with
    | BAnd ->
       let v1 = interp_exp frame e1 in
       if assert_bool v1 then
         let v2 = interp_exp frame e2 in
         BoolVal (assert_bool v2)
       else
         BoolVal false
    | BOr ->
       let v1 = interp_exp frame e1 in
       if assert_bool v1 then
         BoolVal true
       else
         let v2 = interp_exp frame e2 in
         BoolVal (assert_bool v2)
    | _ -> raise (Binop_error "interp_binop: impossible case")
  else
    let v1 = interp_exp frame e1 in
    let v2 = interp_exp frame e2 in
    match b with
    | BPlus -> val_plus v1 v2
    | BMinus ->
       IntVal (Int32.sub (assert_int32 v1) (assert_int32 v2))
    | BTimes ->
       IntVal (Int32.mul (assert_int32 v1) (assert_int32 v2))
    | BDiv ->
       IntVal (Int32.div (assert_int32 v1) (assert_int32 v2))
    | BEq -> BoolVal (val_eq v1 v2)
    | BNeq -> BoolVal (not (val_eq v1 v2))
    | BLt ->
       BoolVal (Int32.compare (assert_int32 v1) (assert_int32 v2) < 0)
    | BGt ->
       BoolVal (Int32.compare (assert_int32 v1) (assert_int32 v2) > 0)
    | BLe ->
       BoolVal (Int32.compare (assert_int32 v1) (assert_int32 v2) <= 0)
    | BGe ->
       BoolVal (Int32.compare (assert_int32 v1) (assert_int32 v2) >= 0)
    | _ -> raise (Binop_error "interp_binop: impossible case")

and init_frame_with_args cur_frame locals arg_exps f lnum
    : value_env =
  (* Allocate locals on stack frame *)
  let frame = ref (Symtab.create ()) in
  let instrs = map (fun ti ->
                   let aid = fresh_anim_id () in
                   frame := Symtab.set ti.id_of
                              (ref (default_value ti.ty_of), aid, false) !frame;
                   ICreate(aid,
                           (if is_array_ty ti.ty_of then ATArray else ATVar),
                           string_of_id ti.id_of))
                 locals in
  add_anim_com (CStep (instrs, lnum));
  (* BatList.iter *)
  (*   (fun ti -> *)
  (*     let aid = anim_create *)
  (*                 (if is_array_ty ti.ty_of then ATArray else ATVar) in *)
  (*     frame := Symtab.set ti.id_of (ref (default_value ti.ty_of), aid, false) *)
  (*                !frame) *)
  (*   locals; *)
  (* Allocate formal parameters on the stack frame*)
  BatList.iter2
    (fun e ai ->
      let ti = ai.tid_of in
      (* Pass by reference (only works with Id arguments) *)
      if ai.by_ref then
        let id = assert_id_exp e in
        frame := Symtab.set ti.id_of
                   (lookup_id cur_frame id, lookup_aid cur_frame id, true)
                   !frame
      (* Pass by value *)
      else
        let v = interp_exp cur_frame e in
        let aid = anim_create
                    (if is_array_ty ti.ty_of then ATArray else ATVar)
                    (string_of_id ti.id_of) lnum in
        frame := Symtab.set ti.id_of (ref v, aid, false) !frame;
        anim_assign_var !frame ti.id_of v lnum)
  arg_exps f.args;
  !frame

(* May contain duplicates *)
let rec scan_com_for_vars c =
  match c.com_of with
  | CIf (e, c_lst1, c_lst2) ->
       (fold_left (fun acc x -> acc @ (scan_com_for_vars x)) [] c_lst1) @
         (fold_left (fun acc x -> acc @ (scan_com_for_vars x)) [] c_lst2)
  | CAss (id, e) -> [mk_tid id e.ety_of]
  | CWhile (e, c_lst) ->
     (fold_left (fun acc x -> acc @ (scan_com_for_vars x)) [] c_lst)
  | _ -> []

let build_function_locals fundefs =
  BatList.iter
    (fun f ->
      function_locals :=
        Symtab.set f.nm
          (* Use unique to eliminate duplicates *)
          (unique (fold_left (fun acc x -> acc @ scan_com_for_vars x)
                     [] f.body))
          !function_locals)
    fundefs

let interp_prog (p : prog) : (value * anim_prog) =
  reset_anim();
  (* Add functions to the function environment *)
  BatList.iter (fun f -> functions := Symtab.set f.nm f !functions)
               p.fundefs;
  (* Add locals of each function to the function locals environment *)
  build_function_locals p.fundefs;
  (* Find names and types of variables used in the "main" scope *)
  let main_locals = 
    unique (fold_left (fun acc x -> acc @ scan_com_for_vars x) [] p.main) in
  (* initialize the main "stack frame" *)
  let main_frame = init_frame main_locals
                     (BatList.at p.main 0).start_of.pos_lnum in
  let (result, _) = interp_com_block main_frame p.main in
  (result, AProg ("", rev !anim_commands))
