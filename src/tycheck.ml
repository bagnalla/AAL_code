open Batteries
open BatFormat
open BatList

open Lexing

open AST
open Exp

(** Declare a new exception type, Ty_error, which takes as
    its first argument a message describing the type error *)

exception Ty_error of string

(** Given an error string, raise the associated type error *)

let raise_ty_err (err : string) = raise (Ty_error err)

(** The type of type environments \Gamma, mapping identifiers to the
    types they've been assigned *)

type ty_env = ty Symtab.t

(** The type of the global environment \Delta, mapping function names
    (identifiers) to the types of their arguments and to their
    result type *)

type globs_env = (ty list * ty) Symtab.t
type poly_env = fundef Symtab.t

(** delta is a file-scope reference to the global type environment *)

let delta : globs_env ref = ref (Symtab.create ())
let poly_functions : poly_env ref = ref (Symtab.create())

let reset () =
  delta := Symtab.create ();
  poly_functions := Symtab.create ()

(** Return the argument types and return type associated with
    function id [f] in [delta], or raise a type error. *)

let ety_of_funid (f : id) : (ty list * ty) =
  match Symtab.get f !delta with
  | None ->
    raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
      "unbound function identifier '%a'" pp_id f))
  | Some tys -> tys

let lookup_poly (f : id) : fundef =
  match Symtab.get f !poly_functions with
  | None ->
    raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
      "unbound function identifier '%a'" pp_id f))
  | Some f' -> f'

(** Is id [x] bound in environment [gamma]? *)

let is_bound (gamma : ty_env) (x : id) : bool =
  BatOption.is_some (Symtab.get x gamma)

let is_intrinsic_id = function
  | Id i ->
     match i with
     | "at" | "set" | "swap" | "clear" | "insert" | "size"
       | "concat" | "create" | "delete" | "subarray" -> true
     | _ -> false

let instantiate_function fid t =
  let f = lookup_poly fid in
  let rec subst_t_exp e =
    match e.exp_of with
    | EId _ -> if e.ety_of = TyT then { e with ety_of = t } else e
    | ECall (id, e_lst, t') ->
       let e_lst' = map subst_t_exp e_lst in
       { e with
         exp_of = ECall (id, e_lst', t');
         ety_of = if e.ety_of = TyT then t else e.ety_of }
    | EUnop (u, e1) ->
       let e1' = subst_t_exp e1 in
       { e with
         exp_of = EUnop (u, e1');
         ety_of = if e.ety_of = TyT then t else e.ety_of }
    | EBinop (b, e1, e2) ->
       let e1' = subst_t_exp e1 in
       let e2' = subst_t_exp e2 in
       { e with
         exp_of = EBinop (b, e1', e2');
         ety_of = if e.ety_of = TyT then t else e.ety_of }
    | EArray e_lst ->
       let e_lst' = map subst_t_exp e_lst in
       { e with
         exp_of = EArray e_lst';
         ety_of = if e.ety_of = TyT then t else e.ety_of }
    | _ -> e
  in
  let rec subst_t_com c =
    match c.com_of with
    | CCall (id, e_lst, t') ->
       let e_lst' = map subst_t_exp e_lst in
       { c with com_of = CCall (id, e_lst', t') }
    | CIf (e1, c_lst1, c_lst2) ->
       let e1' = subst_t_exp e1 in
       let c_lst1' = map subst_t_com c_lst1 in
       let c_lst2' = map subst_t_com c_lst2 in
       { c with com_of = CIf (e1', c_lst1', c_lst2') }
    | CAss (id, e1) ->
       let e1' = subst_t_exp e1 in
       { c with com_of = CAss (id, e1') }
    | CWhile (e1, c_lst) ->
       let e1' = subst_t_exp e1 in
       let c_lst' = map subst_t_com c_lst in
       { c with com_of = CWhile (e1', c_lst') }
    | CReturn e1 -> { c with com_of = CReturn (subst_t_exp e1) }
  in
  let coms = map subst_t_com f.body in
  { f with
    body = coms }


(* let instantiate_function fid t = *)
(*   match Symtab.get fid !poly_functions with *)
(*   | None -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf *)
(*               "unbound polymorphic function identifier '%a'" pp_id fid)) *)
(*   | Some f -> *)
(*      let rec subst_t_exp e = *)
(*        match e.exp_of with *)
(*        | EId _ -> if e.ety_of = TyT then { e with ety_of = t } else e *)
(*        | ECall (id, e_lst, t') -> *)
(*           let e_lst' = map subst_t_exp e_lst in *)
(*           { e with *)
(*             exp_of = ECall (id, e_lst', t'); *)
(*             ety_of = if e.ety_of = TyT then t else e.ety_of } *)
(*        | EUnop (u, e1) -> *)
(*           let e1' = subst_t_exp e1 in *)
(*           { e with *)
(*             exp_of = EUnop (u, e1'); *)
(*             ety_of = if e.ety_of = TyT then t else e.ety_of } *)
(*        | EBinop (b, e1, e2) -> *)
(*           let e1' = subst_t_exp e1 in *)
(*           let e2' = subst_t_exp e2 in *)
(*           { e with *)
(*             exp_of = EBinop (b, e1', e2'); *)
(*             ety_of = if e.ety_of = TyT then t else e.ety_of } *)
(*        | EArray e_lst -> *)
(*           let e_lst' = map subst_t_exp e_lst in *)
(*           { e with *)
(*             exp_of = EArray e_lst'; *)
(*             ety_of = if e.ety_of = TyT then t else e.ety_of } *)
(*        | _ -> e *)
(*      in *)
(*      let rec subst_t_com c = *)
(*        match c.com_of with *)
(*        | CCall (id, e_lst, t') -> *)
(*           let e_lst' = map subst_t_exp e_lst in *)
(*           { c with com_of = CCall (id, e_lst', t') } *)
(*        | CIf (e1, c_lst1, c_lst2) -> *)
(*           let e1' = subst_t_exp e1 in *)
(*           let c_lst1' = map subst_t_com c_lst1 in *)
(*           let c_lst2' = map subst_t_com c_lst2 in *)
(*           { c with com_of = CIf (e1', c_lst1', c_lst2') } *)
(*        | CAss (id, e1) -> *)
(*           let e1' = subst_t_exp e1 in *)
(*           { c with com_of = CAss (id, e1') } *)
(*        | CWhile (e1, c_lst) -> *)
(*           let e1' = subst_t_exp e1 in *)
(*           let c_lst' = map subst_t_com c_lst in *)
(*           { c with com_of = CWhile (e1', c_lst') } *)
(*        | CReturn e1 -> { c with com_of = CReturn (subst_t_exp e1) } *)
(*      in *)
(*      map subst_t_com f.body *)

(* replace all TyT in t with t' *)
let rec instantiate_ty t' t =
  match t with
  | TyT -> t'
  | TyArray t0 -> TyArray (instantiate_ty t' t0)
  | _ -> t

let rec ty_contains_TyT t =
  match t with
  | TyT -> true
  | TyArray t' -> ty_contains_TyT t'
  | _ -> false

let tys_contain_TyT ts =
  fold_left (fun acc x -> acc || ty_contains_TyT x) false ts

(* t contains the TyT, t' contains all concrete types .
   Find the concrete types in t' that corresponds to TyT in t.
   If there is more than one, the error will get caught later. *)
let rec find_ty_of_TyT t t' =
  match t with
  | TyT -> t'
  | TyArray t0 ->
     (match t' with
     | TyArray t0' -> find_ty_of_TyT t0 t0'
     | _ ->  raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
             "expected array type in arguments when inferring type parameter. \
              found '%a' instead" pp_ty t')))
     (* | _ -> raise_ty_err "expected array type when inferring type parameter") *)
  | _ -> raise_ty_err "error in find_TyT_ty. should be impossible"

let infer_ty_param args params =
  let rec go a p =
    match a with
    | [] -> raise_ty_err
              "infer_ty_param failed to infer a typed parameter (bug)"
    | a' :: atl ->
       match p with
       | [] -> raise_ty_err
                 "infer_ty_param failed to infer a typed parameter (bug)"
       | p' :: ptl ->
          if ty_contains_TyT p' then
            find_ty_of_TyT p' a'.ety_of
          else
            go atl ptl
  in
  go args params

(* Make sure every control flow path has an explicit return *)
let rec check_returns (c_lst : com list) : bool =
  match c_lst with
  | [] -> false
  | c :: tl ->
     match c.com_of with
     | CIf (e, c_lst1, c_lst2) ->
        check_returns tl ||
          (check_returns c_lst1 && check_returns c_lst2)
     | CReturn _ -> true
     | _ -> check_returns tl

let rec tycheck_com (gamma : ty_env) (rt : ty) (c : com) : com =
  match c.com_of with
  | CCall (id, e_lst, t) ->
     (* look up function id in global environment *)
     (match Symtab.get id !delta with
     | None -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
                 "unbound function identifier '%a'@ at position %a"
                 pp_id id pp_pos_com c))
     | Some funsig ->
        (* Make sure the return type is unit *)
        if snd funsig <> TyUnit then
          raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
            "function '%a' with return type other than 'Unit'
             being used as a command@ at position %a"
            pp_id id pp_pos_com c))
        else
          let (i, ta, tp, rt) = tycheck_call gamma funsig e_lst id t in
          { c with com_of = CCall (i, ta, tp) })
  | CIf (e, c_lst1, c_lst2) ->
     let te = tycheck_exp gamma e in
     let ce_lst1 = tycheck_com_list gamma rt c_lst1 in
     let ce_lst2 = tycheck_com_list gamma rt c_lst2 in
     { c with com_of = CIf (te, ce_lst1, ce_lst2) }
  | CWhile (e, c_lst) ->
     let te = tycheck_exp gamma e in
     let ce_lst = tycheck_com_list gamma rt c_lst in
     { c with com_of = CWhile (te, ce_lst) }
  | CReturn e ->
     let te = tycheck_exp gamma e in
     if te.ety_of = rt || te.ety_of = TyT || rt = TyT then
       { c with com_of = CReturn te }
     else
       raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
         "type of value being returned '%a' doesn't match the
          return type of the function '%a'@ at position %a"
         pp_ty te.ety_of pp_ty rt pp_pos_com c))
  | _ -> raise_ty_err "bug in tycheck_com"

and tycheck_com_list (gamma : ty_env) (rt : ty) (c_lst : com list) : com list =
  match c_lst with
  | [] -> []
  | c :: tl ->
     match c.com_of with
     | CAss (id, e) ->
        let te = tycheck_exp gamma e in
        let gamma' = Symtab.set id te.ety_of gamma in
        { c with com_of = CAss (id, te) } :: tycheck_com_list gamma' rt tl
     | _ -> tycheck_com gamma rt c :: tycheck_com_list gamma rt tl

and tycheck_poly gamma id rt ty_param =
  if is_intrinsic_id id then ()
  else
    let fun_instance = instantiate_function id ty_param in
    let _ = tycheck_fundef fun_instance in
    (* let _ = tycheck_com_list gamma rt fun_instance in *)
    ()

and print_tys tys =
  iter (fun t -> print_string (string_of_ty t ^ ", ")) tys

and tycheck_call gamma funsig e_lst id t =
  (* If polymorphic, make sure the function typechecks with
     the concrete type being used *)
  if tys_contain_TyT (snd funsig :: (fst funsig)) then
    (* Infer type parameter if necessary *)
    if t = TyT then
      let typed_args = map (tycheck_exp gamma) e_lst in
      let ty_param = infer_ty_param typed_args (fst funsig) in
      let param_types = map (instantiate_ty ty_param) (fst funsig) in
      let rt = instantiate_ty ty_param (snd funsig) in
      (* typecheck args again, against the params this time *)
      let typed_args' = tycheck_args gamma param_types e_lst in
      tycheck_poly gamma id rt ty_param;
      (id, typed_args', ty_param, rt)
    else
      let param_types = map (instantiate_ty t) (fst funsig) in
      let rt = instantiate_ty t (snd funsig) in
      let typed_args = tycheck_args gamma param_types e_lst in
      tycheck_poly gamma id (snd funsig) t;
      (id, typed_args, t, rt)
  else
    let typed_args = tycheck_args gamma (fst funsig) e_lst in
    let rt = instantiate_ty t (snd funsig) in
    (id, typed_args, t, rt)

and tycheck_exp (gamma : ty_env) (e : exp) : exp =
  match e.exp_of with
  | EInt i -> { e with ety_of = TyInt }
  | EString s -> { e with ety_of = TyString }
  | EId x -> (
    match Symtab.get x gamma with
    | None ->
       raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
         "unbound identifier '%a'@ at position %a" pp_id x pp_pos e))
    | Some t -> { e with ety_of = t })
  | ECall (id, exp_lis, t) -> (
      (* look up function id in global environment *)
      match Symtab.get id !delta with
      | None -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
                  "unbound function identifier '%a'@ at position %a"
                  pp_id id pp_pos e))
      | Some funsig ->
         (* Make sure the return type isn't unit *)
         if snd funsig = TyUnit then
           raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
             "function '%a' with return type 'unit' being used
              as an expression@ at position %a"
             pp_id id pp_pos e))
         else
           let (i, ta, tp, rt) = tycheck_call gamma funsig exp_lis id t in
           { e with exp_of = ECall (i, ta, tp);
                    ety_of = rt })
  | EUnop (u, e1) -> tycheck_unop e gamma u e1
  | EBinop (b, e1, e2) -> tycheck_binop e gamma b e1 e2
  | EUnit -> { e with
               exp_of = EUnit;
               ety_of = TyUnit }
  | ETrue -> { e with
               exp_of = ETrue;
               ety_of = TyBool }
  | EFalse -> { e with
                exp_of = EFalse;
                ety_of = TyBool }
  | EArray e_lst ->
     if length e_lst = 0 then
       raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
         "empty array literal at position %a" pp_pos e))
     else
       let te1 = tycheck_exp gamma (hd e_lst) in
       let te_lst = map (fun e -> assert_ty gamma e te1.ety_of) (tl e_lst) in
       { e with exp_of = EArray (te1 :: te_lst); ety_of = TyArray te1.ety_of }

(** [assert_ty gamma e t]: Raise a type error if [e] does not
    have type [t] in [gamma]. Returns a type-annotated version
    of [e] (just as in [tycheck]) *)

and assert_array_ty (gamma : ty_env) (e : exp) : exp =
  let te = tycheck_exp gamma e in
  match te.ety_of with
  | TyArray _ -> te
  | _ -> raise_ty_err (pp_to_string (fun ppf ->
    fprintf ppf "expression '%a'@ expected to have array type at position %a"
    pp_exp e pp_pos e))

and ty_equiv_poly t1 t2 =
  match t1 with
  | TyT -> true
  | TyArray t1' ->
     (match t2 with
     | TyT -> true
     | TyArray t2' ->
        ty_equiv_poly t1' t2'
     | _ -> false)
  | _ -> t2 = TyT

and assert_ty (gamma : ty_env) (e : exp) (t : ty) : exp =
  let te = tycheck_exp gamma e in
  if te.ety_of = t then te
  else raise_ty_err (pp_to_string (fun ppf ->
    fprintf ppf "expression '%a'@ expected to have type '%a'@ at position %a"
    pp_exp e pp_ty t pp_pos e))

(** [assert_arith gamma e]: Raise a type error if [e] does not have an
    arithmetic type (see [exp.ml] and [exp.mli] for the definition of
    "arithmetic type". Returns a type-annotated version of [e]
    (just as in [tycheck]) *)

and assert_arith (gamma : ty_env) (e : exp) : exp =
  let te = tycheck_exp gamma e in
  if is_arith_ty te.ety_of then te else
    raise_ty_err (pp_to_string (fun ppf ->
      fprintf ppf "expression '%a'@ expected to have 
                   arithmetic type@ at position %a"
    pp_exp e pp_pos e))

(* Assert the term has a type that can be used with the plus operator
   (currently int and string) *)
and assert_plus_ty (gamma : ty_env) (e : exp) : exp =
  let te = tycheck_exp gamma e in
  match te.ety_of with
  | TyInt | TyString -> te
  | _ -> raise_ty_err (pp_to_string (fun ppf ->
          fprintf ppf "expression '%a'@ expected to have 
                       type int or string@ at position %a"
                  pp_exp e pp_pos e))

and tycheck_args (gamma : ty_env) (param_types : ty list)
      (args : (exp) list) : (exp) list =
  match param_types with
  | param_type :: param_types_tl -> (
    match args with
    | arg :: args_tl -> assert_ty gamma arg param_type ::
      tycheck_args gamma param_types_tl args_tl
    | [] -> raise_ty_err "not enough arguments to function call"
  )
  | [] -> (
    match args with
    | [] -> []
    | hd :: _ -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
      "too many arguments to function call@ at position %a" pp_pos hd))
  )

(** [tycheck_unop e gamma u e2]: 
    Assumes [e = EUnop(u, e2)].
    Checks that [EUnop(u, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)

and tycheck_unop (e : exp) (gamma : ty_env) (u : unop) (e2 : exp)
  : exp =
  match u with
  | UMinus ->
    let te2 = assert_arith gamma e2 in
    { e with
      exp_of = EUnop(UMinus, te2);
      ety_of = te2.ety_of }
  | UNot ->
    { e with
      exp_of = EUnop(UNot, assert_ty gamma e2 TyBool);
      ety_of = TyBool }

(** [tycheck_binop e gamma b e1 e2]:
    Assumes [e = EBinop(b, e1, e2)].
    Checks that [EBinop(b, e1, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)

and tycheck_binop (e : exp) (gamma : ty_env) (b : binop) (e1 : exp)
  (e2 : exp) : exp =
  match b with
  | BPlus ->
     let te1 = assert_plus_ty gamma e1 in
     let te2 = assert_ty gamma e2 te1.ety_of in
     { e with
       exp_of = EBinop (b, te1, te2);
       ety_of = te1.ety_of }
  | BMinus | BTimes | BDiv ->
     let te1 = assert_arith gamma e1 in
     let te2 = assert_arith gamma e2 in
     if te1.ety_of = te2.ety_of then
       { e with
         exp_of = EBinop(b, te1, te2);
         ety_of = te1.ety_of }
     else raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
       "mismatch of operand types in binary operation '%a'@ at position %a"
       pp_exp e pp_pos e))
  | BAnd | BOr ->
     let te1 = assert_ty gamma e1 TyBool in
     let te2 = assert_ty gamma e2 TyBool in
     { e with
       exp_of = EBinop(b, te1, te2);
       ety_of = TyBool}
  | BLt | BGt | BLe | BGe ->
     let te1 = assert_arith gamma e1 in
     let te2 = assert_arith gamma e2 in
     if te1.ety_of = te2.ety_of then
       { e with
         exp_of = EBinop(b, te1, te2);
         ety_of = TyBool }
     else raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
       "mismatch of operand types in binary operation '%a'@ at position %a"
       pp_exp e pp_pos e))
  | BEq | BNeq ->
     let te1 = tycheck_exp gamma e1 in
     let te2 = assert_ty gamma e2 te1.ety_of in
     { e with
       exp_of = EBinop(b, te1, te2);
       ety_of = TyBool }

and tycheck_fundef (f : fundef) : fundef =
  (* Define an auxiliary function to bind a list of args to gamma *)
  let rec bind_args gamma args =
    match args with
    | arg :: tl ->
       let ti = arg.tid_of in
       let new_gamma = Symtab.set ti.id_of ti.ty_of gamma in
       bind_args new_gamma tl
    | [] -> gamma
  in
  (* Extend gamma with arg bindings *)
  let gamma = bind_args (Symtab.create()) f.args in
  (* Type-check the function body *)
  let typed_body = tycheck_com_list gamma f.ret_ty f.body in
  (* Make sure all control flow paths reach a return command if the
     return type isn't unit *)
  if f.ret_ty <> TyUnit && not (check_returns f.body) then
    raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
      "not all control paths return a value in function '%a'"
      pp_id f.nm))
  else
    (* Return the typed fundef *)
    { f with body = typed_body }

let add_intrinsics () =
  delta := Symtab.set (Id("create")) ([], TyArray TyT) !delta;
  (* delta := Symtab.set (Id("read")) ([TyString], TyArray TyT) !delta; *)
  delta := Symtab.set (Id("at")) ([TyArray TyT; TyInt], TyT) !delta;
  delta := Symtab.set (Id("set")) ([TyArray TyT; TyInt; TyT], TyUnit) !delta;
  delta := Symtab.set (Id("insert")) ([TyArray TyT; TyInt; TyT], TyUnit) !delta;
  delta := Symtab.set (Id("size")) ([TyArray TyT], TyInt) !delta;
  delta := Symtab.set (Id("swap")) ([TyArray TyT; TyInt; TyInt], TyUnit) !delta;
  delta := Symtab.set (Id("concat")) ([TyArray TyT; TyArray TyT], TyArray TyT)
             !delta;
  delta := Symtab.set (Id("clear")) ([TyArray TyT], TyUnit) !delta;
  delta := Symtab.set (Id("delete")) ([TyArray TyT; TyInt], TyUnit) !delta;
  delta := Symtab.set (Id("subarray")) ([TyArray TyT; TyInt; TyInt], TyArray TyT) !delta;
  ()

(** [tycheck_prog p]:
    Checks that program [p] is well-typed.
    Returns a type-annotated version of [p]. *)

let tycheck_prog (p : prog) : prog =
  reset ();
  add_intrinsics ();
  (* aux function to bind all function ids before typechecking.
     allows for mutual recursion *)
  let rec add_funs fundefs =
    match fundefs with
    | f :: tl ->
      (
        (* throw error if id has existing binding (no shadowing allowed) *)
        match Symtab.get f.nm !delta with
        | Some t -> raise_ty_err (pp_to_string (fun ppf -> fprintf ppf
          "shadowing previous binding of function identifier '%a'" pp_id f.nm))
        | _ -> ()
      );
      (* make list of argument types *)
      let arg_types = BatList.map (fun arg -> arg.tid_of.ty_of) f.args in
      (* add function binding to delta *)
      delta := Symtab.set f.nm (arg_types, f.ret_ty) !delta;
      (* add to poly_functions if f is polymorphic *)
      if tys_contain_TyT (f.ret_ty :: arg_types) then
        poly_functions := Symtab.set f.nm f !poly_functions;
      add_funs tl
    | [] -> ()
  in
  add_funs p.fundefs;
  (* type-check the fundefs *)
  let typed_fundefs = BatList.map tycheck_fundef p.fundefs in
  (* type-check the prog result expression *)
  let typed_main = tycheck_com_list (Symtab.create()) TyT p.main in
  (* return the typed prog *)
  {
    fundefs = typed_fundefs;
    main = typed_main
  }
