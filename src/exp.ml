open Batteries
open BatFormat
open BatList

open Lexing
open AST

type raw_exp =
  | EInt of int32
  | EString of string
  | EId of id
  | ECall of id * exp list * ty
  | EUnop of unop * exp
  | EBinop of binop * exp * exp
  | EUnit
  | ETrue
  | EFalse
  | EArray of exp list

and exp = 
  { start_of : Lexing.position;
    end_of : Lexing.position;
    exp_of : raw_exp;
    ety_of : ty }

type raw_com =
  | CCall of id * (exp list) * ty
  | CIf of exp * (com list) * (com list)
  | CAss of id * exp
  | CWhile of exp * (com list)
  | CReturn of exp

and com =
  { start_of : Lexing.position;
    end_of : Lexing.position;
    com_of : raw_com }

type fundef =
  { nm  : id;
    args : argid list;
    ret_ty : ty;
    body : com list;
    start_of : Lexing.position;
    end_of : Lexing.position }

type prog =
  { fundefs : fundef list;
    main  : com list }

let rec ty_eq (t1 : ty) (t2 : ty) : bool =
  match t1, t2 with
  | TyInt, TyInt -> true
  | TyString, TyString -> true
  | TyBool, TyBool -> true
  | TyArray t1, TyArray t2 -> ty_eq t1 t2
  | TyUnit, TyUnit -> true
  | _, _ -> false

(* let exp_seq (e1 : exp) (e2 : exp) : raw_exp = *)
(*   match e1.exp_of, e2.exp_of with *)
(*   | ESeq el1, ESeq el2 -> ESeq (el1 @ el2) *)
(*   | _, ESeq el2 -> ESeq (e1 :: el2) *)
(*   | ESeq el1, _ -> ESeq (el1 @ [e2]) *)
(*   | _, _ -> ESeq (e1 :: e2 :: []) *)

(* let subst_var_com (new_id : id) (old_id : id) (c : 'a com) : 'a com = *)
(*   let rec go_raw c0 = *)
(*     match c0 with *)
(*     | _ -> c0 *)
(*   and go c0 = *)
(*     { c0 with com_of = go_raw c0.com_of } *)
(*   in *)
(*   go c *)

let subst_var_exp (new_id : id) (old_id : id) (e : exp) : exp =
  let rec go_raw e0 =
    match e0 with
    | EInt _ -> e0
    | EString _ -> e0
    | EId x -> if x = old_id then EId new_id else e0
    | ECall (f, el, t) -> ECall (f, BatList.map go el, t)
    | EUnop (u, e1) -> EUnop (u, go e1)
    | EBinop (b, e1, e2) -> EBinop (b, go e1, go e2)
    (* | EScope e1 -> EScope (go e1) *)
    | EUnit -> e0
    | ETrue -> e0
    | EFalse -> e0
    | EArray el -> EArray (BatList.map go el)
    (* | ECreate t -> e0 *)
    (* (\* | EAccess (i, e1) -> EAccess ((if i = old_id then new_id else i), go e1) *\) *)
    (* | ESeq el -> ESeq (BatList.map go el) *)
    (* | EIf (e1, e2, e3) -> EIf (go e1, go e2, go e3) *)
    (* | EAss (i, e1) -> EAss ((if i = old_id then new_id else i), go e1) *)
    (* | EWhile (e1, e2) -> EWhile (go e1, go e2) *)
    
  and go e0 =
    { e0 with exp_of = go_raw e0.exp_of }
  in go e

let rec subst_var_com (new_id : id) (old_id : id) (c : com) : com =
  let rec go_raw c0 =
    match c0 with
    | CCall (id, e_lst, t) ->
       CCall (id, map (subst_var_exp new_id old_id) e_lst, t)
    | CIf (e1, blk1, blk2) ->
       CIf (subst_var_exp new_id old_id e1,
         map (subst_var_com new_id old_id) blk1,
         map (subst_var_com new_id old_id) blk2)
    | CAss (id, e1) -> CAss (id, subst_var_exp new_id old_id e1)
    | CWhile (e1, blk) ->
       CWhile (subst_var_exp new_id old_id e1,
         map (subst_var_com new_id old_id) blk)
    | CReturn e1 -> CReturn (subst_var_exp new_id old_id e1)
  
    and go c0 =
      { c0 with com_of = go_raw c0.com_of }
  in go c

let pp_tabbed ppf pp v =
  open_tbox ();
  print_tab ();
  pp ppf v;
  close_tbox ()

let rec pp_com ppf (c : com) : unit =
  fprintf ppf "%a" pp_raw_com c.com_of

and pp_raw_com ppf (c : raw_com) :unit =
  fprintf ppf "%s" "pp_raw_com"

let pp_com_list ppf (c_lst : com list) : unit =
  BatList.iter (pp_com ppf) c_lst

let rec pp_exp ppf (e : exp) : unit = 
  fprintf ppf "%a" pp_raw_exp e.exp_of

and pp_raw_exp ppf (e : raw_exp) : unit =
  match e with
  | EInt n -> pp_int ppf (Int32.to_int n)
  | EString s -> pp_stringconst ppf s
  | EId x -> fprintf ppf "%a" pp_id x 
  (* | ESeq el -> pp_list ppf pp_exp pp_semi el *)
  (* | ECall(x, el) -> fprintf ppf "%a(%a)" pp_id x *)
  (*       		    (fun ppf0 -> pp_list ppf0 pp_exp pp_comma) el *)
  | EUnop(u, e2) -> fprintf ppf "%a %a" pp_unop u pp_exp e2
  | EBinop(b, e1, e2) -> fprintf ppf "(%a %a %a)" pp_exp e1 pp_binop b pp_exp e2
  (* | EIf(e1, e2, e3) -> *)
  (*    fprintf ppf "(if %a then@ " pp_exp e1; pp_tabbed ppf pp_exp e2; *)
  (*    fprintf ppf "@ else@ "; pp_tabbed ppf pp_exp e3; *)
  (*    fprintf ppf ")" *)
  (* | EScope(e2) -> *)
  (*    fprintf ppf "{@ %a@ }" pp_exp e2 *)
  | EUnit -> fprintf ppf "tt"
  | ETrue -> fprintf ppf "true"
  | EFalse -> fprintf ppf "false"
  | EArray el -> fprintf ppf "EArray"
  (* | ECreate t -> fprintf ppf "create %a" pp_ty t *)
  (* | EAccess (i, e1) -> fprintf ppf "EAccess" *)
  (* | EWhile (e1, e2) -> *)
  (*    fprintf ppf "while %a do@ @[<1>%a@]" *)
  (*            pp_exp e1 pp_exp e2 *)
  (* | EAss (i, e1) -> fprintf ppf "EAss" *)
  | _ -> fprintf ppf "stuff"

let rec pp_texp ppf (e : exp) : unit = 
  fprintf ppf "(%a : %a)" pp_raw_texp e.exp_of pp_ty e.ety_of

and pp_raw_texp ppf (e : raw_exp) : unit =
  match e with
  | EInt n -> pp_int ppf (Int32.to_int n)
  | EString s -> pp_stringconst ppf s
  | EId x -> fprintf ppf "%a" pp_id x 
  (* | ESeq el -> pp_list ppf pp_texp pp_semi el *)
  (* | ECall(x, el) -> fprintf ppf "%a(%a)" pp_id x *)
  (*       		    (fun ppf0 -> pp_list ppf0 pp_texp pp_comma) el *)
  | EUnop(u, e2) -> fprintf ppf "%a %a" pp_unop u pp_texp e2
  | EBinop(b, e1, e2) -> fprintf ppf "(%a %a %a)" pp_texp e1 pp_binop b pp_texp e2
  (* | EIf(e1, e2, e3) -> *)
  (*    fprintf ppf "(if %a then@ " pp_texp e1; pp_tabbed ppf pp_texp e2; *)
  (*    fprintf ppf "@ else@ "; pp_tabbed ppf pp_texp e3; *)
  (*    fprintf ppf ")" *)
  (* | EScope(e2) -> *)
  (*    fprintf ppf "{@ %a@ }" pp_texp e2 *)
  | EUnit -> fprintf ppf "tt"
  | ETrue -> fprintf ppf "true"
  | EFalse -> fprintf ppf "false"
  | EArray el -> fprintf ppf "EArray"
  (* | ECreate t -> fprintf ppf "create %a" pp_ty t *)
  (* | EAccess (i, e1) -> fprintf ppf "EAccess" *)
  (* | EWhile(e1, e2) -> *)
  (*    fprintf ppf "while %a do@ @[<1>%a@]" *)
  (*            pp_texp e1 pp_texp e2 *)
  (* | EAss (i, e1) -> fprintf ppf "EAss" *)
  | _ -> fprintf ppf "stuff"

let pp_pos_com ppf (c : com) : unit =
  let start_pos = c.start_of in
  let end_pos = c.end_of in
  fprintf ppf "[%d:%d-%d:%d]"
	  start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	  end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)	  

let pp_pos ppf (e : exp) : unit =
  let start_pos = e.start_of in
  let end_pos = e.end_of in
  fprintf ppf "[%d:%d-%d:%d]"
	  start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	  end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
	  
let pp_fundef ppf pp ppty f : unit =
  fprintf ppf "@[<1>def %a(%a)@ :@ %a {@\n@[%a@]@]@\n}@,"
	  pp_id f.nm
	  (fun ppf0 ->
	   pp_list ppf0 (fun ppf1 x ->
               let ti = x.tid_of in
	       fprintf ppf1 "%a : %a" pp_id ti.id_of ppty ti.ty_of) pp_comma)
          f.args ppty f.ret_ty pp f.body

let pp_prog ppf pp ppty sep p =
  fprintf ppf "@[<0>%a%a@[%a@]@]@\n@."
	  (fun ppf0 -> BatList.iter (pp_fundef ppf0 pp ppty)) p.fundefs
	  sep ()
	  pp p.main
