open Sexplib.Sexp
open Tree
open BatList
open Lexing

exception Ais_exception of string

(* type lex_pos = *)
(*   { pos_fname : string; *)
(*     pos_lnum : int; *)
(*     pos_bol : int; *)
(*     pos_cnum : int; } *)
(* [@@deriving sexp] *)

(* let lex_pos_of_lexing_position (pos : Lexing.position) : lex_pos = *)
(*   { pos_fname = pos.pos_fname; *)
(*     pos_lnum = pos.pos_lnum; *)
(*     pos_bol = pos.pos_bol; *)
(*     pos_cnum = pos.pos_cnum } *)

type anim_lbl = string
(* [@@deriving sexp] *)

type anim_id = int
(* [@@deriving sexp] *)

type anim_value =
  | AAtom of string
  | AArray of anim_value list
(* [@@deriving sexp] *)

type anim_type =
  | ATVar
  | ATArray
(* [@@deriving sexp] *)

type anim_loc =
  | LVar of anim_id
  | LArrayCell of anim_id * int
(* [@@deriving sexp] *)
            
type anim_instr =
  | ICreate of anim_id * anim_type * anim_lbl
  | IDestroy of anim_id
  | IInsert of anim_id * int * anim_value
  | IDelete of anim_id * int
  | IAssign of anim_loc * anim_value
  | ISwap of anim_id * int * int
  | IClear of anim_id
(* [@@deriving sexp] *)

type anim_com = 
  | CFrameBegin of (anim_lbl * int)
  | CFrameEnd of int
  | CStep of (anim_instr list * int)
(* [@@deriving sexp] *)

type anim_prog =
  AProg of string * anim_com list
(* [@@deriving sexp] *)

let sexp_of_anim_id id =
  Atom (string_of_int id)

let sexp_of_anim_type = function
  | ATVar -> Atom "ATVar" 
  | ATArray -> Atom "ATArray"

let rec sexp_of_anim_value = function
  | AAtom s -> List [Atom "AAtom"; Atom s]
  | AArray vs -> List [Atom "AArray"; List (map sexp_of_anim_value vs)]

let rec sexp_of_anim_loc = function
  | LVar id -> List [Atom "LVar"; sexp_of_anim_id id]
  | LArrayCell (id, i) ->
     List [Atom "LArrayCell"; sexp_of_anim_id id; Atom (string_of_int i)]

let sexp_of_anim_instr = function
  | ICreate (id, ty, lbl) ->
     List [Atom "ICreate"; sexp_of_anim_id id; sexp_of_anim_type ty; Atom lbl]
  | IDestroy id -> List [Atom "IDestroy"; sexp_of_anim_id id]
  | IInsert (id, i, v) ->
     List [Atom "IInsert"; sexp_of_anim_id id; Atom (string_of_int i);
           sexp_of_anim_value v]
  | IDelete (id, i) -> List [Atom "IDelete"; sexp_of_anim_id id;
                             Atom (string_of_int i)]
  | IAssign (loc, v) ->
     List [Atom "IAssign"; sexp_of_anim_loc loc; sexp_of_anim_value v]
  | ISwap (id, i1, i2) ->
     List [Atom "ISwap"; sexp_of_anim_id id; Atom (string_of_int i1);
           Atom (string_of_int i2)]
  | IClear id -> List [Atom "IClear"; sexp_of_anim_id id]

let sexp_of_anim_com = function
  | CFrameBegin (lbl, lnum) -> List [Atom "CFrameBegin";
                                     List [Atom lbl; Atom (string_of_int lnum)]]
  | CFrameEnd lnum -> List [Atom "CFrameEnd"; Atom (string_of_int lnum)]
  | CStep (instrs, lnum) ->
     List [Atom "CStep"; List [List (map sexp_of_anim_instr instrs); Atom (string_of_int lnum)]]

let sexp_of_anim_prog = function
  | AProg (name, coms) ->
     List ([Atom "Aprog"; Atom name; List (map sexp_of_anim_com coms)])

let name_of_anim_prog = function
  | AProg (name, _) -> name

let coms_of_anim_prog = function
  | AProg (_, coms) -> coms

let line_num_of_anim_com = function
  | CFrameBegin (_, lnum) -> lnum 
  | CFrameEnd lnum -> lnum
  | CStep (_, lnum) -> lnum

let rec string_of_anim_value = function
  | AAtom s -> "AAtom(" ^ s ^ ")"
  | AArray vs ->
     "AArray(" ^ String.concat ", "
                   (map string_of_anim_value vs) ^ ")"

let string_of_anim_type = function
  | ATVar -> "ATVar"
  | ATArray -> "ATArray"

let string_of_anim_loc = function
  | LVar id -> "LVar(" ^ string_of_int id ^ ")"
  | LArrayCell (id, i) ->
     "LArrayCell(" ^ string_of_int id ^ ", " ^
       string_of_int i ^ ")"

let string_of_anim_instr = function
  | ICreate (id, t, lbl) ->
     "ICreate(" ^ string_of_int id ^ ", " ^ string_of_anim_type t ^
       ", " ^ lbl ^ ")"
  | IDestroy id -> "IDestroy(" ^ string_of_int id ^ ")"
  | IInsert (id, i, v) ->
     "IInsert(" ^ string_of_int id ^ ", " ^ string_of_int i ^ ", " ^
       string_of_anim_value v ^ ")"
  | IDelete (id, i) ->
     "IDelete(" ^ string_of_int id ^ ", " ^
       string_of_int i ^ ")"
  | IAssign (loc, v) ->
     "IAssign(" ^ string_of_anim_loc loc ^ ", " ^
       string_of_anim_value v ^ ")"
  | ISwap (id, i1, i2) ->
     "ISwap(" ^ string_of_int id ^ ", " ^ string_of_int i1 ^ ", " ^
       string_of_int i2 ^ ")"
  | IClear id -> "IClear(" ^ string_of_int id ^ ")"

let string_of_anim_com = function
  | CFrameBegin (lbl, lnum) -> "CFrameBegin(" ^ (string_of_int lnum) ^ ", " ^
                                 lbl ^ ")"
  | CFrameEnd lnum -> "CFrameEnd(" ^ string_of_int lnum ^ ")"
  | CStep (instrs, lnum) ->
     "CStep(" ^ (string_of_int lnum) ^ ", " ^
       String.concat ", " (map string_of_anim_instr instrs) ^ ")"

let string_of_anim_prog (aprog : anim_prog)  =
  match aprog with
  | AProg (name, coms) -> name ^ ": " ^
     "[" ^ String.concat ",\n" (map string_of_anim_com coms) ^ "]"

let write_sexp_anim_prog_to_file
      (filename : string) (aprog : anim_prog) =
  let outchan = open_out filename in
  Sexplib.Sexp.output_mach outchan (sexp_of_anim_prog aprog)

(* let read_sexp_anim_prog_from_file (filename : string) = *)
(*   (anim_prog_of_sexp (Sexplib.Sexp.load_sexp filename)) *)

let write_marshaled_anim_prog_to_file
      (filename : string) (aprog : anim_prog) =
  let outchan = open_out filename in
  Marshal.to_channel outchan aprog []

let read_marshaled_anim_prog_from_file (filename : string) =
  let inchan = open_in filename in
  Marshal.from_channel inchan

let tree_of_anim_lbl l =
  Leaf ("Lbl(" ^ l ^ ")")

let tree_of_anim_id i =
  Leaf ("Id(" ^ string_of_int i ^ ")")

let rec tree_of_anim_value = function
  | AAtom s -> Leaf ("Val(" ^ s ^ ")")
  | AArray av_list -> Node("AArray", map tree_of_anim_value av_list)

let tree_of_anim_type = function
  | ATVar -> Leaf "OVar"
  | ATArray -> Leaf "OArray"

let tree_of_anim_loc = function
  | LVar id -> Node ("LVar", [tree_of_anim_id id])
  | LArrayCell (id, i) -> Node ("LArrayCell " ^ string_of_int i,
                                [tree_of_anim_id id])

let tree_of_anim_instr = function
  | ICreate (id, t, lbl) ->
     Node ("ICreate", [tree_of_anim_id id; tree_of_anim_type t;
                       tree_of_anim_lbl lbl])
  | IDestroy id ->
     Node ("IDestroy", [tree_of_anim_id id])
  | IInsert (id, i, v) ->
     Node ("IInsert " ^ string_of_int i,
           [tree_of_anim_id id; tree_of_anim_value v])
  | IDelete (id, i) ->
     Node ("IDelete " ^ string_of_int i, [tree_of_anim_id id])
  | IAssign (loc, v) ->
     Node ("IAssign", [tree_of_anim_loc loc; tree_of_anim_value v])
  | ISwap (id, i1, i2) ->
     Node ("ISwap " ^ string_of_int i1 ^ ", " ^ string_of_int i2,
           [tree_of_anim_id id])
  | IClear id ->
     Node ("IClear", [tree_of_anim_id id])

let tree_of_anim_com = function
  | CFrameBegin (lbl, _) -> Leaf ("CFrameBegin " ^ lbl)
  | CFrameEnd _ -> Leaf "CFrameEnd"
  | CStep (ai_list, _) ->
     Node ("CStep", map tree_of_anim_instr ai_list)
  
let tree_of_anim_prog (p : anim_prog) : string tree =
  match p with
  | AProg (name, coms) ->
     Node (name, map tree_of_anim_com coms)

let assert_atom_anim_value = function
  | AAtom s -> s
  | _ -> raise (Ais_exception "expected AAtom value")

let assert_array_anim_value = function
  | AArray v_list -> v_list
  | _ -> raise (Ais_exception "expected AArray value")
