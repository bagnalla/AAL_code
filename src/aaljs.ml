(* open Core.Std *)
(* open Sexplib.Conv *)
open Lexing
open Aalparser
open Tycheck
open Ais
open Interp
open Interptypes
open Sexplib.Sexp

(* Boilerplate code for calling OCaml in the worker thread. *)
let js_object = Js.Unsafe.variable "Object"
let js_handler = jsnew js_object ()
let postMessage = Js.Unsafe.variable "postMessage"

let log s = ignore (Js.Unsafe.call postMessage (Js.Unsafe.variable "self")
		      [|Js.Unsafe.inject (Js.string s)|])

let onmessage event =
  let fname = event##data##fname in
  let args = event##data##args in
  let handle = Js.Unsafe.get js_handler fname in
  let result = Js.Unsafe.fun_call handle (Js.to_array args) in
  let response = jsnew js_object () in
  Js.Unsafe.set response (Js.string "fname") fname;
  Js.Unsafe.set response (Js.string "result") result;
  Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject response|]

let _ = Js.Unsafe.set (Js.Unsafe.variable "self") (Js.string "onmessage") onmessage

(* The NNF conversion and registration in JS. *)
(* let formula_of_string s = Parser.parse_formula Lexer.lex (Lexing.from_string s) *)
(* let js_nnf s = *)
(*   log ("computing nnf of " ^ (Js.to_string s)); *)
(*   Js.string (str (nnf (formula_of_string (Js.to_string s)))) *)

(* let _ = Js.Unsafe.set js_handler (Js.string "nnf") (Js.wrap_callback js_nnf) *)

type aalresponse =
  | ErrorResponse of string
  | SuccessResponse of (string * anim_prog)
(* [@@deriving sexp] *)

let sexp_of_aalresponse = function
  | ErrorResponse s -> List [Atom "ErrorResponse"; Atom s]
  | SuccessResponse (s, aprog) ->
     List [Atom "SuccessResponse"; List [Atom s; sexp_of_anim_prog aprog]]

let print_pos lexbuf =
  let pos = lexbuf.lex_curr_p in
  pos.pos_fname ^ " line " ^ (string_of_int pos.pos_lnum) ^
    " col " ^ (string_of_int (pos.pos_cnum - pos.pos_bol + 1))

(* let remove_white_space s = *)
  

let js_compile s =
  let body = Js.to_string s in
  if String.trim body = "" then (* if body = "" then *)
    Js.string (to_string_mach
                    (sexp_of_aalresponse
                       (ErrorResponse "Error: empty program")))
  else
    let lexbuf = Lexing.from_string body in
    try
      let prog = Aalparser.prog Aallexer.token lexbuf in
      let tychecked = tycheck_prog prog in
      let (result, aprog') = interp_prog tychecked in
      let aprog = AProg ("", coms_of_anim_prog aprog') in
      Js.string
        (to_string_mach
           (sexp_of_aalresponse (SuccessResponse
                                   (string_of_value result, aprog))))
    with
    | Aalparser.Error ->
       Js.string (to_string_mach
                    (sexp_of_aalresponse
                       (ErrorResponse ("Syntax error: " ^ (print_pos lexbuf)))))
    | Aallexer.Syntax_err msg ->
        Js.string (to_string_mach
                     (sexp_of_aalresponse
                        (ErrorResponse
                           ("Lexer error: " ^ (print_pos lexbuf)
                            ^ "<br>" ^  msg))))
    | Ty_error msg ->
       Js.string (to_string_mach
                    (sexp_of_aalresponse
                       (ErrorResponse ("Type error: " ^ msg))))

let _ = Js.Unsafe.set js_handler (Js.string "compile")
          (Js.wrap_callback js_compile)
