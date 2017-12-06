open Batteries
open BatFormat
open BatOptParse
open Sexplib.Std

open Lexing
open AST
open Exp       
open Tycheck
open Interptypes
open Interp
open Tree
open Aaltree
open Ais
			    
let print_pos outchan lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outchan "%s:%d:%d" pos.pos_fname 
	  pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)  
		    
let parse_with_err lexbuf =
  try Aalparser.prog Aallexer.token lexbuf with
  | Aalparser.Error -> 
    eprintf "%a: syntax error\n" print_pos lexbuf;
    exit (-1)

  | Aallexer.Syntax_err msg -> 
    eprintf "%a: %s\n" print_pos lexbuf msg;
    exit (-1)

let () =
  let opt_parser = OptParser.make () in
  let infile_opt = StdOpt.str_option ~metavar:"filename" () in  
  let dump_tycheck_opt = StdOpt.store_true () in
  let dump_marshal_opt = StdOpt.store_true () in
  OptParser.add opt_parser ~short_name:'i' ~long_name:"infile" infile_opt;
  OptParser.add opt_parser ~long_name:"dump-tycheck" dump_tycheck_opt;
  OptParser.add opt_parser ~long_name:"dump-marshal" dump_marshal_opt;
  let _ = OptParser.parse_argv opt_parser in
  try
    let infile =
      try Opt.get infile_opt
      with Opt.No_value ->
	   OptParser.usage opt_parser ();
	   exit (-1)
    in
    let name = fst (String.split infile ".") in
    let outfile = infile ^ ".result" in
    let tycheckfile = infile ^ ".tychecked" in
    let sexpfile = infile ^ ".sexp" in
    let treefile = infile ^ ".tree" in
    let aisfile = infile ^ ".ais" in
    let inbuf  = open_in infile in
    let outbuf = open_out outfile in
    let outbuf_ppf = formatter_of_out_channel outbuf in
    let lexbuf = Lexing.from_channel inbuf in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = infile };
    let p = parse_with_err lexbuf in
    (* print_endline "a"; *)
    let p_tychecked =
      try tycheck_prog p
      with Ty_error err ->
	eprintf "Type error: %s\n" err;
	exit (-1)
    in

    (* print prog tree *)
    write_tree_to_file treefile (tree_of_prog p_tychecked);
    
    (* conditionally dump tychecked *)
    if Opt.get dump_tycheck_opt
    then let tycheckbuf = open_out tycheckfile in
	 let tycheckbuf_ppf = formatter_of_out_channel tycheckbuf in
	 pp_prog tycheckbuf_ppf pp_com_list pp_ty pp_nl p_tychecked
    else ();

    let (result, aprog') = interp_prog p_tychecked in
    let aprog = AProg (name, coms_of_anim_prog aprog') in
    print_endline (string_of_value result);
    fprintf outbuf_ppf "%s\n" (string_of_value result);

    (* write AIS tree *)
    (* let aprog_tree = tree_of_anim_prog aprog in *)
    (* write_tree_to_file treefile aprog_tree; *)

    (* conditionally dump marshaled AIS program *)
    if Opt.get dump_marshal_opt
    then
      write_marshaled_anim_prog_to_file aisfile aprog;

    (* write AIS program sexp *)
    (* let sexpbuf = open_out sexpfile in *)
    (* let sexpbuf_ppf = formatter_of_out_channel sexpbuf in *)
    (* fprintf sexpbuf_ppf "%s\n" *)
    (*   (Sexplib.Sexp.to_string (sexp_of_anim_prog aprog)); *)
    write_sexp_anim_prog_to_file sexpfile aprog
   
  with
  | Sys_error err -> eprintf "System error: %s\n" err
  | Failure err -> eprintf "Error: %s\n" err
  | Internal_error err -> eprintf "Internal error: %s\n" err
