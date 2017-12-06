 {
    open Lexing
    open Aalparser
    open Batteries
    open Exp

    exception Eof
    exception Syntax_err of string
}

let newline = '\r' | '\n' | "\r\n"
let character = ['0'-'9'] | ['a'-'z'] | ['A'-'Z']

rule token = parse
| [' ' '\t'] { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| "//" { comment lexbuf }
| "/*" { nested_comment 1 lexbuf }
| ['0'-'9']+ as lxm { INTCONST(Int32.of_string lxm) }
(* | ['0'-'9']+'.'['0'-'9']* as lxm { FLOATCONST(Float.of_string lxm) } *)
| "true"|"false" as lxm { BOOLCONST(Bool.of_string lxm) }
| ['"']character*['"'] as lxm { STRINGCONST(String.sub lxm 1 (String.length lxm - 2)) }
| "array" { ARRAY }
| "def" { DEF }
| "while" { WHILE }
| "if" { IF }
| "else" { ELSE }
| "int" { INT }
| "string" { STRING }
| "bool" { BOOL }
| "unit" { UNIT }
| "tt" { TT }
| 'T' { T }
| "return" { RETURN }
| "!=" { NEQ }
| "!" { NOT }
| ['a'-'z''A'-'Z']+['_''a'-'z''A'-'Z''0'-'9']* as lxm { ID(lxm) }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| "&&" { AND }
| "||" { OR }
| '<' { LT }
| '>' { GT }
| "<=" { LE }
| ">=" { GE }
| "==" { EQ }
| "[" { LSQUARE }
| "]" { RSQUARE }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMI }
| '=' { ASSIGN }
| ':' { COLON }
| ',' { COMMA }
| '&' { AMP }
| '@' { AT }
| eof { EOF }
| _ { raise (Syntax_err ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
| newline { new_line lexbuf; token lexbuf }  
| _ { comment lexbuf }  

and nested_comment level = parse
| "/*" { nested_comment (level+1) lexbuf }
| "*/" { if level = 1 then token lexbuf else nested_comment (level-1) lexbuf }
| newline { new_line lexbuf; nested_comment level lexbuf }
| _ { nested_comment level lexbuf }
