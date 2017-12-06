%{
    open AST
    open Exp
%}

(***********************************************************************
* TOKENS
***********************************************************************)

%token <int32> INTCONST
%token <bool> BOOLCONST
%token <string> STRINGCONST
%token <string> ID

%token ARRAY

%token DEF WHILE IF ELSE INT BOOL STRING UNIT TT RETURN T

%token NOT AMP
%token PLUS MINUS TIMES DIV
%token AND OR LT GT LE GE EQ NEQ ASSIGN

%token LSQUARE RSQUARE LPAREN RPAREN LBRACE RBRACE SEMI COLON COMMA EOF

%token AT

(***********************************************************************
* PRIORITY/ASSOCIATIVITY DECLARATIONS
***********************************************************************)

(* %nonassoc it *)
(* %left ite *)
(* %left SEMI *)
(* %nonassoc ASSIGN *)
(* %nonassoc call *)
(* %left ite *)
(* %nonassoc ELSE *)
%left OR
%left AND
%nonassoc LT GT LE GE EQ NEQ
%left PLUS MINUS
%left TIMES DIV
%nonassoc unary_over_binary

%start <Exp.prog> prog

%%

(***********************************************************************
* PRODUCTION RULES (modeled after Grumpy language specification)
***********************************************************************)

(* function argument *)
arg:
| arg_id = id COLON arg_ty = ty AMP { mk_argid (mk_tid arg_id arg_ty) true }
| arg_id = id COLON arg_ty = ty { mk_argid (mk_tid arg_id arg_ty) false }

(* AST.unop *)
%inline unop:
| MINUS { UMinus }
| NOT { UNot }

(* AST.binop *)
%inline binop:
| PLUS { BPlus }
| MINUS { BMinus }
| TIMES { BTimes }
| DIV { BDiv }
| AND { BAnd }
| OR { BOr }
| LT { BLt }
| GT { BGt }
| LE { BLe }
| GE { BGe }
| EQ { BEq }
| NEQ { BNeq }

(* AST.id *)
id:
| i = ID { Id(i) }

(* int const *)
num_int:
| n = INTCONST { EInt(n) }

(* string const *)
string_const:
| s = STRINGCONST { EString(s) }

(* bool const *)
bool_const:
| b = BOOLCONST { if b then ETrue else EFalse }

(* AST.ty *)
ty:
| LPAREN t = ty RPAREN { t }
| INT { TyInt }
| STRING { TyString }
| BOOL { TyBool}
| t = ty ARRAY { TyArray(t) }
| UNIT { TyUnit }
| T { TyT }

ty_param:
| AT t = ty { t }

block:
| LBRACE c = com* RBRACE { c }

els:
| ELSE c = block { c }

(* exp.exp *)
exp:
e = raw_exp {{ start_of = $startpos;
               end_of = $endpos;
               exp_of = e;
               ety_of = TyUnit; }}

(* exp.raw_exp *)
raw_exp:
(* parentheses *)
| LPAREN e = raw_exp RPAREN { e }
(* scope *)
(* | LBRACE e = exp RBRACE { EScope(e) } *)
(* id *)
| i = id { EId(i) }
(* int const *)
| n = num_int { n }
(* string const *)
| s = string_const { s }
(* bool const *)
| b = bool_const { b }
(* function call *)
| i = id t = ty_param? LPAREN args = separated_list(COMMA, exp) RPAREN
    { let t' = match t with Some t' -> t' | None -> TyT in
      ECall(i, args, t') }
(* unop *)
| op = unop e = exp %prec unary_over_binary { EUnop(op, e) }
(* binop *)
| e1 = exp op = binop e2 = exp { EBinop(op, e1, e2) }
(* unit *)
| TT { EUnit }
(* Array access by index *)
(* | i = ID LSQUARE e = exp RSQUARE { EAccess(Id(i), e) } *)
| i = id LSQUARE e = exp RSQUARE {
                         ECall(Id("at"),
                               [{ start_of = $startpos;
                                  end_of = $endpos;
                                  exp_of = EId(i);
                                  ety_of = TyUnit; (* not typed yet *)
                                }; e], TyT) }
| LSQUARE vals = separated_list(COMMA, exp) RSQUARE { EArray(vals) }


(* exp.exp *)
com:
c = raw_com {{ start_of = $startpos;
               end_of = $endpos;
               com_of = c; }}

raw_com:
| i = id t = ty_param? LPAREN args = separated_list(COMMA, exp) RPAREN SEMI
    { let t' = match t with Some t' -> t' | None -> TyT in CCall(i, args, t') }
(* | IF e = exp c1 = block ELSE c2 = block *)
(*     { CIf (e, c1, c2) } *)
| IF e = exp c1 = block c2 = els?
    { let c2' = match c2 with Some c2' -> c2' | None -> [] in CIf (e, c1, c2') }
(* | IF e = exp LBRACE c1 = com* RBRACE ELSE LBRACE c2 = com* RBRACE *)
(*     { CIf (e, c1, c2) } *)
(* | IF e = exp c1 = com ELSE LBRACE c2 = com* RBRACE { CIf (e, [c1], c2) } *)
(* | IF e = exp LBRACE c1 = com* RBRACE ELSE c2 = com { CIf (e, c1, [c2]) } *)
(* | IF e = exp LBRACE c1 = com* RBRACE { CIf (e, c1, []) } *)
(* | IF e = exp c1 = com { CIf (e, [c1], []) } *)
| i = id LSQUARE e1 = exp RSQUARE ASSIGN e2 = exp SEMI
  {
    CCall(Id("set"),
          [{ start_of = $startpos;
             end_of = $endpos;
             exp_of = EId(i);
             ety_of = TyUnit; (* not typed yet *)
           }; e1; e2], TyT) }
| i = id ASSIGN e = exp SEMI { CAss(i, e) }
| WHILE e = exp LBRACE c = com* RBRACE { CWhile (e, c) }
| RETURN e = exp SEMI { CReturn e }
| RETURN SEMI+ { CReturn { exp_of = EUnit;
  	      	   	   ety_of = TyUnit;
 			  start_of = $startpos;
			  end_of = $endpos } }

(* AST.fundef *)
fundef:
| DEF i = id LPAREN arguments = separated_list(COMMA, arg) RPAREN
  COLON t = ty LBRACE c = com* RBRACE
{{ nm = i;
   args = arguments;
   ret_ty = t;
   body = c;
   start_of = $startpos;
   end_of = $endpos }}

(* AST.prog *)
prog:
| l = fundef* c = com* EOF {{ fundefs = l;
                              main = c; }}
