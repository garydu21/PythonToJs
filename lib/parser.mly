%{
open Lang

(* Names such as int can be type names but also by function names (cast to int). 
   Therefore, the lexer cannot identify them as type names. 
 *)
let id_to_tp = function
   | "bool"   -> BoolT
   | "int"    -> IntT
   | "float"  -> FloatT   
   | "None"   -> NoneT
   | "str"    -> StringT
   | t -> failwith ("invalid type name " ^ t)


(* Separating a list of Left / Right tagged elements into two lists (left and right) *)
let add_left (ls, rs) le = (ls@[le], rs)
let add_right (ls, rs) re = (ls, rs@[re])

let sep_left_right lrs =
  List.fold_left (fun p -> (Either.fold ~left:(add_left p) ~right:(add_right p))) ([],[]) lrs
%}

%token <string> IDENTIFIER
%token <bool> BCONSTANT
%token <float> FLOATCONSTANT
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN 
%token EQ COMMA COLON VBAR
%token DEF IF ELSE WHILE RETURN BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token ARROW
%token BEGIN END

%token EOF

%start main
%type <Lang.prog> main

%%

main: p = prog; EOF { p }
;

/* TODO: add function definitions */
prog: svs = list(statement_or_vardecl) 
     { let (vds, ss) = sep_left_right svs in Prog([], vds, Block ss) }
;

statement_or_vardecl : 
|  s = vardecl   { Either.Left s }
|  s = statement { Either.Right s}
;

/* basic type expressions, as in: x : int */
tpexpr_base:
  i = IDENTIFIER { id_to_tp i }
;

/* TODO: add complex type expressions, as in: x : int | str */
vardecl: i = IDENTIFIER; COLON; t= tpexpr_base { Vardecl(i, mk_norm_tp [t]) }
;

/* *******  EXPRESSIONS  ******* */

primary:
| a =atom { a }
;

  
atom: 
  v = IDENTIFIER      { VarE(v) }
| bc = BCONSTANT      { Const(BoolV bc) }
| fc = FLOATCONSTANT  { Const(FloatV fc) }
| ic = INTCONSTANT    { Const(IntV ic) }
| c = STRINGCONSTANT  { Const(StringV(c)) }
| LPAREN e = expression RPAREN
    { e }
;
  
expression:
  | mult = mult_expr { mult }
  | add = add_expr { add }
  | primary { $1 }
    /* OMITTED: expression , assignment_expression */

;

add_expr:
  | e1 = add_expr PLUS e2 = primary { BinOp (BArith BAadd,e1,e2) }
  | primary { $1 }
; 

mult_expr:
  | e1 = mult_expr PLUS e2 = primary { BinOp (BArith BAmul,e1,e2) }
  | primary { $1 }

/* *******  STATEMENTS  ******* */

/* TODO: Most statement need to be defined */
statement: 
| s = simple_stmt { s }
| c = compound_stmt { c }
;

block:
| BEGIN bloc = list(statement) END { Block bloc }
;

/* Regle composée */
compound_stmt: 
  | w = while_stmt { w }
  | b = block { b }
  | i = if_stmt { i }
  

/*WHILE réfère au token WHILE dans lang.ml */

while_stmt:
  | WHILE e = expression COLON b = block
      { While (e, b) }
;

if_stmt:
  | IF e = expression COLON b1 = block ELSE COLON b2 = block
    { Cond (e, b1, b2) }
  | IF e = expression COLON b = block
    { Cond (e, b, Block []) }
;


/* TODO: also consider return and call */
simple_stmt:
  | s = assignment { s }
  | r = return_stmt { r }
;

assignment: vn = IDENTIFIER; EQ; e = expression  { Assign(vn, e) }
;

return_stmt: RETURN e = expression { Return(e) }
;