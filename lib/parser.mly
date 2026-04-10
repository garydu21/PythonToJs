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


type top =
  | TopFun of fundefn
  | TopVar of vardecl
  | TopStmt of stmt

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
prog:
  svs = list(statement_or_vardecl_or_fun) {
  let (funcs, vds, ss) =
    List.fold_left (fun (fs, vs, st) x ->
      match x with
      | TopFun f  -> (f :: fs, vs, st)
      | TopVar v  -> (fs, v :: vs, st)
      | TopStmt s -> (fs, vs, s :: st)
    ) ([], [], []) svs
  in
  Prog(List.rev funcs, List.rev vds, Block(List.rev ss))
}
;

statement_or_vardecl_or_fun : 
|  v = vardecl   { TopVar v }
|  s = statement { TopStmt s}
|  f = func_def { TopFun f}
;

/* basic type expressions, as in: x : int */
tpexpr_base:
  i = IDENTIFIER { id_to_tp i }
;

/* TODO: add complex type expressions, as in: x : int | str */
vardecl: i = IDENTIFIER; COLON; t= tpexpr_base { Vardecl(i, mk_norm_tp [t]) }
;

func_def:
  | DEF vn = IDENTIFIER LPAREN params = separated_list(COMMA, vardecl) RPAREN ARROW t = tpexpr_base COLON b = block
    { Fundefn(Fundecl(vn, params, mk_norm_tp [t]), [], b)}
;

/* *******  EXPRESSIONS  ******* */

primary:
  | a = atom { a }
;

  
atom: 
  | f = IDENTIFIER LPAREN a = argument RPAREN { CallE(f, a) }
  | v = IDENTIFIER      { VarE(v) }
  | bc = BCONSTANT      { Const(BoolV bc) }
  | fc = FLOATCONSTANT  { Const(FloatV fc) }
  | ic = INTCONSTANT    { Const(IntV ic) }
  | c = STRINGCONSTANT  { Const(StringV(c)) }
  | LPAREN e = expression RPAREN { e }
;

argument: sl = separated_list(COMMA, expression) { sl }

expression:
  | or_e = or_expr { or_e }
    /* OMITTED: expression , assignment_expression */
;

or_expr:
  | and_e = and_expr { and_e }
  | o1 = or_expr BLOR o2 = and_expr { BinOp (BBool BBor,o1,o2) }

and_expr:
  | comp = compar_expr { comp }
  | a1 = and_expr BLAND a2 = compar_expr { BinOp (BBool BBand,a1,a2) }
;

compar_expr:
  | add = add_expr { add }
  | a1 = add_expr BCEQ a2 = add_expr { BinOp (BCompar BCeq,a1,a2)} 
  | a1 = add_expr BCNE a2 = add_expr { BinOp (BCompar BCne,a1,a2)} 
  | a1 = add_expr BCGT a2 = add_expr { BinOp (BCompar BCgt,a1,a2)} 
  | a1 = add_expr BCGE a2 = add_expr { BinOp (BCompar BCge,a1,a2)} 
  | a1 = add_expr BCLT a2 = add_expr { BinOp (BCompar BClt,a1,a2)} 
  | a1 = add_expr BCLE a2 = add_expr { BinOp (BCompar BCle,a1,a2)} 
;


add_expr:
  | e1 = add_expr PLUS e2 = mult_expr { BinOp (BArith BAadd,e1,e2) }
  | e1 = add_expr MINUS e2 = mult_expr { BinOp (BArith BAsub,e1,e2) }
  | mult = mult_expr { mult }
; 

mult_expr:
  | e1 = mult_expr TIMES e2 = primary { BinOp (BArith BAmul,e1,e2) }
  | e1 = mult_expr DIV e2 = primary { BinOp (BArith BAdiv,e1,e2) }
  | e1 = mult_expr MOD e2 = primary { BinOp (BArith BAmod,e1,e2) }
  | primary { $1 }
;
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
  | cs = callS_stmt { cs } 
;

assignment: vn = IDENTIFIER; EQ; e = expression  { Assign(vn, e) }
;

return_stmt: RETURN e = expression { Return(e) }
;

callS_stmt: p = IDENTIFIER LPAREN a = argument RPAREN { CallS(p,a) }
;
