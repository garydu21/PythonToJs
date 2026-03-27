{
  open Lexing
  open Parser
  exception Lexerror

  (* for cutting off the trailing quotes of strings *)
  let clean_string s = String.sub s 1 (String.length s - 2) ;;


  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}


let comment = '#' [^'\n']*
let alphus =           ['a'-'z''A'-'Z''_']
let num  =           ['0'-'9'] 
let decimal	=	'0'|(['1'-'9']['0'-'9']*)
let stringchar = [^'"''\\''\n']
let string = '"' stringchar* '"'  

rule token = parse

(* --  Rules for dealing with whitespace or comments  -- *)

| [' ' '\t']
    { token lexbuf }    (* white space: recursive call of lexer *)
| '\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)

(* Fake comments as begin / end markers *)
| "#begin"  [^'\n']* '\n'   {advance_line lexbuf; BEGIN}
| "#end"     [^'\n']* '\n'  {advance_line lexbuf; END}
(* Other comments --> ignore *)
| comment  { token lexbuf }


(* --  Rules for dealing with other tokens -- *)    
| decimal  as i	  { INTCONSTANT (int_of_string i)}
| decimal '.' (['0'-'9']*)   as f	  { FLOATCONSTANT (float_of_string f)}
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '('  { LPAREN }
| ')'  { RPAREN }
| '='  { EQ }
| ','  { COMMA }
| ':'  { COLON }
| '|'  { VBAR }

| "True"       {BCONSTANT true}
| "False"      {BCONSTANT false}
| "def"        {DEF}
| "if"         {IF}
| "else"       {ELSE}
| "while"      {WHILE}
| "return"     {RETURN}

| "=="         {BCEQ}
| ">="         {BCGE}
| '>'          {BCGT}
| "<="         {BCLE}
| '<'          {BCLT}
| "!="         {BCNE}

| "and"        {BLAND}
| "or"         {BLOR}

| "->"         {ARROW}

| eof          {EOF}

| alphus(alphus|num)* as i  { IDENTIFIER i }
| string as s {STRINGCONSTANT (clean_string s)}

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }

{
  (* wrapper around the token function; only really used in lexer2.mll, here added to have the same interface *)
  let token_wrap lb = token lb
}
