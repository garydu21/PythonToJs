open PPrint
open Lang


(* Global constant *) 
let indent_level = 4

let doc_of_var v = string v

let doc_of_var_list vs = parens (separate_map comma string vs)

let doc_of_binop op =
  match op with
  | BArith BAadd -> string "+"
  | BArith BAsub -> string "-"
  | BArith BAmul -> string "*"
  | BArith BAdiv -> string "/"
  | BArith BAmod -> string "%"
  | BBool BBand  -> string "&&"
  | BBool BBor   -> string "||"
  | BCompar BCeq -> string "=="
  | BCompar BCne -> string "!="
  | BCompar BClt -> string "<"
  | BCompar BCle -> string "<="
  | BCompar BCgt -> string ">"
  | BCompar BCge -> string ">="

let doc_of_value v =
  match v with
  | IntV n    -> string (string_of_int n)
  | BoolV b   -> string (if b then "true" else "false")
  | FloatV f  -> string (string_of_float f)
  | StringV s -> string ("\"" ^ s ^ "\"")
  | NoneV     -> string "null"

let rec doc_of_expr e =
  match e with
  | Const v -> doc_of_value v
  | VarE v  -> doc_of_var v
  | BinOp (op, e1, e2) ->
      parens (doc_of_expr e1 ^^ space ^^ doc_of_binop op ^^ space ^^ doc_of_expr e2)
  | CallE (f, args) ->
      string f ^^ parens (separate_map (string ", ") doc_of_expr args)

let rec doc_of_stmt s =
  match s with
  | Block stmts ->
      braces (nest indent_level (hardline ^^ separate_map hardline doc_of_stmt stmts) ^^ hardline)
  | VardeclS (Vardecl(vn, _)) ->
      string "let" ^^ space ^^ string vn ^^ string ";"
  | Assign (v, e) ->
      string v ^^ string " = " ^^ doc_of_expr e ^^ string ";"
  | Cond (cond, s1, s2) ->
      string "if" ^^ space ^^ parens (doc_of_expr cond) ^^ space ^^ doc_of_stmt s1
      ^^ space ^^ string "else" ^^ space ^^ doc_of_stmt s2
  | While (cond, body) ->
      string "while" ^^ space ^^ parens (doc_of_expr cond) ^^ space ^^ doc_of_stmt body
  | Return e ->
      string "return" ^^ parens (doc_of_expr e) ^^ string ";"
  | CallS (f, args) ->
      string f ^^ parens (separate_map (string ", ") doc_of_expr args) ^^ string ";"

let doc_of_local_vardecl (Vardecl(vn,_t)) = string "let" ^^ space ^^ string vn ^^ string ";"
let doc_of_global_vardecl (Vardecl(vn,_t)) = string "var" ^^ space ^^ string vn ^^ string ";"

let doc_of_fundefn (Fundefn(Fundecl(fn, params, _rt), vds, s)) =
  let header =
    separate space [
      string "function"; string fn; (doc_of_var_list (List.map name_of_vardecl params))
    ]
  in
  let locals = separate_map hardline doc_of_local_vardecl vds in
  let body =
    if vds = [] then doc_of_stmt s
    else braces (nest indent_level (hardline ^^ locals ^^ hardline ^^ doc_of_stmt s) ^^ hardline)
  in
  header ^^ space ^^ body

let doc_of_prog (Prog(fdfs, vds, s)) = 
  (separate_map hardline doc_of_fundefn fdfs) ^^
  hardline ^^
  (separate hardline
          [
            (separate hardline (List.map doc_of_global_vardecl vds)) 
          ; (doc_of_stmt s)
          ]  ) ^^  
  hardline


let print_prog prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog prg);
  flush stdout

let extract_inputs stmts =
  List.filter_map (fun s ->
    match s with
    | Assign (v, CallE ("int", [CallE ("input", [Const (StringV msg)])])) -> Some (v, msg)
    | Assign (v, CallE ("input", [Const (StringV msg)])) -> Some (v, msg)
    | _ -> None
  ) stmts

let extract_print stmts =
  List.filter_map (fun s ->
    match s with
    | CallS ("print", [Const (StringV msg); CallE (f, args)]) -> Some (msg, f, args)
    | _ -> None
  ) stmts

let doc_of_input_html (v, msg) =
  string (Printf.sprintf "<label for=\"%s\">%s</label>" v msg) ^^ hardline ^^
  string (Printf.sprintf "<input type=\"number\" id=\"%s\"><br><br>" v)

let doc_of_print_html (msg, f, args) =
  let args_js = String.concat ", " (List.map (fun v ->
    match v with
    | VarE vn -> Printf.sprintf "parseInt(document.getElementById('%s').value)" vn
    | _ -> "?"
  ) args) in
  string (Printf.sprintf "<label>%s</label> <label id=\"demo\"></label>" msg) ^^ hardline ^^
  string "<p></p>" ^^ hardline ^^
  string (Printf.sprintf "<button onclick=\"document.getElementById('demo').innerHTML = %s(%s)\">" f args_js) ^^ hardline ^^
  string "Compute </button>"

let doc_of_prog_html (Prog(fdfs, _vds, s)) =
  let Block stmts = s in
  let title = string "<h1>PythonToJS</h1>" in
  let script_open = string "<script>" in
  let script_content = separate_map (hardline ^^ hardline) doc_of_fundefn fdfs in
  let script_close = string "</script>" in
  let inputs = extract_inputs stmts in
  let prints = extract_print stmts in
  string "<!DOCTYPE html>" ^^ hardline ^^
  string "<html>" ^^ hardline ^^
  string "<body>" ^^ hardline ^^
  title ^^ hardline ^^
  script_open ^^ hardline ^^
  script_content ^^ hardline ^^
  script_close ^^ hardline ^^
  separate_map hardline doc_of_input_html inputs ^^ hardline ^^
  separate_map hardline doc_of_print_html prints ^^ hardline ^^
  string "</body>" ^^ hardline ^^
  string "</html>"

let print_prog_html prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog_html prg);
  flush stdout