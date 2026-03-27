open PPrint
open Lang


(* Global constant *)
let indent_level = 4

let doc_of_var v = string v

let doc_of_var_list vs = parens (separate_map comma string vs)

let rec doc_of_stmt s = string "INCOMPLET"
let doc_of_local_vardecl (Vardecl(vn,_t)) = string "let" ^^ space ^^ string vn ^^ string ";"
let doc_of_global_vardecl (Vardecl(vn,_t)) = string "var" ^^ space ^^ string vn ^^ string ";"

let doc_of_fundefn (Fundefn(Fundecl(fn, params, _rt), vds, s)) =
      (separate space [
        string "function"; string fn; (doc_of_var_list (List.map name_of_vardecl params))
        ]) ^^ space ^^ string "INCOMPLET"

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
    
