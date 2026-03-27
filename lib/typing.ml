open Auxdefs
open Lang


(* globals: all global variable declarations
   locals: local variable declarations of current function
 *)
type var_environment = {
    globals: (vname * tp) list; 
    locals: (vname * tp) list;
}
[@@deriving show]

(* fdecls: all the function declarations of the program
   static_vars: according to variable declarations, static
   dyn_vars: types of current variable assignments, dynamically changing
   curfun: current function (when within a function). Only for purposes of error messages.
 *)
type environment = { 
    fdecls: (vname * ((tp list) * tp)) list; 
    static_vars: var_environment;
    dyn_vars: var_environment;
    curfun: fname option;
    }
  [@@deriving show]

let tp_expr (env: environment) (e: expr) : tp =
  match e with
  | Const c -> 
      (match c with
       | IntV i -> UnionT [IntT]
       | BoolV b -> UnionT [BoolT]
       | FloatV f -> UnionT [FloatT]
       | StringV s -> UnionT [StringT]
       | _ -> UnionT [NoneT])
  | VarE v ->
      (try List.assoc v env.dyn_vars.locals
       with Not_found -> List.assoc v env.dyn_vars.globals)
  | _ -> UnionT [NoneT]
;;

let rec tp_stmt ((env, t, returned) : (environment * tp * bool)) s =
  match s with
  | Block[Assign(v,e)] -> 
    let t = tp_expr env e in Printf.printf "Type : %s\n" (Lang.show_tp t); true
  | _ -> Printf.printf "Type : inconnue"; true;;


let tp_fundefn init_env (Fundefn(Fundecl(fn, pards, rt), vds, s)) = true

  (* Function declarations of library / predefined functions *)
let library_fds = [
    ("input", ([UnionT[StringT]], UnionT[StringT]))
  ; ("int",   ([UnionT[BoolT; FloatT; IntT; StringT]], UnionT[IntT]))
  ; ("print", ([UnionT[StringT]], UnionT[NoneT]))
  ; ("str",   ([UnionT[BoolT; FloatT; IntT; StringT]], UnionT[StringT]))
  ]

(* The following has to be defined in detail *)
let tp_prog (Prog(fdefns, vds, s)) = 
  let fds = [] in
  let globs = [] in
  let init_venv = { globals = globs; locals = [] } in 
  let init_env = 
    { fdecls = fds @ library_fds; static_vars = init_venv; dyn_vars = init_venv; curfun = None } in
  if duplicate_free (List.map fst fds) 
    && duplicate_free (List.map fst globs) 
  && List.for_all (tp_fundefn init_env) fdefns
  then tp_stmt (init_env, UnionT[NoneT], false) s
  else failwith "duplicate function or variable declarations"
