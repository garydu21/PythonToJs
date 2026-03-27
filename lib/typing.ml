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

let tp_expr_const c =
  match c with
       | IntV i -> UnionT [IntT]
       | BoolV b -> UnionT [BoolT]
       | FloatV f -> UnionT [FloatT]
       | StringV s -> UnionT [StringT]
       | _ -> UnionT [NoneT]

let tp_expre_varE v env =
  (try List.assoc v env.dyn_vars.locals
     with Not_found ->
       try List.assoc v env.dyn_vars.globals
       with Not_found ->
         failwith ("Variable non définie : " ^ v))

let tp_expr (env: environment) (e: expr) : tp =
  match e with
  | Const c -> tp_expr_const c
  | VarE v -> tp_expre_varE v env
  | _ -> UnionT [NoneT]
;;

let rec tp_stmt ((env, t, returned) : (environment * tp * bool)) s =
  match s with
  | Block stmts ->
    (match stmts with
    | [] -> (env, t, returned)
    | stmt :: rest ->
        let (new_env, new_t, new_returned) = tp_stmt (env, t, returned) stmt in
        tp_stmt (new_env, new_t, new_returned) (Block rest))

  | Assign (v, e) ->
    let t_expr = tp_expr env e in
    Printf.printf "Type : %s\n" (Lang.show_tp t_expr);
    let new_env = {
      env with
      dyn_vars = {
        env.dyn_vars with
        globals = (v, t_expr) :: env.dyn_vars.globals
      }
    } in
    (new_env, t, returned)
;;


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
