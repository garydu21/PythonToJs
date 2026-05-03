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

let tp_union (UnionT ts1) (UnionT ts2) =
  mk_norm_tp (ts1 @ ts2)

let tp_compatible (UnionT ts1) (UnionT ts2) =
  List.for_all (fun b1 -> List.mem b1 ts2) ts1

let tp_expr_const c =
  match c with
  | IntV _ -> UnionT [IntT]
  | BoolV _ -> UnionT [BoolT]
  | FloatV _ -> UnionT [FloatT]
  | StringV _ -> UnionT [StringT]
  | _ -> UnionT [NoneT]

let tp_expre_varE v env =
  try List.assoc v env.dyn_vars.locals
  with Not_found ->
    try List.assoc v env.dyn_vars.globals
    with Not_found ->
      failwith ("Variable non définie : " ^ v)

let tp_expre_varE_static v env =
  try List.assoc v env.static_vars.locals
  with Not_found ->
    try List.assoc v env.static_vars.globals
    with Not_found ->
      failwith ("Variable non déclarée : " ^ v)

let tp_update_dyn v t env =
  if List.mem_assoc v env.dyn_vars.locals then
    { env with dyn_vars = { env.dyn_vars with locals = update_assoc v t env.dyn_vars.locals } }
  else
    { env with dyn_vars = { env.dyn_vars with globals = update_assoc v t env.dyn_vars.globals } }

let tp_merge_dyn env env1 env2 =
  let merge lst1 lst2 =
    List.map (fun (v, t1) ->
      let t2 = try List.assoc v lst2 with Not_found -> t1 in
      (v, tp_union t1 t2)
    ) lst1
  in
  { env with dyn_vars = {
    globals = merge env1.dyn_vars.globals env2.dyn_vars.globals;
    locals  = merge env1.dyn_vars.locals  env2.dyn_vars.locals;
  }}

let rec tp_expr (env: environment) (e: expr) : tp =
  match e with
  | Const c -> tp_expr_const c
  | VarE v -> tp_expre_varE v env
  | BinOp (op, e1, e2) -> tp_expre_binOp op e1 e2 env
  | CallE (f_name, exp_list) -> tp_expre_CallE f_name exp_list env

and tp_expre_binOp op e1 e2 env =
  let t1 = tp_expr env e1 in
  let t2 = tp_expr env e2 in
  match op with
  | BArith _ ->
      (match t1, t2 with
       | UnionT [IntT], UnionT [IntT] -> UnionT [IntT]
       | _ -> failwith "Opération arithmétique invalide")
  | BBool _ ->
      (match t1, t2 with
       | UnionT [BoolT], UnionT [BoolT] -> UnionT [BoolT]
       | _ -> failwith "Opération booléenne invalide")
  | BCompar _ ->
      (match t1, t2 with
       | UnionT [IntT], UnionT [IntT]
       | UnionT [BoolT], UnionT [BoolT]
       | UnionT [StringT], UnionT [StringT]
       | UnionT [FloatT], UnionT [FloatT] -> UnionT [BoolT]
       | _ -> failwith "Comparaison invalide")

and tp_expre_CallE f_name exp_list env =
  let (param_types, return_type) =
    try List.assoc f_name env.fdecls
    with Not_found ->
      failwith ("Fonction non définie : " ^ f_name)
  in
  let arg_types = List.map (tp_expr env) exp_list in
  if List.length arg_types <> List.length param_types then
    failwith ("Nombre d'arguments incorrect pour " ^ f_name)
  else
    let rec check_args args params =
      match args, params with
      | [], [] -> return_type
      | t_arg :: rest_args, t_param :: rest_params ->
          if tp_compatible t_arg t_param then
            check_args rest_args rest_params
          else
            failwith ("Type d'argument incorrect pour " ^ f_name)
      | _ -> failwith "Erreur interne CallE"
    in
    check_args arg_types param_types

let rec tp_stmt ((env, t, returned) : (environment * tp * bool)) s =
  match s with
  | Block stmts ->
      (match stmts with
       | [] -> (env, t, returned)
       | stmt :: rest ->
           let (new_env, new_t, new_returned) = tp_stmt (env, t, returned) stmt in
           tp_stmt (new_env, new_t, new_returned) (Block rest))

  | VardeclS (Vardecl(v, vt)) ->
      let add lst = if List.mem_assoc v lst then lst else lst @ [(v, vt)] in
      let in_fun = env.static_vars.locals <> [] in
      let new_static =
        if in_fun then { globals = env.static_vars.globals; locals = add env.static_vars.locals }
        else { globals = add env.static_vars.globals; locals = env.static_vars.locals }
      in
      let new_dyn =
        if in_fun then { globals = env.dyn_vars.globals; locals = add env.dyn_vars.locals }
        else { globals = add env.dyn_vars.globals; locals = env.dyn_vars.locals }
      in
      ({ env with static_vars = new_static; dyn_vars = new_dyn }, t, returned)

  | Assign (v, e) ->
      let t_expr = tp_expr env e in
      let t_static = tp_expre_varE_static v env in
      if not (tp_compatible t_expr t_static) then
        failwith ("Affectation invalide pour " ^ v);
      let new_env = tp_update_dyn v t_expr env in
      (new_env, t, returned)

  | Cond (cond, s1, s2) ->
      let _ = tp_expr env cond in
      let (env1, t1, ret1) = tp_stmt (env, t, false) s1 in
      let (env2, t2, ret2) = tp_stmt (env, t, false) s2 in
      let merged_env = tp_merge_dyn env env1 env2 in
      (merged_env, tp_union t1 t2, ret1 && ret2)

  | While (cond, body) ->
      let _ = tp_expr env cond in
      let rec fixpoint env_cur =
        let (env_after, _, _) = tp_stmt (env_cur, t, false) body in
        let env_merged = tp_merge_dyn env env_cur env_after in
        if env_merged.dyn_vars = env_cur.dyn_vars then env_after
        else fixpoint env_merged
      in
      let env_final = fixpoint env in
      (env_final, t, false)

  | Return e ->
      let t_e = tp_expr env e in
      (env, tp_union t t_e, true)

  | CallS (f_name, args) ->
      let _ = tp_expre_CallE f_name args env in
      (env, t, returned)
;;

let tp_fundefn init_env (Fundefn(Fundecl(fn, pards, rt), vds, s)) =
  let param_bindings = List.map (fun (Vardecl(n, t)) -> (n, t)) pards in
  let local_bindings = List.map (fun (Vardecl(n, t)) -> (n, t)) vds in
  let all_locals = param_bindings @ local_bindings in
  if not (duplicate_free (List.map fst all_locals)) then
    failwith ("Déclarations locales dupliquées dans la fonction " ^ fn);
  let local_venv = { globals = init_env.static_vars.globals; locals = all_locals } in
  let fun_env = { init_env with static_vars = local_venv; dyn_vars = local_venv; curfun = Some fn } in
  let (_, body_ret_tp, _) = tp_stmt (fun_env, UnionT [NoneT], false) s in
  let UnionT ts = body_ret_tp in
  let effective = UnionT (List.filter (fun b -> b <> NoneT) ts) in
  if tp_compatible effective rt then true
  else failwith ("Type de retour incorrect pour la fonction " ^ fn)

(* Function declarations of library / predefined functions *)
let library_fds = [
    ("input", ([UnionT [StringT]], UnionT [StringT]));
    ("int",   ([UnionT [BoolT; FloatT; IntT; StringT]], UnionT [IntT]));
    ("print", ([UnionT [StringT]; UnionT [BoolT; FloatT; IntT; NoneT; StringT]], UnionT [NoneT]));
    ("str",   ([UnionT [BoolT; FloatT; IntT; StringT]], UnionT [StringT]))
]

(* The following has to be defined in detail *)
let tp_prog (Prog(fdefns, vds, s)) = 
  let fds = List.map (fun (Fundefn(Fundecl(fn, pards, rt), _, _)) ->
    (fn, (List.map tp_of_vardecl pards, rt))
  ) fdefns in
  let globs = List.map (fun (Vardecl(n, t)) -> (n, t)) vds in
  let init_venv = { globals = globs; locals = [] } in 
  let init_env = 
    { fdecls = fds @ library_fds; static_vars = init_venv; dyn_vars = init_venv; curfun = None } in
  if duplicate_free (List.map fst fds) 
     && duplicate_free (List.map fst globs) 
     && List.for_all (tp_fundefn init_env) fdefns
  then tp_stmt (init_env, UnionT [NoneT], false) s
  else failwith "duplicate function or variable declarations"