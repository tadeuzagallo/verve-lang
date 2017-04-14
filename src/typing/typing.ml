module T = Types
open Absyn

exception TypeError of string
exception UnificationError of string

type ty_env = (name * T.ty) list
type subst = (T.tvar * T.ty) list

let new_var name = T.TV (ref (), name)
let extend_env env (x, t) = (x, t)::env

let ty_int = T.Const "Int"
let ty_type = T.Const "Type"
let ty_unit = T.Const "Unit"

let default_env = [
  ("Type", ty_type);
  ("Int", ty_int);
  ("Void", ty_unit);
]

let (>>) s1 s2 =
  s1 @ s2

let rec apply s =
  let var v =
    try List.assoc v s
    with Not_found -> T.Var v
  in function
  | T.Const t -> T.Const t
  | T.Var v -> var v
  | T.Arrow (t1, t2) ->
      T.Arrow (apply s t1, apply s t2)
  | T.TypeArrow (v1, t2) ->
      T.TypeArrow (v1, apply s t2)

let rec unify = function
  | T.Const t1, T.Const t2 when t1 = t2 -> []

  | T.Var t1, t2
  | t2, T.Var t1 ->
      [(t1, t2)]

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      let s1 = unify (t11, t21) in
      let s2 = unify (apply s1 t12, apply s1 t22) in
      s2 >> s1

  | T.TypeArrow (v11, t12), T.Arrow (t21, t22)
  | T.Arrow (t21, t22), T.TypeArrow (v11, t12) ->
      unify (apply [(v11, t21)] t12, apply [(v11, t21)] (T.Arrow(t21, t22)))

  | t1, t2 ->
      let msg = Printf.sprintf "Failed to unify %s with %s"
        (T.to_string t1) (T.to_string t2)
      in raise (UnificationError msg)

let check_literal = function
  | Int _ -> ty_int

let get_type env v =
  try List.assoc v env
  with Not_found ->
    raise (TypeError "Unknown Type")

let rec check_type env : type_ -> T.ty = function
  | Con t -> get_type env t
  | Arrow (parameters, return_type) ->
      let ret = check_type env return_type in
      let fn_type = List.fold_right
        (fun p t -> T.Arrow (check_type env p, t))
        parameters ret
      in fn_type

let rec check_fn env { name; generics; parameters; return_type; body } =
  let generics' = List.map (fun (g: generic) -> new_var g.name) generics in
  let env' = List.fold_left
    (fun env (g, v : generic * T.tvar) -> extend_env env (g.name, T.Var v))
    env (List.combine generics generics')
  in

  let ret_type = check_type env' return_type in


  let (fn_type, env'') = List.fold_right
    (fun p (t, env'') ->
      let ty = check_type env' p.type_ in
      (T.Arrow (ty , t), extend_env env'' (p.name, ty)))
    parameters (ret_type, env)
  in
  let fn_type' = List.fold_right (fun g t -> T.TypeArrow (g, t)) generics' fn_type in
  let (ret, _, s1) = check_exprs env'' body in
  let s2 = unify (ret, ret_type) in

  match name with
  | Some n -> (fn_type', extend_env env (n, fn_type'), s2 >> s1)
  | None -> (fn_type', env, s2 >> s1)

and check_app env { callee; generic_arguments; arguments } =
  let (ty_callee, _, s1) = check_expr env callee in
  let gen_args = List.map (check_type env) generic_arguments in

  let check_type (call, s) g =
    match call with
    | T.TypeArrow (g', tail) ->
        (tail, [(g', g)] >> s)
    | _ -> raise (TypeError "Invalid type for generic application")
  and check (call, s1) argument =
    let (ty_arg, _, s2) = check_expr env argument in
    match call with
    | T.Arrow (t1, t2) ->
        let s3 = unify (t1, ty_arg) in
        (t2, s3 >> s2 >> s1)
    | _ -> raise (TypeError "Invalid type for function call")
  in
  let ty, s2 = List.fold_left check_type (ty_callee, s1) gen_args in
  let ty', s3 = List.fold_left check (apply s2 ty, s2) arguments in
  (ty', env, s3)

and check_expr env : expr -> T.ty * ty_env * subst = function
  | Unit -> (ty_unit, env, [])
  | Literal l -> (check_literal l, env, [])
  | Var v -> (get_type env v, env, [])

  | Function fn -> check_fn env fn
  | Application app -> check_app env app

and check_exprs env exprs =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_expr env node in
      (ty, env', s2 >> s1))
    (ty_unit, env, []) exprs

let check program =
  let ty, _, s = check_exprs default_env program.body in
  apply s ty
