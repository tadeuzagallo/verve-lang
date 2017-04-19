module T = Types
open Absyn

exception TypeError of string
exception UnificationError of string

type ty_env = (name * T.ty) list
type subst = (T.tvar * T.ty) list

let _type_id = ref 0
let new_var name =
  incr _type_id;
  T.TV (!_type_id, name)
let extend_env env (x, t) = (x, t)::env

let ty_int = T.Type "Int"
let ty_type = T.Type "Type"
let ty_unit = T.Type "Unit"

let default_env = [
  ("Type", ty_type);
  ("Int", ty_int);
  ("Void", ty_unit);
]

let (>>) s1 s2 =
  let apply s1 s2 =
    let aux (k, v) =
      try (k, List.assoc k s1)
      with Not_found -> (k, v)
    in List.map aux s2
  in apply s1 s2 @ s1

let rec apply s =
  let var v =
    try List.assoc v s
    with Not_found -> T.Var v
  in function
  | T.Type t -> T.Type t
  | T.Var v -> var v
  | T.Arrow (t1, t2) ->
      T.Arrow (apply s t1, apply s t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (apply s) types in
      T.TypeCtor (name, types')
  | T.TypeArrow (v1, t2) ->
      match var v1 with
      | T.Var v -> T.TypeArrow (v, apply s t2)
      | _ -> apply s t2

let rec unify = function
  | T.Type t1, T.Type t2 when t1 = t2 -> []

  | T.Var t1, t2
  | t2, T.Var t1 ->
      [(t1, t2)]

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      let s1 = unify (t11, t21) in
      let s2 = unify (apply s1 t12, apply s1 t22) in
      s2 >> s1

  | T.TypeArrow (_, t1), t2
  | t2, T.TypeArrow (_, t1) ->
      unify (t1, t2)

  | T.TypeCtor (n1, t1s), T.TypeCtor (n2, t2s) when n1 = n2 ->
      let aux s t1 t2 =
        unify (t1, t2) >> s
      in List.fold_left2 aux [] t1s t2s

  | t1, t2 ->
      let msg = Printf.sprintf "Failed to unify %s with %s"
        (T.to_string t1) (T.to_string t2)
      in raise (UnificationError msg)

let check_literal = function
  | Int _ -> ty_int

let rec instantiate s1 = function
  | T.TypeArrow (var, ty) ->
      let T.TV (_, name) = var in
      let var' = new_var name in
      T.TypeArrow (var', instantiate ([(var, T.Var var')] >> s1) ty)
  | T.Arrow (t1, t2) ->
      T.Arrow (instantiate s1 t1, instantiate s1 t2)
  | T.TypeCtor (n, ts) ->
      let ts' = List.map (instantiate s1) ts in
      T.TypeCtor (n, ts')
  | t -> apply s1 t

let get_type env v =
  try instantiate [] (List.assoc v env)
  with Not_found ->
    raise (TypeError "Unknown Type")

let rec check_type env : type_ -> T.ty * subst = function
  | Con t -> get_type env t, []
  | Arrow (parameters, return_type) ->
      let ret, s = check_type env return_type in
      let fn_type = List.fold_right
        (fun p (t, s1) -> let ty_p, s2 = check_type env p in T.Arrow (ty_p, t), s2 >> s1)
        parameters (ret, s)
      in fn_type
  | Inst (t, args) ->
      let ty = get_type env t in
      apply_generics env ty args []

and apply_generics env ty_callee gen_args s1 =
  let gen_args' = List.map (check_type env) gen_args in
  let check_type (call, s1) (g, s2) =
    match call with
    | T.TypeArrow (g', tail) ->
        let s = [(g', g)] >> s2 >> s1 in
        (apply s tail, s)
    | _ -> raise (TypeError "Invalid type for generic application")
  in
  List.fold_left check_type (ty_callee, s1) gen_args'

let rec check_fn env { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  let generics = match fn_generics with
  | Some g -> g
  | None -> []
  in

  let generics' = List.map (fun g -> new_var g.name) generics in
  let env' = List.fold_left
    (fun env (g, v : generic * T.tvar) -> extend_env env (g.name, T.Var v))
    env (List.combine generics generics')
  in

  let ret_type, s0 = check_type env' fn_return_type in

  let (fn_type, env'', s1) = List.fold_right
    (fun p (t, env'', s1) ->
      let ty, s2 = check_type env' p.param_type in
      (T.Arrow (ty , t), extend_env env'' (p.param_name, ty), s2 >> s1))
      fn_parameters (ret_type, env, s0)
  in
  let fn_type' = match fn_type with
  | T.Arrow _ -> fn_type
  | _ -> T.Arrow (ty_unit, fn_type)
  in
  let fn_type'' = List.fold_right (fun g t -> T.TypeArrow (g, t)) generics' fn_type' in

  let (ret, _, s2) = check_exprs env'' fn_body in
  let s3 = unify (ret, ret_type) in
  let fn_type'' = apply (s3 >> s2 >> s1) fn_type'' in

  match fn_name with
  | Some n -> (fn_type'', extend_env env (n, fn_type''), s3 >> s2 >> s1)
  | None -> (fn_type'', env, s3 >> s2 >> s1)

and check_generic_application env (callee, generic_arguments, arguments) =
  let generic_arguments = match generic_arguments with
  | Some g -> g
  | None -> []
  and arguments = match arguments with
  | None -> []
  | Some [] -> [Unit]
  | Some args -> args
  in

  let (ty_callee, _, s1) = check_expr env callee in

  let check (call, s1) argument =
    let (ty_arg, _, s2) = check_expr env argument in
    let rec check s3 ty =
      match ty with
      | T.Arrow (t1, t2) ->
          let s4 = unify (apply s3 t1, ty_arg) in
          (t2, s4 >> s3)
      | T.TypeArrow (v1, t2) ->
          let t2', s = check s3 t2 in
          T.TypeArrow (v1, t2'), s
      | _ -> raise (TypeError "Invalid type for function call")
    in
    check (s2 >> s1) call
  in
  let ty, s2 = apply_generics env ty_callee generic_arguments s1 in
  let ty', s3 = List.fold_left check (apply s2 ty, s2) arguments in
  (ty', env, s3)

and check_app env { callee; generic_arguments; arguments } =
  check_generic_application env (callee, generic_arguments, Some arguments)

and check_ctor env { ctor_name; ctor_generic_arguments; ctor_arguments } =
  check_generic_application env (Var ctor_name, ctor_generic_arguments, ctor_arguments)

and check_expr env : expr -> T.ty * ty_env * subst = function
  | Unit -> (ty_unit, env, [])
  | Literal l -> (check_literal l, env, [])
  | Var v -> (get_type env v, env, [])
  | Function fn -> check_fn env fn
  | Application app -> check_app env app
  | Ctor ctor -> check_ctor env ctor

and check_exprs env exprs =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_expr env node in
      (ty, env', s2 >> s1))
    (ty_unit, env, []) exprs

and check_enum_item make enum_ty (env, s1) { enum_item_name; enum_item_parameters } =
  match enum_item_parameters with
  | None -> extend_env env (enum_item_name, make enum_ty), s1
  | Some ps ->
      let aux p (enum_ty, s1) =
        let t, s2 = check_type env p in
        T.Arrow (t, enum_ty), s2 >> s1
      in
      let ty, s2 = List.fold_right aux ps (enum_ty, s1) in
      let ty' = make ty in
      extend_env env (enum_item_name, ty'), s2

and check_enum env { enum_name; enum_generics; enum_items } =
  let make, enum_ty, env' = match enum_generics with
    | None -> (fun x -> x), T.Type enum_name, env
    | Some gen ->
        let create_var (vars, env) g =
          let var = new_var g.name in
          (var :: vars, extend_env env (g.name, T.Var var))
        in
        let gen', env' = List.fold_left create_var ([], env) gen in
        let base_ty = T.TypeCtor (enum_name, List.map (fun v -> T.Var v) gen') in
        let make t = List.fold_right (fun g t -> T.TypeArrow (g, t)) gen' t in
        make, base_ty , env'
  in
  let env'' = extend_env env' (enum_name, make enum_ty) in
  let env''', s = List.fold_left (check_enum_item make enum_ty) (env'', []) enum_items in
  (enum_ty, env''', s)

and check_decl env = function
  | Expr expr -> check_expr env expr
  | Enum enum -> check_enum env enum

and check_decls env decls =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_decl env node in
      (ty, env', s2 >> s1))
    (ty_unit, env, []) decls

let check program =
  let ty, _, s = check_decls default_env program.body in
  apply s ty
