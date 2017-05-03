open Absyn
open Env
open Type_error

module T = Types

let check_literal = function
  | Int _ -> val_int
  | String _ -> val_string

let rec check_type env : type_ -> T.ty * subst = function
  | Arrow (parameters, return_type) ->
      let ret, s = check_type env return_type in
      let fn_type = List.fold_right
        (fun p (t, s1) -> let ty_p, s2 = check_type env p in T.Arrow (ty_p, t), s2 >> s1)
        parameters (ret, s)
      in fn_type
  | Inst (t, args) ->
      let ty = match get_type env t with
      | T.TypeInst _ as t ->  raise (Error (Value_as_type t))
      | t -> to_value t
      in apply_generics env ty args []
  | RecordType fields ->
    let aux (fields, s1) (name, t) =
      let ty, s2 = check_type env t in
      (name, ty) :: fields, s2 >> s1
    in
    List.fold_left aux ([], []) fields
    |> fun (fields', s) -> T.Record (List.rev fields'), s

and apply_generics env ty_callee gen_args s1 =
  let gen_args' = List.map (check_type env) gen_args in
  let check_type (call, s1) (g, s2) =
    match call with
    | T.TypeArrow (g', tail) ->
        let s = [(g', g)] >> s2 >> s1 in
        (apply s tail, s)
    | _ -> raise (Error Invalid_generic_application)
  in
  List.fold_left check_type (ty_callee, s1) gen_args'

let rec check_fn env { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  let generics' = List.map (var_of_generic env) fn_generics in
  let env' = List.fold_left2
    (fun env g v -> extend_env env (g.name, T.RigidVar v))
    env fn_generics generics'
  in

  let ret_type, s0 = check_type env' fn_return_type in

  let (fn_type, params, s1) = List.fold_right
    (fun p (t, env'', s1) ->
      let ty, s2 = check_type env' p.param_type in
      (T.Arrow (ty , t), extend_env env'' (p.param_name, ty), s2 >> s1))
      fn_parameters (ret_type, [], s0)
  in
  let fn_type' = match fn_type with
  | T.Arrow _ -> fn_type
  | _ -> T.Arrow (val_void, fn_type)
  in
  let fn_type'' = List.fold_right (fun g t -> T.TypeArrow (g, t)) generics' fn_type' in

  let fn_env env =
    params @ env
  in
  let (ret, _, s2) = match fn_name with
    | None -> check_exprs (fn_env env') fn_body
    | Some n -> check_exprs (fn_env @@ extend_env env' (n, fn_type'')) fn_body
  in
  let s3 = unify ~expected:ret_type ~actual:ret in
  let fn_type'' = loosen @@ apply (s3 >> s2 >> s1) fn_type'' in

  match fn_name with
  | Some n -> (fn_type'', extend_env env (n, fn_type''), s3 >> s2 >> s1)
  | None -> (fn_type'', env, s3 >> s2 >> s1)

and check_generic_application env s1 (ty_callee, generic_arguments, arguments) =
  let arguments = match arguments with
  | None -> []
  | Some [] -> [Unit]
  | Some args -> args
  in

  let aux arg (args, subst) =
    let (arg, _, s2) = check_expr env arg in
    (arg :: args), s2 >> subst
  in
  let arguments', s2 = List.fold_right aux arguments ([], s1) in

  let ty, s3 = apply_generics env ty_callee generic_arguments s2 in
  let ty, s4 = apply_arguments (apply s3 ty) arguments' in
  let subst = s4 >> s3 >> s2 >> s1 in
  (apply subst ty, env, subst)

and apply_arguments callee arguments=
  let check (call, s1) argument =
    let rec check s3 ty =
      match ty with
      | T.Arrow (t1, t2) ->
          let s4 = unify ~expected:(apply s3 t1) ~actual:argument in
          (t2, s4 >> s3)
      | T.TypeArrow (v1, t2) ->
          let t2', s = check s3 t2 in
          T.TypeArrow (v1, t2'), s
      | _ -> raise (Error Invalid_application)
    in
    check s1 call
  in
  List.fold_left check (callee, []) arguments

and check_app env ({ callee; generic_arguments; arguments } as app) =
  let (ty_callee, _, s1) = check_expr env callee in
  let ty, env', s = check_generic_application env s1 (ty_callee, generic_arguments, arguments) in
  let rec aux acc = function
    | T.TypeArrow (var, t) -> aux (apply s (T.Var var) :: acc) t
    | _ -> List.rev acc
  in app.generic_arguments_ty <- aux [] ty_callee;
  ty, env', s

and check_ctor env { ctor_name; ctor_generic_arguments; ctor_arguments } =
  let (ty_ctor, _, s1) = check_expr env (Var ctor_name) in
  check_generic_application env s1 (ty_ctor, ctor_generic_arguments, ctor_arguments)

and check_record env fields =
  let aux (fields, s1) (name, v) =
    let ty, _, s2 = check_expr env v in
    ((name, ty) :: fields, s2 >> s1)
  in
  let ty, s = List.fold_left aux ([], []) fields in
  T.Record (List.rev ty), env, s

and check_field_access env { record; field } =
  let record, _, s1 = check_expr env record in
  let fields = match record with
    | T.Record r -> r
    | _ -> raise (Error (Invalid_access (field, record)))
  in
  try
    let ty = List.assoc field fields in
    ty, env, s1
  with Not_found ->
    raise (Error (Unknown_field (field, record)))

and check_match env { match_value; cases } =
  let ty, _, s1 = check_expr env match_value in
  let var = make_var () in
  let s2 = List.fold_left (check_case env ty var) s1 cases in
  apply s2 var, env, s2

and check_case env value_ty ret_ty s1 { pattern; case_value } =
  let pat_ty, env', s2 = check_pattern env pattern in
  let s3 = unify ~expected:pat_ty ~actual:value_ty in
  let case_ty, env'', s4 = check_exprs env' case_value in
  let s5 = unify ~expected:ret_ty ~actual:(apply (s4 >> s3 >> s2) case_ty) in
  s5 >> s1

and check_pattern env = function
  | Pany ->
    make_var (), env, []

  | Pvar v ->
    let var = make_var () in
    var, extend_env env (v, var), []

  | Pctor (name, ps) ->
      match ps, get_type env name with
      | None, t ->
          t, env, []
      | Some l, (T.Arrow _ as t)
      | Some l, (T.TypeArrow _ as t) ->
        let aux p (ps, env, subst) =
          let (p, env', s2) = check_pattern env p in
          (p :: ps), env', s2 >> subst
        in
        let patterns, env', s1 = List.fold_right aux l ([], env, []) in
        let ty, s2 = apply_arguments t patterns in
        let subst = s2 >> s1 in
        apply subst ty, env', subst

      | _, t ->
        raise (Error (Invalid_pattern (Pctor (name, ps), t)))

and check_operator env op =
  check_fn env (fn_of_operator op)

and check_binop env binop =
  let app = app_of_binop binop in
  let res = check_app env app in
  binop.bin_generic_arguments_ty <- app.generic_arguments_ty;
  res

and check_expr env : expr -> T.ty * ty_env * subst = function
  | Unit -> (val_void, env, [])
  | Literal l -> (check_literal l, env, [])
  | Var v -> (get_type env v, env, [])
  | Function fn -> check_fn env fn
  | Application app -> check_app env app
  | Ctor ctor -> check_ctor env ctor
  | Record r -> check_record env r
  | Field_access f -> check_field_access env f
  | Match m -> check_match env m
  | Operator o -> check_operator env o
  | Binop b -> check_binop env b
  | Wrapped expr -> check_expr env expr

and check_exprs env exprs =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_expr env node in
      (ty, env', s2 >> s1))
    (val_void, env, []) exprs
