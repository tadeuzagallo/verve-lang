open Absyn
open Env
open Type_error

module T = Types

let check_literal = function
  | Int _ -> ty_int
  | String _ -> ty_string

let rec check_type env = function
  | Arrow (parameters, return_type) ->
      let ret = check_type env return_type in
      let fn_type = List.fold_right
        (fun p t -> let ty_p = check_type env p in T.arrow ty_p t)
        parameters ret
      in fn_type
  | Inst (t, args) ->
    let ty = instantiate (Env.find_type env t) in
    apply_generics env ty args
  | RecordType fields ->
    let aux fields (name, t) = (name, check_type env t) :: fields in
    List.fold_left aux [] fields
    |> fun f -> T.record (List.rev f)

and apply_generics env ty_callee gen_args =
  let gen_args' = List.map (check_type env) gen_args in
  let check_type call g =
    match T.desc call with
    | T.TypeArrow (g', tail) ->
      unify ~expected:g' g;
      tail
    | _ -> raise (Error Invalid_generic_application)
  in
  List.fold_left check_type ty_callee gen_args'

let rec check_fn env { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  let generics' = List.map (fun v -> T.rigid_var @@ var_of_generic env v) fn_generics in
  let env' = List.fold_left2
    (fun env g v -> Env.add_type env (g.name, v))
    env fn_generics generics'
  in

  let ret_type = check_type env' fn_return_type in

  let fn_type, params = List.fold_right
    (fun p (t, env'') ->
      let ty = check_type env' p.param_type in
      (T.arrow ty t, Env.add_value env'' (p.param_name, ty)))
      fn_parameters (ret_type, Env.empty)
  in
  let fn_type' = match (T.desc fn_type) with
  | T.Arrow _ -> fn_type
  | _ ->  T.arrow ty_void fn_type
  in
  let fn_type'' = List.fold_right (fun g t -> T.type_arrow g t) generics' fn_type' in

  let fn_env env =
    Env.merge params env
  in
  let (ret, _) = match fn_name with
    | None -> check_exprs (fn_env env') fn_body
    | Some n -> check_exprs (fn_env @@ Env.add_value env' (n, fn_type'')) fn_body
  in
  unify ~expected:ret_type ret;
  let fn_type'' = loosen fn_type'' in

  match fn_name with
  | Some n -> (fn_type'', Env.add_value env (n, fn_type''))
  | None -> (fn_type'', env)

and check_generic_application env (ty_callee, generic_arguments, arguments) =
  let arguments = match arguments with
  | None -> []
  | Some [] -> [Unit]
  | Some args -> args
  in

  let aux arg args =
    let (arg, _) = check_expr env arg in
    (arg :: args)
  in
  let arguments' = List.fold_right aux arguments [] in

  let ty = apply_generics env ty_callee generic_arguments in
  apply_arguments ty arguments'

and apply_arguments callee arguments =
  let check call argument =
    let rec check ty =
      match T.desc ty with
      | T.Arrow (t1, t2) ->
        unify ~expected:t1 argument;
        t2
      | T.TypeArrow (v1, t2) ->
        check t2
      | _ -> raise (Error Invalid_application)
    in
    check call
  in
  List.fold_left check callee arguments

and check_app env ({ callee; generic_arguments; arguments } as app) =
  let (ty_callee, _) = check_expr env callee in
  let ty = check_generic_application env (ty_callee, generic_arguments, arguments) in
  let rec aux acc t =
    match T.desc t with
    | T.TypeArrow (var, t) ->
      aux (var :: acc) t
    | _ -> List.rev acc
  in app.generic_arguments_ty <- aux [] ty_callee;
  ty

and check_ctor env { ctor_name; ctor_generic_arguments; ctor_arguments } =
  let ty_ctor = Env.find_ctor env ctor_name in
  check_generic_application env (ty_ctor, ctor_generic_arguments, ctor_arguments)

and check_record env fields =
  let aux fields (name, v) =
    let ty, _ = check_expr env v in
    (name, ty) :: fields
  in
  let ty = List.fold_left aux [] fields in
  T.record (List.rev ty)

and check_field_access env { record; field } =
  let record, _ = check_expr env record in
  let fields = match T.desc record with
    | T.Record r -> r
    | _ -> raise (Error (Invalid_access (field, record)))
  in
  try
    let ty = List.assoc field fields in
    ty, env
  with Not_found ->
    raise (Error (Unknown_field (field, record)))

and check_match env { match_value; cases } =
  let ty, _  = check_expr env match_value in
  let var = make_var () in
  List.iter (check_case env ty var) cases;
  var, env

and check_case env value_ty ret_ty { pattern; case_value } =
  let pat_ty, env' = check_pattern env pattern in
  unify ~expected:pat_ty value_ty;
  let case_ty, env'' = check_exprs env' case_value in
  unify ~expected:ret_ty case_ty;

and check_pattern env = function
  | Pany ->
    make_var (), env

  | Pvar v ->
    let var = make_var () in
    var, Env.add_value env (v, var)

  | Pctor (name, ps) ->
      let t = Env.find_ctor env name in
      match ps, T.desc t with
      | None, _ ->
          t, env
      | Some l, T.Arrow _
      | Some l, T.TypeArrow _ ->
        let aux p (ps, env) =
          let (p, env') = check_pattern env p in
          (p :: ps), env'
        in
        let patterns, env' = List.fold_right aux l ([], env) in
        let ty = apply_arguments t patterns in
        ty, env'

      | _, _ ->
        raise (Error (Invalid_pattern (Pctor (name, ps), t)))

and check_operator env op =
  let _, env' =  check_fn env (fn_of_operator op) in
  ty_void, env'

and check_binop env binop =
  let app = app_of_binop binop in
  let res = check_app env app in
  binop.bin_generic_arguments_ty <- app.generic_arguments_ty;
  res

and check_let env { let_var; let_value } =
  let ty, env = check_expr env let_value in
  ty, Env.add_value env (let_var, ty)

and check_expr env = function
  | Unit -> (ty_void, env)
  | Literal l -> (check_literal l, env)
  | Var v -> (Env.find_value env v, env)
  | Function fn -> check_fn env fn
  | Application app -> check_app env app, env
  | Ctor ctor -> check_ctor env ctor, env
  | Record r -> check_record env r, env
  | Field_access f -> check_field_access env f
  | Match m -> check_match env m
  | Operator o -> check_operator env o
  | Binop b -> check_binop env b, env
  | Wrapped expr -> check_expr env expr
  | Let l -> check_let env l

and check_exprs env exprs =
  List.fold_left
    (fun (_, env ) node -> check_expr env node)
    (ty_void, env) exprs
