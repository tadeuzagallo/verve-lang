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
    let params = List.map (check_type env) parameters in
    List.fold_right T.arrow params ret
  | Inst (t, args) ->
    let ty = instantiate (Env.find_type env t) in
    apply_generics env ty args
  | RecordType fields ->
    let keys, types = List.split fields in
    let types = List.map (check_type env) types in
    T.record (List.combine keys types)

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

let rec check_fn env { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  let generics = List.map (fun g -> T.rigid_var (var_of_generic env g)) fn_generics in
  let generic_names = List.map (fun g -> g.name) fn_generics in
  let env = List.fold_left Env.add_type env (List.combine generic_names generics) in

  let param_names = List.map (fun p -> p.param_name) fn_parameters in
  let param_types = List.map (fun p -> check_type env p.param_type) fn_parameters in

  let ret_type = check_type env fn_return_type in
  let fn_type = match fn_parameters with
    | [] ->  T.arrow ty_void ret_type
    | _ -> List.fold_right T.arrow param_types ret_type
  in
  let fn_type = List.fold_right T.type_arrow generics fn_type in

  let env = match fn_name with
    | None -> env
    | Some n -> Env.add_value env (n, fn_type)
  in
  let env = List.fold_left Env.add_value env (List.combine param_names param_types) in
  let ret, _ = check_stmts env fn_body in
  unify ~expected:ret_type ret;
  loosen fn_type

and run_application env (ty_callee, generic_arguments, arguments) =
  let arguments = match arguments with
  | None -> []
  | Some [] -> [Unit]
  | Some args -> args
  in

  let arguments' = List.map (check_expr env) arguments in
  let ty = apply_generics env ty_callee generic_arguments in
  apply_arguments ty arguments'


and check_app env ({ callee; generic_arguments; arguments } as app) =
  let ty_callee = check_expr env callee in
  let ty = run_application env (ty_callee, generic_arguments, arguments) in
  let rec aux acc t =
    match T.desc t with
    | T.TypeArrow (var, t) ->
      aux (var :: acc) t
    | _ -> List.rev acc
  in app.generic_arguments_ty <- aux [] ty_callee;
  ty

and check_ctor env { ctor_name; ctor_generic_arguments; ctor_arguments } =
  let ty_ctor = Env.find_ctor env ctor_name in
  run_application env (ty_ctor, ctor_generic_arguments, ctor_arguments)

and check_record env fields =
  let keys, values = List.split fields in
  let values = List.map (check_expr env) values in
  T.record (List.combine keys values)

and check_field_access env { record; field } =
  let record = check_expr env record in
  let fields = match T.desc record with
    | T.Record r -> r
    | _ -> raise (Error (Invalid_access (field, record)))
  in
  try
    List.assoc field fields
  with Not_found ->
    raise (Error (Unknown_field (field, record)))

and check_match env { match_value; cases } =
  let ty = check_expr env match_value in
  let var = make_var () in
  List.iter (check_case env ty var) cases;
  var

and check_case env value_ty ret_ty { pattern; case_value } =
  let pat_ty, env' = check_pattern env pattern in
  unify ~expected:pat_ty value_ty;
  let case_ty, _ = check_stmts env' case_value in
  unify ~expected:ret_ty case_ty

and check_pattern env = function
  | Pany -> make_var (), env

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

and check_binop env binop =
  let app = app_of_binop binop in
  let res = check_app env app in
  binop.bin_generic_arguments_ty <- app.generic_arguments_ty;
  res

and check_expr env = function
  | Unit -> ty_void
  | Literal l -> check_literal l
  | Var v -> Env.find_value env v
  | Function fn -> check_fn env fn
  | Application app -> check_app env app
  | Ctor ctor -> check_ctor env ctor
  | Record r -> check_record env r
  | Field_access f -> check_field_access env f
  | Match m -> check_match env m
  | Binop b -> check_binop env b
  | Wrapped expr -> check_expr env expr

(* Statements *)
and check_let env { let_var; let_value } =
  let ty = check_expr env let_value in
  ty, Env.add_value env (let_var, ty)

and check_fn_stmt env fn =
  let ty = check_fn env fn in
  let name = match fn.fn_name with
    | Some n -> n
    | None -> assert false
  in
  ty, Env.add_value env (name, ty)

and check_stmt env = function
  | Let let_ -> check_let env let_
  | FunctionStmt fn -> check_fn_stmt env fn
  | Expr expr -> check_expr env expr, env

and check_stmts env stmts =
  List.fold_left
    (fun (_, env ) node -> check_stmt env node)
    (Env.ty_void, env) stmts
