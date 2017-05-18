open Absyn
open Env
open Type_error

module T = Types

let map_record fn fields =
  let aux (key, value) =
    (key.str, fn value)
  in
  List.map aux fields

let check_literal = function
  | Int _ -> ty_int
  | String _ -> ty_string

let rec check_type env ty =
  match ty.type_desc with
  | Arrow (parameters, return_type) ->
    let ret = check_type env return_type in
    let params = match parameters with
      | [] -> [Env.ty_void]
      | p -> List.map (check_type env) p
    in
    List.fold_right T.arrow params ret
  | Inst (t, args) ->
    let ty = instantiate (Env.find_type env t) in
    let ty = apply_generics env ty args in
    begin match T.desc ty with
    | T.TypeArrow _ -> raise (Error Invalid_generic_application_few)
    | _ -> ty
    end
  | RecordType fields ->
    T.record (map_record (check_type env) fields)

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
        T.type_arrow v1 (check t2)
      | _ -> raise (Error Invalid_application)
    in
    check call
  in
  List.fold_left check callee arguments

let check_generics ?(var=T.var) env generics =
  let aux (acc, env) g =
    let v = var (var_of_generic env g) in
    (v :: acc, Env.add_type env g.name v)
  in
  let generics, env = List.fold_left aux ([], env) generics in
  List.rev generics, env

let rec check_fn env { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
  let generics, env = check_generics ~var:T.rigid_var env fn_generics in

  let param_names = List.map (fun p -> p.param_name) fn_parameters in
  let param_types = List.map (fun p -> check_type env p.param_type) fn_parameters in

  let ret_type = match fn_return_type with
    | None -> Env.ty_void
    | Some t -> check_type env t
  in
  let fn_type = match fn_parameters with
    | [] ->  T.arrow ty_void ret_type
    | _ -> List.fold_right T.arrow param_types ret_type
  in
  let fn_type = List.fold_right T.type_arrow generics fn_type in

  let env = match fn_name with
    | None -> env
    | Some n -> Env.add_value env n fn_type
  in
  let aux env name p = Env.add_value env name (Env.constrain generics p) in
  let env = List.fold_left2 aux env param_names param_types in
  let ret, _ = check_stmts env fn_body in
  unify ~expected:ret_type ret;
  loosen fn_type

and run_application env (ty_callee, generic_arguments, arguments) =
  let arguments = match arguments with
  | None -> []
  | Some [] -> [{ expr_loc = dummy_loc; expr_desc = Unit }]
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

and check_class_ctor env cc =
  let aux props (p, e) =
    let ty = check_expr env e in
    try
      let expected = List.assoc p.str props in
      unify ~expected ty;
      List.remove_assoc p.str props
    with Not_found ->
      raise (Error (Unknown_property p))
  in
  let t = instantiate (Env.find_type env cc.cc_name) in
  let t = apply_generics env t cc.cc_generics in
  match T.desc t with
  | T.Class cls ->
    let missing = List.fold_left aux cls.T.cls_props cc.cc_record in
    begin match missing with
    | [] -> t
    | _ ->
      let m = List.map fst missing in
      raise (Error (Missing_properties m))
    end
  | _ -> raise (Error (Constructor_not_class cc.cc_name))

and check_record env fields =
  T.record (map_record (check_expr env) fields)

and check_field_access env { record; field } =
  let record = check_expr env record in
  let fields = match T.desc record with
    | T.Record r -> r
    | T.Class { T.cls_props } -> cls_props
    | _ -> raise (Error (Invalid_access (field, record)))
  in
  get_record_field record fields field

and get_record_field record fields field =
  try
    List.assoc field.str fields
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

and check_pattern env pat =
  match pat.pat_desc with
  | Pany -> make_var (), env

  | Pvar v ->
    let var = make_var () in
    var, Env.add_value env v var

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
        raise (Error (Invalid_pattern (pat, t)))

and check_binop env binop =
  let app = app_of_binop binop in
  let res = check_app env app in
  binop.bin_generic_arguments_ty <- app.generic_arguments_ty;
  res

and check_if env if_ =
  let ty_cond = check_expr env if_.if_cond in
  unify ~expected:Env.ty_bool ty_cond;

  let ty_conseq, _ = check_stmts env if_.if_conseq in
  let ty_alt = check_else env if_.if_alt in
  unify ~expected:ty_conseq ty_alt;

  ty_conseq

and check_else env = function
  | None -> Env.ty_void
  | Some (ElseIf if_) -> check_if env if_
  | Some (ElseBlock block) -> fst (check_stmts env block)

and check_var env var =
  let t = Env.find_value env var.var_name in
  let rec aux ts t =
    match T.desc t with
    | T.TypeArrow (t1, t2) ->
      aux (t1 :: ts) t2
    | _ -> List.rev ts
  in
  var.var_type <- aux [] t;
  t

and check_method_call env mc =
  let obj = check_expr env mc.mc_object in
  match T.desc obj with
  | T.Class c ->
      let fn =
        try
          List.assoc mc.mc_method.str !(c.T.cls_fns)
        with Not_found ->
          raise (Error (Unknown_method (mc.mc_method, c.T.cls_name)))
      in
      run_application env (fn, [], Some mc.mc_args)
  | T.Record c ->
    let fn = get_record_field obj c mc.mc_method in
    run_application env (fn, [], Some mc.mc_args)
  | _ ->
    raise (Error (Callee_not_object (obj, mc.mc_method)))

and check_expr env expr =
  match expr.expr_desc with
  | Unit -> ty_void
  | Literal l -> check_literal l
  | Var var -> check_var env var
  | Function fn -> check_fn env fn
  | Application app -> check_app env app
  | Ctor ctor -> check_ctor env ctor
  | ClassCtor ctor -> check_class_ctor env ctor
  | Record r -> check_record env r
  | Field_access f -> check_field_access env f
  | Match m -> check_match env m
  | Binop b -> check_binop env b
  | Wrapped expr -> check_expr env expr
  | If if_ -> check_if env if_
  | MethodCall mc -> check_method_call env mc

(* Statements *)
and check_let env { let_var; let_value } =
  let ty = check_expr env let_value in
  ty, Env.add_value env let_var ty

and check_fn_stmt env fn =
  let ty = check_fn env fn in
  let name = match fn.fn_name with
    | Some n -> n
    | None -> assert false
  in
  ty, Env.add_value env name ty

and check_stmt env stmt =
  match stmt.stmt_desc with
  | Let let_ -> check_let env let_
  | FunctionStmt fn -> check_fn_stmt env fn
  | Expr expr -> check_expr env expr, env

and check_stmts env stmts =
  List.fold_left
    (fun (_, env ) node -> check_stmt env node)
    (Env.ty_void, env) stmts
