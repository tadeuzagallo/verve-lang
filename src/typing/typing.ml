open Absyn
open Type_error

module T = Types

type ty_env = (name * T.ty) list
type subst = (T.tvar * T.ty) list

let extend_env env (x, t) = (x, t)::env

let ty_int = T.TypeCtor ("Int", [])
let ty_type = T.TypeCtor ("Type", [])
let ty_void = T.TypeCtor ("Void", [])

let val_int = T.TypeInst ("Int", [])
let val_type = T.TypeInst ("Type", [])
let val_void = T.TypeInst ("Void", [])

let default_env = [
  ("Type", ty_type);
  ("Int", ty_int);
  ("Void", ty_void);
]

let (>>) s1 s2 =
  let apply s1 s2 =
    let aux (k, v) =
      try (k, List.assoc k s1)
      with Not_found -> (k, v)
    in List.map aux s2
  in apply s1 s2 @ s1

let rec apply s = function
  | T.Arrow (t1, t2) ->
      T.Arrow (apply s t1, apply s t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (apply s) types in
      T.TypeCtor (name, types')
  | T.TypeInst (name, types) ->
      let types' = List.map (apply s) types in
      T.TypeInst (name, types')
  | T.Interface i -> T.Interface i
  | T.Implementation i -> T.Implementation i
  | T.RigidVar var -> T.RigidVar var
  | T.Var ({ T.name; T.constraints } as var) ->
    begin try List.assoc var s
    with Not_found -> T.Var var
    end
  | T.TypeArrow (v1, t2) ->
      let v1' =
        try List.assoc v1 s
        with Not_found -> T.Var v1
      in match v1' with
      | T.Var v -> T.TypeArrow (v, apply s t2)
      | _ -> apply s t2

let rec unify = function
  | T.TypeInst (n2, t2s), T.TypeInst (n1, t1s)
  when n1 = n2 ->
      let aux s t1 t2 =
        unify (t1, t2) >> s
      in List.fold_left2 aux [] t1s t2s

  | T.TypeCtor _, t when t = val_type -> []
  | t, T.TypeCtor _ when t = val_type -> []

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      let s1 = unify (t11, t21) in
      let s2 = unify (apply s1 t12, apply s1 t22) in
      s2 >> s1

  (* Order matters: Var must come before TypeArrow *)
  | T.Var ({ T.constraints = cs1 } as var1),
    T.Var ({ T.constraints = cs2 } as var2) ->
      let aux c1 c2 = String.compare c1.T.intf_name c2.T.intf_name in
      [(var1, T.Var { var2 with T.constraints = List.sort_uniq aux (cs1 @ cs2)})]

  | T.Var ({ T.constraints } as var), t
  | t, T.Var ({ T.constraints } as var) ->
      let impls intf_desc =
        match t with
        | T.Var var
        | T.RigidVar var ->
            if not (List.mem intf_desc var.T.constraints) then
              raise (Error (Instance_not_found (t, intf_desc)))
        | t ->
            if not (List.mem_assoc t intf_desc.T.intf_impls) then
              raise (Error (Instance_not_found (t, intf_desc)))
      in
      List.iter impls constraints;
      [ var, t ]

  | T.TypeArrow (_, t1), t2
  | t2, T.TypeArrow (_, t1) ->
      unify (t1, t2)

  | T.RigidVar v1, T.RigidVar v2 when v1 = v2 -> []

  | t1, t2 ->
      raise (Error (Unification_error (t1, t2)))

let _fresh_tbl = Hashtbl.create 256
let _default_id = 1
let _fresh name =
  try
    let type_id = Hashtbl.find _fresh_tbl name in
    incr type_id;
    !type_id
  with Not_found ->
    Hashtbl.add _fresh_tbl name (ref _default_id);
    _default_id

let fresh ({ T.name } as var) =
  { var with T.id = _fresh name }

let rec instantiate s1 = function
  | T.Arrow (t1, t2) ->
      T.Arrow (instantiate s1 t1, instantiate s1 t2)
  | T.TypeArrow (var, ty) ->
      let var' = fresh var in
      let s = [(var, T.Var var')] >> s1 in
      T.TypeArrow (var', instantiate s ty)
  | T.TypeCtor (n, ts) ->
      let ts' = List.map (instantiate s1) ts in
      T.TypeCtor (n, ts')
  | t -> apply s1 t

let rec loosen = function
  | T.Arrow (t1, t2) -> T.Arrow (loosen t1, loosen t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (var, loosen ty)
  | T.TypeInst (n, ts) -> T.TypeInst (n, List.map loosen ts)
  | T.RigidVar var -> T.Var var
  | t -> t

let get_type env v =
  try instantiate [] (List.assoc v env)
  with Not_found ->
    raise (Error (Unknown_type v))

let rec to_value = function
  | T.TypeCtor (n, ts) -> T.TypeInst (n, List.map to_value ts)
  | T.TypeInst (n, ts) -> T.TypeInst (n, List.map to_value ts)
  | T.Arrow (t1, t2) -> T.Arrow (to_value t1, to_value t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (var, to_value ty)
  | t -> t

let var_of_generic env { name; constraints } =
  let resolve n = match get_type env n with
    | T.Interface i -> i
    | t -> raise (Error (Invalid_constraint (name, t)))
  in
  let intfs = List.map resolve constraints in
  { T.id = _fresh name; T.name; T.constraints = intfs }

let check_literal = function
  | Int _ -> val_int

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
  let env' = List.fold_left
    (fun env (g, v : generic * T.tvar) -> extend_env env (g.name, T.RigidVar v))
    env (List.combine fn_generics generics')
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
  | _ -> T.Arrow (val_void, fn_type)
  in
  let fn_type'' = List.fold_right (fun g t -> T.TypeArrow (g, t)) generics' fn_type' in

  let (ret, _, s2) = check_exprs env'' fn_body in
  let s3 = unify (ret, ret_type) in
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
      | _ -> raise (Error Invalid_application)
    in
    check (s2 >> s1) call
  in
  let ty, s2 = apply_generics env ty_callee generic_arguments s1 in
  let ty', s3 = List.fold_left check (apply s2 ty, s2) arguments in
  (apply s3 ty', env, s3)

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

and check_expr env : expr -> T.ty * ty_env * subst = function
  | Unit -> (val_void, env, [])
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
    (val_void, env, []) exprs

and check_enum_item make item_ty (env, s1) { enum_item_name; enum_item_parameters } =
  match enum_item_parameters with
  | None -> extend_env env (enum_item_name, make item_ty), s1
  | Some ps ->
      let aux p (enum_ty, s1) =
        let t, s2 = check_type env p in
        T.Arrow (t, enum_ty), s2 >> s1
      in
      let ty, s2 = List.fold_right aux ps (item_ty, s1) in
      let ty' = make ty in
      extend_env env (enum_item_name, ty'), s2

and check_enum env { enum_name; enum_generics; enum_items } =
  let create_var (vars, env) g =
    let var = var_of_generic env g in
    (var :: vars, extend_env env (g.name, T.Var var))
  in
  let gen, env' = List.fold_left create_var ([], env) enum_generics in
  let enum_ty = T.TypeCtor (enum_name, List.map (fun v -> T.Var v) gen) in
  let item_ty = T.TypeInst (enum_name, List.map (fun v -> T.Var v) gen) in
  let make t = List.fold_right (fun g t -> T.TypeArrow (g, t)) gen t in
  let env'' = extend_env env' (enum_name, make enum_ty) in
  let env''', s = List.fold_left (check_enum_item make item_ty) (env'', []) enum_items in
  (enum_ty, env''', s)

and check_interface env { intf_name; intf_param; intf_functions } =
  let intf_ty = T.Interface { T.intf_name; T.intf_impls = [] } in
  let env' = extend_env env (intf_name, intf_ty) in
  let generic = { name = intf_param.name; constraints =
intf_name::intf_param.constraints } in
  let var = var_of_generic env' generic in
  let env'' = List.fold_left (check_proto (intf_param.name, var)) env' intf_functions in
  (intf_ty, env'', [])

and check_proto (var_name, var) env { proto_name; proto_generics; proto_params; proto_ret_type } =
  let env' = extend_env env (var_name, T.RigidVar var) in
  let ret_type, s1 = check_type env' proto_ret_type in
  let make_arrow param (t, s1) =
    let param_ty, s2 = check_type env' param in
    T.Arrow (param_ty, t), s2 >> s1
  in
  let fn_ty, s = List.fold_right make_arrow proto_params (ret_type, s1) in
  let fn_ty' = match fn_ty with
  | T.Arrow _ -> fn_ty
  | _ -> T.Arrow (val_void, fn_ty)
  in
  let fn_ty'' = List.fold_right (fun g t -> T.TypeArrow (var_of_generic env' g, t)) proto_generics fn_ty' in
  let fn_ty''' = loosen @@ apply s fn_ty'' in
  let fn_ty'''' = T.TypeArrow (var, fn_ty''') in
  extend_env env (proto_name, fn_ty'''')

and check_implementation env ({ impl_name; impl_arg; impl_functions } as impl) =
  let impl_arg_ty, s1 = check_type env impl_arg in
  impl.impl_arg_type <- Some (impl_arg_ty);
  let impl_desc = { T.impl_name; T.impl_type = impl_arg_ty; T.impl_functions = [] } in
  let impl_ty = T.Implementation impl_desc in
  let intf_desc =
    match get_type env impl_name with
    | T.Interface i -> i
    | t -> raise (Error (Invalid_implementation (impl_name, t)))
  in
  intf_desc.T.intf_impls <- (impl_arg_ty, impl_desc) :: intf_desc.T.intf_impls;
  (impl_ty, env, s1)

and check_decl env = function
  | Expr expr -> check_expr env expr
  | Enum enum -> check_enum env enum
  | Interface intf -> check_interface env intf
  | Implementation impl -> check_implementation env impl

and check_decls env decls =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_decl env node in
      (ty, env', s2 >> s1))
    (val_void, env, []) decls

let check program =
  try
    let ty, _, s = check_decls default_env program.body in
    apply s ty
  with Error e ->
    report_error Format.err_formatter e;
    Format.pp_print_newline Format.err_formatter ();
    exit 1
