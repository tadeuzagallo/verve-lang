open Absyn
open Env
open Type_error

module T = Types

let rec check_enum env { enum_name; enum_generics; enum_items } =
  let create_var g (vars, env) =
    let var = T.var @@ var_of_generic env g in
    (var :: vars, Env.add_type env g.name var)
  in
  let gen, env_internal = List.fold_right create_var enum_generics ([], env) in
  let enum_ty = T.type_ctor (enum_name.str, gen) in
  let make t = List.fold_right T.type_arrow gen t in
  let external_ty = make enum_ty in
  let env_internal = Env.add_type env_internal enum_name external_ty in
  let env_external = Env.add_type env enum_name external_ty in
  let env_external = List.fold_left (check_enum_item env_internal make enum_ty) env_external enum_items in
  enum_ty, env_external

and check_enum_item env_internal make item_ty env_external { enum_item_name; enum_item_parameters } =
  match enum_item_parameters with
  | None -> Env.add_ctor env_external enum_item_name (make item_ty)
  | Some ps ->
      let aux p enum_ty =
        let t = Typing_expr.check_type env_internal p in
        T.arrow t enum_ty
      in
      let ty = List.fold_right aux ps item_ty in
      let ty' = make ty in
      Env.add_ctor env_external enum_item_name ty'

and check_interface env { intf_name; intf_param; intf_items } =
  let intf_desc = { T.intf_name = intf_name.str; T.intf_items = []; T.intf_impls = [] } in
  let intf_ty = T.interface intf_desc in
  let env' = Env.add_type env intf_name intf_ty in
  let generic = { name = intf_param.name; constraints = [intf_name]::intf_param.constraints } in
  let var = T.var @@ var_of_generic env' generic in

  let fns, env'' = List.fold_left (check_intf_item (intf_param.name, var)) ([], env') intf_items in
  intf_desc.T.intf_items <- fns;
  intf_ty, env''

and check_intf_item pair (fns, env) = function
  | Prototype p -> check_proto pair (fns, env) p
  | OperatorPrototype op -> check_proto pair (fns, env) (prototype_of_op_proto op)

and check_proto (var_name, var) (fns, env) { proto_name; proto_generics; proto_params; proto_ret_type } =
  let env' = Env.add_type env var_name var in
  let generics' = List.map (fun  v -> T.rigid_var @@ var_of_generic env v) proto_generics in
  let env' = List.fold_left
      (fun env (g, v) -> Env.add_type env g.name v)
      env' (List.combine proto_generics generics')
  in
  let ret_type = match proto_ret_type with
    | None -> Env.ty_void
    | Some t -> Typing_expr.check_type env' t
  in
  let make_arrow param t =
    let param_ty = Typing_expr.check_type env' param in
    T.arrow param_ty t
  in
  let fn_ty = List.fold_right make_arrow proto_params ret_type in
  let fn_ty' = match T.desc fn_ty with
  | T.Arrow _ -> fn_ty
  | _ -> T.arrow ty_void fn_ty
  in
  let fn_ty'' = List.fold_right (fun g t -> T.type_arrow g t) generics' fn_ty' in
  let fn_ty''' = loosen fn_ty'' in
  let fn_ty'''' = T.type_arrow var fn_ty''' in
  (proto_name.str, fn_ty'''')::fns, Env.add_value env proto_name fn_ty''''

and check_implementation env ({ impl_name; impl_arg; impl_items } as impl) =
  let impl_arg_ty = Typing_expr.check_type env impl_arg in
  impl.impl_arg_type <- Some (impl_arg_ty);
  let impl_name' = List.map (fun g -> g.str) impl_name in
  let impl_desc = { T.impl_name = impl_name'; T.impl_type = impl_arg_ty; T.impl_items = [] } in
  let intf_desc =
    let t = Env.find_type env impl_name in
    match T.desc t with
    | T.Interface i -> i
    | _ -> raise (Error (Invalid_implementation (impl_name, t)))
  in
  intf_desc.T.intf_impls <- (impl_arg_ty, impl_desc) :: intf_desc.T.intf_impls;
  let names = List.fold_left (check_impl_item intf_desc env) [] impl_items in
  check_missing_impls names intf_desc;
  T.implementation impl_desc

and check_missing_impls names intf =
  let aux (fn_name, _) =
    if not (List.exists (fun n -> n.str = fn_name) names) then
      raise (Error (Missing_implementation (intf.T.intf_name, fn_name)))
  in
  List.iter aux intf.T.intf_items

and check_impl_item intf env names = function
  | ImplFunction fn ->
    let name = match fn.fn_name with Some n -> n | None -> assert false in
    check_matches_intf env name intf (Typing_expr.check_fn env fn);
    name :: names
  | ImplOperator op ->
    check_matches_intf env op.op_name intf (fst @@ check_operator env op);
    op.op_name :: names

and check_matches_intf env name intf ty =
  let ty' =
    try List.assoc name.str intf.T.intf_items
    with Not_found -> raise (Error (Extraneous_implementation (intf.T.intf_name, name)))
  in
  unify ~expected:(Env.instantiate ty') ty

and check_operator env op =
  let ty = Typing_expr.check_fn env (fn_of_operator op) in
  ty, Env.add_value env op.op_name ty

and check_type_alias env ta =
  let generics, env' = Typing_expr.check_generics env ta.ta_generics in
  let ty = Typing_expr.check_type env' ta.ta_type in
  let ty = List.fold_right T.type_arrow generics ty in
  Env.ty_type, Env.add_type env ta.ta_name ty

and check_class env cls =
  let generics, env_internal = Typing_expr.check_generics ~var:T.rigid_var env cls.class_generics in
  let cls_name = cls.class_name.str in
  let aux cp = (cp.cp_name.str, Typing_expr.check_type env_internal cp.cp_type) in
  let cls_props = List.map aux cls.class_props in
  let cls_desc = { T.cls_name; T.cls_props; T.cls_fns = ref []; T.cls_generics = generics } in
  let ty = T.class_ cls_desc in
  let env_internal = Env.add_value env_internal (mk_name "this") ty in
  let ty = List.fold_right T.type_arrow generics ty in
  let env_internal = Env.add_type env_internal cls.class_name ty in

  let aux (env_internal, env_external) fn =
    match fn.fn_name with
    | None -> assert false
    | Some n ->
      let t = Typing_expr.check_fn ~make_arrow:false env_internal fn in
      let t = T.arrow ty t in
      let t = List.fold_right T.type_arrow generics t in
      let t = loosen t in
      (Env.add_value env_internal n t, Env.add_value env_external n t)
  in
  let (_, env_external) = List.fold_left aux (env_internal, env) cls.class_fns in
  let ty = loosen ty in
  ty, Env.add_type env_external cls.class_name ty

and check_decl env decl =
  match decl.decl_desc with
  | Stmt stmt -> Typing_expr.check_stmt env stmt
  | Enum enum -> check_enum env enum
  | Interface intf -> check_interface env intf
  | TypeAlias ta -> check_type_alias env ta
  | Implementation impl -> check_implementation env impl, env
  | Class c -> check_class env c
  | Operator op -> check_operator env op

and check_decls env decls =
  List.fold_left
    (fun (_, env) node ->
      let ty, env' = check_decl env node in
      (ty, env'))
    (ty_void, env) decls
