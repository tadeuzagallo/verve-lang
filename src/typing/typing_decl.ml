open Absyn
open Env
open Type_error

let rec check_enum env { enum_name; enum_generics; enum_items } =
  let create_var g (vars, env) =
    let var = var_of_generic env g in
    (var :: vars, extend_env env (g.name, T.Var var))
  in
  let gen, env' = List.fold_right create_var enum_generics ([], env) in
  let enum_ty = T.TypeCtor (enum_name, List.map (fun v -> T.Var v) gen) in
  let item_ty = T.TypeInst (enum_name, List.map (fun v -> T.Var v) gen) in
  let make t = List.fold_right (fun g t -> T.TypeArrow (g, t)) gen t in
  let env'' = extend_env env' (enum_name, make enum_ty) in
  let env''', s = List.fold_left (check_enum_item make item_ty) (env'', []) enum_items in
  (enum_ty, env''', s)

and check_enum_item make item_ty (env, s1) { enum_item_name; enum_item_parameters } =
  match enum_item_parameters with
  | None -> extend_env env (enum_item_name, make item_ty), s1
  | Some ps ->
      let aux p (enum_ty, s1) =
        let t, s2 = Typing_expr.check_type env p in
        T.Arrow (t, enum_ty), s2 >> s1
      in
      let ty, s2 = List.fold_right aux ps (item_ty, s1) in
      let ty' = make ty in
      extend_env env (enum_item_name, ty'), s2

and check_interface env { intf_name; intf_param; intf_items } =
  let intf_ty = T.Interface { T.intf_name; T.intf_impls = [] } in
  let env' = extend_env env (intf_name, intf_ty) in
  let generic = { name = intf_param.name; constraints =
intf_name::intf_param.constraints } in
  let var = var_of_generic env' generic in
  let env'' = List.fold_left (check_intf_item (intf_param.name, var)) env' intf_items in
  (intf_ty, env'', [])

and check_intf_item pair env = function
  | Prototype p -> check_proto pair env p
  | OperatorPrototype op -> check_proto pair env (prototype_of_op_proto op)

and check_proto (var_name, var) env { proto_name; proto_generics; proto_params; proto_ret_type } =
  let env' = extend_env env (var_name, T.RigidVar var) in
  let generics' = List.map (var_of_generic env) proto_generics in
  let env' = List.fold_left
      (fun env (g, v) -> extend_env env (g.name, T.RigidVar v))
      env' (List.combine proto_generics generics')
  in
  let ret_type, s1 = Typing_expr.check_type env' proto_ret_type in
  let make_arrow param (t, s1) =
    let param_ty, s2 = Typing_expr.check_type env' param in
    T.Arrow (param_ty, t), s2 >> s1
  in
  let fn_ty, s = List.fold_right make_arrow proto_params (ret_type, s1) in
  let fn_ty' = match fn_ty with
  | T.Arrow _ -> fn_ty
  | _ -> T.Arrow (val_void, fn_ty)
  in
  let fn_ty'' = List.fold_right (fun g t -> T.TypeArrow (g, t)) generics' fn_ty' in
  let fn_ty''' = loosen @@ apply s fn_ty'' in
  let fn_ty'''' = T.TypeArrow (var, fn_ty''') in
  extend_env env (proto_name, fn_ty'''')

and check_implementation env ({ impl_name; impl_arg; impl_items } as impl) =
  let impl_arg_ty, s1 = Typing_expr.check_type env impl_arg in
  impl.impl_arg_type <- Some (impl_arg_ty);
  let impl_desc = { T.impl_name; T.impl_type = impl_arg_ty; T.impl_items = [] } in
  let impl_ty = T.Implementation impl_desc in
  let intf_desc =
    match get_type env impl_name with
    | T.Interface i -> i
    | t -> raise (Error (Invalid_implementation (impl_name, t)))
  in
  intf_desc.T.intf_impls <- (impl_arg_ty, impl_desc) :: intf_desc.T.intf_impls;
  (impl_ty, env, s1)


and check_decl env = function
  | Expr expr -> Typing_expr.check_expr env expr
  | Enum enum -> check_enum env enum
  | Interface intf -> check_interface env intf
  | Implementation impl -> check_implementation env impl

and check_decls env decls =
  List.fold_left
    (fun (_, env, s1) node ->
      let ty, env', s2 = check_decl env node in
      (ty, env', s2 >> s1))
    (val_void, env, []) decls
