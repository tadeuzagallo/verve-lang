open Absyn
open Env
open Type_error

module T = Types

let rec check_enum env { enum_name; enum_generics; enum_items } =
  let create_var g (vars, env) =
    let var = T.var @@ var_of_generic env g in
    (var :: vars, Env.add_type env g.name var)
  in
  let gen, env' = List.fold_right create_var enum_generics ([], env) in
  let enum_ty = T.type_ctor (enum_name.str, gen) in
  let make t = List.fold_right T.type_arrow gen t in
  let env'' = Env.add_type env' enum_name (make enum_ty) in
  let env''' = List.fold_left (check_enum_item make enum_ty) env'' enum_items in
  enum_ty, env'''

and check_enum_item make item_ty env { enum_item_name; enum_item_parameters } =
  match enum_item_parameters with
  | None -> Env.add_ctor env enum_item_name (make item_ty)
  | Some ps ->
      let aux p enum_ty =
        let t = Typing_expr.check_type env p in
        T.arrow t enum_ty
      in
      let ty = List.fold_right aux ps item_ty in
      let ty' = make ty in
      add_ctor env enum_item_name ty'

and check_interface env { intf_name; intf_param; intf_items } =
  let intf_ty = T.interface { T.intf_name = intf_name.str; T.intf_impls = [] } in
  let env' = Env.add_type env intf_name intf_ty in
  let generic = { name = intf_param.name; constraints =
intf_name::intf_param.constraints } in
  let var = T.var @@ var_of_generic env' generic in
  let env'' = List.fold_left (check_intf_item (intf_param.name, var)) env' intf_items in
  intf_ty, env''

and check_intf_item pair env = function
  | Prototype p -> check_proto pair env p
  | OperatorPrototype op -> check_proto pair env (prototype_of_op_proto op)

and check_proto (var_name, var) env { proto_name; proto_generics; proto_params; proto_ret_type } =
  let env' = Env.add_type env var_name var in
  let generics' = List.map (fun  v -> T.rigid_var @@ var_of_generic env v) proto_generics in
  let env' = List.fold_left
      (fun env (g, v) -> Env.add_type env g.name v)
      env' (List.combine proto_generics generics')
  in
  let ret_type = Typing_expr.check_type env' proto_ret_type in
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
  Env.add_value env proto_name fn_ty''''

and check_implementation env ({ impl_name; impl_arg; impl_items } as impl) =
  let impl_arg_ty = Typing_expr.check_type env impl_arg in
  impl.impl_arg_type <- Some (impl_arg_ty);
  let impl_desc = { T.impl_name = impl_name.str; T.impl_type = impl_arg_ty; T.impl_items = [] } in
  let impl_ty = T.implementation impl_desc in
  let intf_desc =
    let t = Env.find_type env impl_name in
    match T.desc t with
    | T.Interface i -> i
    | _ -> raise (Error (Invalid_implementation (impl_name, t)))
  in
  intf_desc.T.intf_impls <- (impl_arg_ty, impl_desc) :: intf_desc.T.intf_impls;
  impl_ty

and check_operator env op =
  let ty = Typing_expr.check_fn env (fn_of_operator op) in
  Env.ty_void, Env.add_value env op.op_name ty

and check_decl env decl =
  match decl.decl_desc with
  | Stmt stmt -> Typing_expr.check_stmt env stmt
  | Enum enum -> check_enum env enum
  | Interface intf -> check_interface env intf
  | Implementation impl -> check_implementation env impl, env
  | Operator op -> check_operator env op

and check_decls env decls =
  List.fold_left
    (fun (_, env) node ->
      let ty, env' = check_decl env node in
      (ty, env'))
    (ty_void, env) decls
