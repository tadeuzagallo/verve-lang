open Absyn
open Runtime_error

module T = Types
module V = Value

(* Expr *)

let rec expr env e =
  match e.expr_desc with
  | Unit -> V.Unit
  | Literal l -> V.Literal l
  | Wrapped e -> expr env e
  | Ctor c -> eval_ctor env c
  | ClassCtor c -> eval_class_ctor env c
  | Application app -> eval_app env app
  | Var v -> eval_var env v
  | Record r -> eval_record env r
  | Field_access f -> eval_field_access env f
  | Match m -> eval_match env m
  | Binop bin -> expr env ({ expr_loc = e.expr_loc; expr_desc = Application (app_of_binop bin) })
  | Function fn -> V.Function fn
  | If if_ -> eval_if env if_
  | MethodCall mc -> eval_method_call env mc

and eval_ctor env c =
  let args = match c.ctor_arguments with
    | None -> None
    | Some args -> Some (List.map (expr env) args)
  in V.Ctor { c with ctor_arguments = args }

and eval_class_ctor env cc =
  let aux (n, v) = (n.str, expr env v) in
  let props = List.map aux cc.cc_record in
  match Rt_env.find_name cc.cc_name env with
  | V.Class (c, fns) -> V.Object (c, fns, props)
  | _ -> assert false

and eval_app env app =
  let callee = expr env app.callee in
  let arguments = match app.arguments with
    | None -> []
    | Some a -> a
  in
  run_app env callee app.generic_arguments_ty arguments

and run_app env callee generic_args arguments =
  let arguments = List.map (expr env) arguments in
  match callee with
    | V.Builtin (_, fn) -> fn env arguments
    | _ ->
      let arguments = List.map V.expr_of_value arguments in
      fst @@ stmts env (Subst.subst generic_args arguments callee)

and eval_var env var =
  let value =
    try Rt_env.find_name var.var_name env
    with Not_found ->
      error (Unbound_variable var.var_name)
  in
  match value, var.var_type with
  | V.InterfaceFunction (fn, None), t :: _ ->
    V.InterfaceFunction (fn, Some t)
  | t, _ -> t

and eval_record env record =
  let record = List.map (fun (n, v) -> (n.str, expr env v)) record in
  V.Record record

and eval_field_access env fa =
  eval_field_access2 env fa.record fa.field

and eval_field_access2 env record field =
  let value = expr env record in
  match value with
  | V.Record fields
  | V.Object (_, _, fields) ->
    List.assoc field.str fields
  | _ -> raise Not_found

and eval_ufcs env mc =
  let fn = Rt_env.find_name [mc.mc_method] env in
  let obj = V.expr_of_value (expr env mc.mc_object) in
  let v = run_app env fn mc.mc_ty_args [obj] in
  match v with
  | V.Function _ -> v
  | _ -> V.Function {
      fn_name = None;
      fn_generics = [];
      fn_parameters = [];
      fn_return_type = None;
      fn_body = [{stmt_desc = Expr (V.expr_of_value v); stmt_loc = dummy_loc}];
    }

and eval_if env if_ =
  let v = expr env if_.if_cond in
  match v with
  | V.Ctor { ctor_name= [{ str = "True" }] } ->
    fst (stmts env if_.if_conseq)
  | V.Ctor { ctor_name= [{ str = "False" }] } ->
    eval_else env if_.if_alt
  | _ -> assert false

and eval_else env = function
  | None -> V.Unit
  | Some (ElseIf if_) -> eval_if env if_
  | Some (ElseBlock block) -> fst (stmts env block)

and eval_match env { match_value; cases } =
  let v = expr env match_value in
  let matched_case = List.find (matched_case env v) cases in
  eval_case env v matched_case

and matched_case env value { pattern } =
  matched_case2 env value pattern

and matched_case2 env value pattern =
  match pattern.pat_desc, value with
  | Pany, _ -> true
  | Pvar _, _ -> true
  | Pctor (name, None), V.Ctor { ctor_name; ctor_arguments = None } when name = ctor_name -> true
  | Pctor (name, Some ps), V.Ctor { ctor_name; ctor_arguments=(Some args) } when name = ctor_name ->
    List.for_all2 (matched_case2 env) args ps
  | _ -> false

and eval_case env value p =
  let env' = eval_pattern env p.pattern value in
  let value, _ = stmts env' p.case_value in
  value

and eval_pattern env pat value =
  match pat.pat_desc, value with
  | Pany, _ -> env
  | Pvar x, v -> Rt_env.extend_name env x v
  | Pctor (_, ps), V.Ctor { ctor_arguments } ->
    begin match ps, ctor_arguments with
    | None, None -> env
    | Some patterns, Some args ->
      List.fold_left2 eval_pattern env patterns args
    | _ -> assert false
    end
  | _ -> assert false

and eval_method_call env mc =
  let fn =
    try eval_field_access2 env mc.mc_object mc.mc_method
    with _ -> eval_ufcs env mc
  in
  run_app env fn [] mc.mc_args

(* Stmt *)
and stmts env stmts =
  List.fold_left (fun (_, env) s -> stmt env s) (V.Unit, env) stmts

and stmt env stmt =
  match stmt.stmt_desc with
  | Expr e -> expr env e, env
  | Let l -> eval_let env l
  | FunctionStmt fn ->
    let fn' = expr env { expr_loc = stmt.stmt_loc; expr_desc = (Function fn) } in
    match fn.fn_name with
    | Some name -> fn', Rt_env.extend_name env name fn'
    | None -> assert false

and eval_let env { let_var; let_value } =
  let value = expr env let_value in
  value, Rt_env.extend_name env let_var value

(* decl *)
and decl env decl =
  match decl.decl_desc with
  | Stmt s -> stmt env s
  | Enum { enum_name } -> (V.Type enum_name.str, env)
  | TypeAlias { ta_name } -> (V.Type ta_name.str, env)
  | Interface intf -> eval_intf env intf
  | Operator op -> eval_operator env op
  | Implementation impl -> eval_impl env impl
  | Class c -> eval_class env c

and eval_intf env intf =
  Hashtbl.add Rt_env.intf_to_impls intf.intf_name.str (ref []);
  let env' = List.fold_left (eval_intf_item intf.intf_name) env intf.intf_items in
  (V.Unit, env')

and eval_intf_item intf_name env = function
  | Prototype { proto_name }
  | OperatorPrototype { oproto_name = proto_name } ->
    Hashtbl.add Rt_env.fn_to_intf proto_name.str intf_name.str;
    Rt_env.extend_name env proto_name (V.InterfaceFunction (proto_name.str, None))

and eval_operator env op =
  let value = expr env ({ expr_loc = dummy_loc; expr_desc = Function (fn_of_operator op) }) in
  V.Unit, Rt_env.extend_name env op.op_name value

and eval_impl env impl =
  let ty = match impl.impl_arg_type with
    | Some t -> t
    | None -> assert false
  in
  let impls = Hashtbl.find Rt_env.intf_to_impls (Rt_env.last impl.impl_name).str in
  impls := (ty, List.map eval_impl_item impl.impl_items) :: !impls;
  (V.Unit, env)

and eval_impl_item = function
  | ImplOperator op ->
    (op.op_name.str, fn_of_operator op)
  | ImplFunction impl_fn ->
    match impl_fn.fn_name with
    | Some n -> (n.str, impl_fn)
    | None -> assert false

and eval_class env cls =
  let aux env fn = match fn.fn_name with
    | None -> assert false
    | Some n ->
      let this = {
        param_name = mk_name "this";
        param_type = {
          type_desc = Inst (mk_qualified_name ["Self"], []);
          type_loc = dummy_loc;
        }
      } in
      let fn = {
        fn with fn_parameters = this :: fn.fn_parameters
      }
      in
      Rt_env.extend_name env n (V.Function fn)
  in
  let value = V.Class (cls.class_name.str, []) in
  let env = Rt_env.extend_name env cls.class_name value in
  let env = List.fold_left aux env cls.class_fns in
  value, env
