open Absyn
open Runtime_error

module T = Types
module V = Value

let rec eval_let env { let_var; let_value } =
  let value = expr env let_value in
  value, Rt_env.extend_name env let_var value

and expr env e =
  match e.expr_desc with
  | Unit -> V.Unit
  | Wrapped e -> expr env e
  | Ctor c ->
    let args = match c.ctor_arguments with
      | None -> None
      | Some args -> Some (List.map (expr env) args)
    in V.Ctor { c with ctor_arguments = args }
  | Literal l -> V.Literal l
  | Application { callee; generic_arguments; arguments; generic_arguments_ty } ->
      let callee' = expr env callee in
      let arguments = match arguments with
        | None -> []
        | Some a -> List.map (expr env) a
      in
      begin match callee' with
      | V.Builtin (_, fn) -> fn env arguments
      | _ ->
        let arguments' = List.map V.expr_of_value arguments in
        fst @@ stmt env (Subst.subst generic_arguments_ty arguments' callee')
      end
  | Var v -> begin
      try Rt_env.find_name v env
      with Not_found ->
        error (Unbound_variable v)
      end
  | Record r ->
    V.Record (List.map (fun (n, v) -> (n.str, expr env v)) r)
  | Field_access f ->
    let value = expr env f.record in
    begin match value with
      | V.Record fields ->
        Rt_env.find_name [f.field] fields
      | _ -> assert false
    end
  | Match m -> eval_match env m
  | Binop bin -> expr env ({ expr_loc = e.expr_loc; expr_desc = Application (app_of_binop bin) })
  | Function ({ fn_name; fn_parameters; fn_body } as fn) -> V.Function fn
  | If if_ -> eval_if env if_

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
  match pattern.pat_desc, value with
  | Pany, _ -> true
  | Pvar _, _ -> true
  | Pctor (name, _), V.Ctor { ctor_name } when name = ctor_name -> true
  | _ -> false

and eval_case env value p =
  let env' = pattern env p.pattern value in
  let value, _ = stmt env' (List.hd @@ List.rev @@ p.case_value) in
  value

and pattern env pat value =
  match pat.pat_desc, value with
  | Pany, _ -> env
  | Pvar x, v -> Rt_env.extend_name env x v
  | Pctor (_, ps), V.Ctor { ctor_arguments } ->
    begin match ps, ctor_arguments with
    | None, None -> env
    | Some patterns, Some args -> List.fold_left2 pattern env patterns args
    | _ -> assert false
    end
  | _ -> assert false

and eval_intf_item intf_name env = function
  | Prototype { proto_name }
  | OperatorPrototype { oproto_name = proto_name } ->
    Hashtbl.add Rt_env.fn_to_intf proto_name.str intf_name.str;
    Rt_env.extend_name env proto_name (V.InterfaceFunction proto_name.str)

and eval_impl_item = function
  | ImplOperator op ->
    (op.op_name.str, fn_of_operator op)
  | ImplFunction impl_fn ->
    match impl_fn.fn_name with
    | Some n -> (n.str, impl_fn)
    | None -> assert false

and stmt env stmt =
  match stmt.stmt_desc with
  | Expr e -> expr env e, env
  | Let l -> eval_let env l
  | FunctionStmt fn ->
    let fn' = expr env { expr_loc = stmt.stmt_loc; expr_desc = (Function fn) } in
    match fn.fn_name with
    | Some name -> fn', Rt_env.extend_name env name fn'
    | None -> assert false

and stmts env stmts =
  List.fold_left (fun (_, env) s -> stmt env s) (V.Unit, env) stmts

and decl env decl =
  match decl.decl_desc with
  | Stmt s -> stmt env s
  | Enum { enum_name } -> (V.Type enum_name.str, env)
  | TypeAlias { ta_name } -> (V.Type ta_name.str, env)
  | Interface { intf_name; intf_items } ->
    Hashtbl.add Rt_env.intf_to_impls intf_name.str (ref []);
    let env' = List.fold_left (eval_intf_item intf_name) env intf_items in
    (V.Unit, env')
  | Operator op ->
    let value = expr env ({ expr_loc = dummy_loc; expr_desc = Function (fn_of_operator op) }) in
    V.Unit, Rt_env.extend_name env op.op_name value

  | Implementation { impl_name; impl_arg_type; impl_items } ->
      let ty = match impl_arg_type with
        | Some t -> t
        | None -> assert false
      in
      let impls = Hashtbl.find Rt_env.intf_to_impls (Rt_env.last impl_name).str in
      impls := (ty, List.map eval_impl_item impl_items) :: !impls;
      (V.Unit, env)

let eval { body } =
  List.fold_left (fun (_, env) node -> decl env node) (V.Unit, Rt_env.default_env) body
  |> fst
