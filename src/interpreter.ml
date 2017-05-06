open Absyn

module V = Value
module T = Types

exception Unbound_variable of string
exception Runtime_error of string

let int_op op env = function
  | [V.Literal (Int x); V.Literal (Int y)] ->
    V.Literal (Int (op x y)), env
  | _ -> assert false

let add_builtin name fn env =
  (name, V.Builtin (name, fn)) :: env

let default_env = []
  |> add_builtin "int_add" (int_op ( + ))
  |> add_builtin "int_sub" (int_op ( - ))
  |> add_builtin "int_mul" (int_op ( * ))
  |> add_builtin "int_div" (int_op ( / ))

let fn_to_intf = Hashtbl.create 256
let intf_to_impls = Hashtbl.create 64

let rec combine (subst, params) : parameter list * expr list -> 'a * 'b = function
  | x::xs, y::ys ->
      combine ((x.param_name,y)::subst, params) (xs, ys)
  | x::xs, [] ->
      combine (subst, x::params) (xs, [])
  | [], [] ->
      (List.rev subst, List.rev params)
  | [], _ ->
      raise (Runtime_error "Function applied to too many arguments")

let rec combine_ty subst = function
  | x::xs, y::ys ->
      combine_ty ((x.name, y)::subst) (xs, ys)
  | _, _ ->
      List.rev subst

let rec subst_ty ty_args = function
  | T.Arrow (t1, t2) ->
      T.Arrow (subst_ty ty_args t1, subst_ty ty_args t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (subst_ty ty_args) types in
      T.TypeCtor (name, types')
  | (T.RigidVar { T.name } as var)
  | (T.Var { T.name } as var) ->
      begin try List.assoc name ty_args
      with Not_found -> var
      end
  | T.TypeArrow (v1, t2) ->
      let ty_args' = List.remove_assoc v1.T.name ty_args in
      T.TypeArrow (v1, subst_ty ty_args' t2)
  | T.Record r ->
    let aux (n, t) =
      (n, subst_ty ty_args t)
    in T.Record (List.map aux r)
  | t -> t


let rec subst_expr ty_args args = function
  | Unit -> Unit
  | Literal l -> Literal l
  | Wrapped expr -> subst_expr ty_args args expr
  | Var x as v -> begin
      try List.assoc x args
      with Not_found -> v
  end
  | Application ({ callee; arguments; generic_arguments_ty } as app) ->
      let callee = subst_expr ty_args args callee in
      let arguments = match arguments with
        | None -> None
        | Some a -> Some (List.map (subst_expr ty_args args) a)
      in
      let generic_arguments_ty = List.map (subst_ty ty_args) generic_arguments_ty in
      Application { app with callee; arguments; generic_arguments_ty }

  | Function ({ fn_parameters; fn_body } as fn) ->
      let filter (x, _) =  not (List.exists (fun p -> p.param_name = x) fn_parameters) in
      let args' = List.filter filter args in
      let body = List.map (subst_expr ty_args args') fn_body in
      Function { fn with fn_body=body }
  | Ctor ({ ctor_arguments } as ctor) ->
      let ctor_arguments = match ctor_arguments with
        | None -> None
        | Some cargs -> Some (List.map (subst_expr ty_args args) cargs)
      in Ctor { ctor with ctor_arguments }
  | Record r ->
    let aux (name, v) =
      (name, subst_expr ty_args args v)
    in
    let r' = List.map aux r in
    Record r'
  | Field_access f ->
    Field_access { f with record=subst_expr ty_args args f.record }
  | Binop op ->
    Binop { op with
      bin_lhs = subst_expr ty_args args op.bin_lhs;
      bin_rhs = subst_expr ty_args args op.bin_rhs;
      bin_generic_arguments_ty = List.map (subst_ty ty_args) op.bin_generic_arguments_ty;
    }
  | Match m ->
    let subst_case c =
      { c with case_value = List.map (subst_expr ty_args args) c.case_value }
    in Match {
      match_value = subst_expr ty_args args m.match_value;
      cases = List.map subst_case m.cases;
    }

  | Let l ->
      Let { l with
        let_value = subst_expr ty_args args l.let_value;
      }

let subst generics arguments fn =
  let { fn_parameters; fn_body } as fn = match fn with
    | V.Function f -> f
    | V.InterfaceFunction fn ->
        begin match generics with
        | [t] ->
          let rec get_type = function
            | T.Var { T.resolved_ty = (Some t) } -> get_type t
            | t -> t
          in
          let intf = Hashtbl.find fn_to_intf fn in
          let impls = Hashtbl.find intf_to_impls intf in
          let t' = get_type t in
          let impl = List.assoc t' !impls in
          (List.assoc fn impl)
        | _ -> assert false
        end
    | v ->
        Printer.Value.pp Format.err_formatter v;
        print_newline ();
        assert false
  in
  let subst, params = combine ([], []) (fn_parameters, arguments) in
  let subst_ty = combine_ty [] (fn.fn_generics, generics) in
  let body' = List.rev_map (subst_expr subst_ty subst) fn_body in
  match params, body' with
  | [], [] -> Unit
  | [], _ -> List.hd body'
  | _ -> Function { fn with fn_parameters = params; fn_body = body' }

let rec eval_let env { let_var; let_value } =
  let value, env = eval_expr env let_value in
  value, (let_var, value) :: env

and eval_expr env = function
  | Unit -> (V.Unit, env)
  | Wrapped expr -> eval_expr env expr
  | Ctor c ->
    let args = match c.ctor_arguments with
      | None -> None
      | Some args -> Some (List.map (fun e -> fst @@ eval_expr env e) args)
    in (V.Ctor { c with ctor_arguments = args }, env)
  | Literal l -> (V.Literal l, env)
  | Application { callee; generic_arguments; arguments; generic_arguments_ty } ->
      let (callee', _) = eval_expr env callee in
      let arguments = match arguments with
        | None -> []
        | Some a -> List.map (fun a -> fst @@ eval_expr env a) a
      in
      begin match callee' with
      | V.Builtin (_, fn) -> fn env arguments
      | _ ->
        let arguments' = List.map V.expr_of_value arguments in
        let (v, _) = eval_expr env (subst generic_arguments_ty arguments' callee') in
        (v, env)
      end
  | Var v -> begin
      try (List.assoc v env, env)
      with Not_found ->
        raise (Unbound_variable v)
      end
  | Record r ->
    V.Record (List.map (fun (n, v) -> (n, fst @@ eval_expr env v)) r), env
  | Field_access f ->
    let value, _ = eval_expr env f.record in
    begin match value with
      | V.Record fields ->
        List.assoc f.field fields, env
      | _ -> assert false
    end
  | Match m -> eval_match env m
  | Operator op ->
    let _, env' = eval_expr env (Function (fn_of_operator op)) in
    V.Unit, env'
  | Binop bin -> eval_expr env (Application (app_of_binop bin))
  | Let l -> eval_let env l
  | Function ({ fn_name; fn_parameters; fn_body } as fn) ->
      let fn' = V.Function fn in
      match fn_name with
      | Some n -> (fn', (n, fn')::env)
      | None -> (fn', env)

and eval_match env { match_value; cases } =
  let v, _ = eval_expr env match_value in
  let matched_case = List.find (matched_case env v) cases in
  eval_case env v matched_case

and matched_case env value { pattern } =
  match pattern, value with
  | Pany, _ -> true
  | Pvar _, _ -> true
  | Pctor (name, _), V.Ctor { ctor_name } when name = ctor_name -> true
  | _ -> false

and eval_case env value { pattern; case_value } =
  let env' = eval_pattern env pattern value in
  let value, _ = eval_expr env' (List.hd @@ List.rev @@ case_value) in
  value, env

and eval_pattern env pattern value =
  match pattern, value with
  | Pany, _ -> env
  | Pvar x, v -> (x, v) :: env
  | Pctor (_, ps), V.Ctor { ctor_arguments } ->
    begin match ps, ctor_arguments with
    | None, None -> env
    | Some patterns, Some args -> List.fold_left2 eval_pattern env patterns args
    | _ -> assert false
    end
  | _ -> assert false

and eval_intf_item intf_name env = function
  | Prototype { proto_name }
  | OperatorPrototype { oproto_name = proto_name } ->
    Hashtbl.add fn_to_intf proto_name intf_name;
    (proto_name, V.InterfaceFunction proto_name)::env

and eval_impl_item = function
  | ImplOperator op ->
    (op.op_name, fn_of_operator op)
  | ImplFunction impl_fn ->
    match impl_fn.fn_name with
    | Some n -> (n, impl_fn)
    | None -> assert false

and eval_decl env = function
  | Expr expr -> eval_expr env expr
  | Enum { enum_name } -> (V.Type enum_name, env)
  | Interface { intf_name; intf_items } ->
    Hashtbl.add intf_to_impls intf_name (ref []);
    let env' = List.fold_left (eval_intf_item intf_name) env intf_items in
    (V.Unit, env')

  | Implementation { impl_name; impl_arg_type; impl_items } ->
      let ty = match impl_arg_type with
        | Some t -> t
        | None -> assert false
      in
      let impls = Hashtbl.find intf_to_impls impl_name in
      impls := (ty, List.map eval_impl_item impl_items) :: !impls;
      (V.Unit, env)

let eval { body } =
  List.fold_left (fun (_, env) node -> eval_decl env node) (V.Unit, default_env) body
  |> fst
