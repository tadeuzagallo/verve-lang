open Absyn

module V = Value
module T = Types

exception Unbound_variable
exception Runtime_error of string

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
          let impl = List.assoc (get_type t) !impls in
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

let rec eval_expr env = function
  | Unit -> (V.Unit, env)
  | Ctor c -> (V.Ctor c, env)
  | Literal l -> (V.Literal l, env)
  | Application { callee; generic_arguments; arguments; generic_arguments_ty } ->
      let (callee', _) = eval_expr env callee in
      let arguments' = match arguments with
        | None -> []
        | Some a -> List.map (fun a -> V.expr_of_value @@ fst @@ eval_expr env a) a
      in
      let (v, _) = eval_expr env (subst generic_arguments_ty arguments' callee') in
      (v, env)
  | Var v -> begin
      try (List.assoc v env, env)
      with Not_found ->
        raise Unbound_variable
      end
  | Record r ->
    V.Record (List.map (fun (n, v) -> (n, fst @@ eval_expr env v)) r), env
  | Field_access f ->
    begin match f.record with
      | Record fields ->
        eval_expr env (List.assoc f.field fields)
      | _ -> assert false
    end
  | Function ({ fn_name; fn_parameters; fn_body } as fn) ->
      let fn' = V.Function fn in
      match fn_name with
      | Some n -> (fn', (n, fn')::env)
      | None -> (fn', env)

and eval_decl env = function
  | Expr expr -> eval_expr env expr
  | Enum { enum_name } -> (V.Type enum_name, env)
  | Interface { intf_name; intf_functions } ->
      let aux { proto_name } =
        Hashtbl.add fn_to_intf proto_name intf_name
      in List.iter aux intf_functions;
      Hashtbl.add intf_to_impls intf_name (ref []);
      let aux env { proto_name } =
        (proto_name, V.InterfaceFunction proto_name)::env
      in
      let env' = List.fold_left aux env intf_functions in
      (V.Unit, env')

  | Implementation { impl_name; impl_arg_type; impl_functions } ->
      let aux impl_fn =
        let fn_name =
          match impl_fn.fn_name with
          | Some n -> n
          | None -> assert false
        in (fn_name, impl_fn)
      in
      let ty = match impl_arg_type with
        | Some t -> t
        | None -> assert false
      in
      let impls = Hashtbl.find intf_to_impls impl_name in
      impls := (ty, List.map aux impl_functions) :: !impls;
      (V.Unit, env)

let eval { body } =
  List.fold_left (fun (_, env) node -> eval_decl env node) (V.Unit, []) body
  |> fst
