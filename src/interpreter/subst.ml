open Absyn
open Runtime_error

module T = Types
module V = Value

(* Combine *)
let rec combine (subst, params) : parameter list * expr list -> 'a * 'b = function
  | x::xs, y::ys ->
      combine ((x.param_name.str, y)::subst, params) (xs, ys)
  | x::xs, [] ->
      combine (subst, x::params) (xs, [])
  | [], [] ->
      (List.rev subst, List.rev params)
  | [], _ ->
    error (Unknown "Function applied to too many arguments") 

let rec combine_ty subst = function
  | x::xs, y::ys ->
      combine_ty ((x.name.str, y)::subst) (xs, ys)
  | _, _ ->
      List.rev subst

(* Types *)
let rec subst_ty ty_args t = match T.desc t with
  | T.Arrow (t1, t2) ->
      T._texpr @@ T.Arrow (subst_ty ty_args t1, subst_ty ty_args t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (subst_ty ty_args) types in
      T._texpr @@ T.TypeCtor (name, types')
  | (T.RigidVar { T.name } as var)
  | (T.Var { T.name } as var) ->
      begin try List.assoc name ty_args
      with Not_found -> T._texpr var
      end
  | T.TypeArrow (v1, t2) ->
    let ty_args =
      match (T.desc v1) with
      | T.Var { T.name }
      | T.RigidVar { T.name } ->
        List.remove_assoc name ty_args
      | _ -> ty_args
    in
    subst_ty ty_args t2
  | T.Record r ->
    let aux (n, t) =
      (n, subst_ty ty_args t)
    in T._texpr @@ T.Record (List.map aux r)
  | t -> T._texpr t

(* Expr *)

let rec subst_expr ty_args args expr : expr =
  let desc = match expr.expr_desc with
  | Unit -> Unit
  | Literal l -> Literal l
  | Wrapped expr -> (subst_expr ty_args args expr).expr_desc
  | Var x as v -> begin
      try (Rt_env.find_name x args).expr_desc
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
      let filter (x, _) =  not (List.exists (fun p -> p.param_name.str = x) fn_parameters) in
      let args' = List.filter filter args in
      let body = List.map (subst_stmt ty_args args') fn_body in
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
      { c with case_value = List.map (subst_stmt ty_args args) c.case_value }
    in Match {
      match_value = subst_expr ty_args args m.match_value;
      cases = List.map subst_case m.cases;
    }
  in { expr with expr_desc = desc }

(* Stmt *)
and subst_stmt ty_args args stmt =
  let desc = match stmt.stmt_desc with
  | Expr expr -> Expr (subst_expr ty_args args expr)
  | Let l -> Let { l with
      let_value = subst_expr ty_args args l.let_value;
    }
  | FunctionStmt fn ->
    let expr = { expr_loc = stmt.stmt_loc; expr_desc = Function fn } in
    match subst_expr ty_args args expr with
    | { expr_desc = Function fn } -> FunctionStmt fn
    | _ -> assert false
  in { stmt with stmt_desc = desc }

(* entry point *)

let subst generics arguments fn =
  let { fn_parameters; fn_body } as fn = match fn with
    | V.Function f -> f
    | V.InterfaceFunction fn ->
        begin match generics with
        | [t] ->
          let intf = Hashtbl.find Rt_env.fn_to_intf fn in
          let impls = Hashtbl.find Rt_env.intf_to_impls intf in
          let impl = List.find (fun (t', _) -> Env.eq_type (T.repr t) t') !impls |> snd in
          (List.assoc fn impl)
        | _ -> assert false
        end
    | v ->
        Printer.Value.dump v;
        assert false
  in
  let subst, params = combine ([], []) (fn_parameters, arguments) in
  let subst_ty = combine_ty [] (fn.fn_generics, generics) in
  let body' = List.rev_map (subst_stmt subst_ty subst) fn_body in
  let stmt_desc = match params, body' with
  | [], [] -> Expr { expr_loc = dummy_loc; expr_desc = Unit }
  | [], _ -> (List.hd body').stmt_desc
  | _ -> Expr { expr_loc = dummy_loc; expr_desc = Function { fn with fn_parameters = params; fn_body = body' } }
  in { stmt_loc = dummy_loc; stmt_desc }
