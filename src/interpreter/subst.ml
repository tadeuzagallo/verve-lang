open Absyn
open Runtime_error

module T = Types
module V = Value

module S_env = struct
  type t = {
    expr_args : (string * expr) list;
    type_args : (string * T.texpr) list;
  }

  let of_substs expr_args type_args =
    { expr_args; type_args }

  let last name = List.nth name (List.length name - 1)

  let find_expr name env =
    List.assoc (last name).str env.expr_args

  let remove_exprs names env =
    let aux env name =
      List.remove_assoc name.str env
    in
    let expr_args = List.fold_left aux env.expr_args names in
    { env with expr_args }

  let find_type name env =
    List.assoc name env.type_args

  let remove_type name env =
    let type_args = List.remove_assoc name env.type_args in
    { env with type_args }

end

let option fn env = function
  | None -> None
  | Some v' -> Some (fn env v')

let list fn env vs = List.map (fn env) vs

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
let rec subst_ty env t =
  match T.desc t with
  | T.Arrow (t1, t2) ->
      T._texpr @@ T.Arrow (subst_ty env t1, subst_ty env t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (subst_ty env) types in
      T._texpr @@ T.TypeCtor (name, types')
  | (T.RigidVar { T.name } as var)
  | (T.Var { T.name } as var) ->
      begin try S_env.find_type name env
      with Not_found -> T._texpr var
      end
  | T.TypeArrow (v1, t2) ->
    let env =
      match (T.desc v1) with
      | T.Var { T.name }
      | T.RigidVar { T.name } ->
        S_env.remove_type name env
      | _ -> env
    in
    subst_ty env t2
  | T.Record r ->
    let aux (n, t) =
      (n, subst_ty env t)
    in T._texpr @@ T.Record (List.map aux r)
  | t -> T._texpr t

(* Expr *)
let rec subst_expr env expr =
  let mk_expr e = { expr with expr_desc = e } in
  match expr.expr_desc with
  | Unit -> expr
  | Literal l -> expr
  | Wrapped expr -> subst_expr env expr
  | Var v -> subst_var env v expr
  | Application app -> mk_expr (Application (subst_app env app))
  | Function fn -> mk_expr (Function (subst_fn env fn))
  | Ctor ctor -> mk_expr (Ctor (subst_ctor env ctor))
  | Record r -> mk_expr (Record (subst_record env r))
  | Field_access f -> mk_expr (Field_access (subst_field_access env f))
  | Binop op -> mk_expr (Binop (subst_binop env op))
  | Match m -> mk_expr (Match (subst_match env m))

and subst_var env var expr =
  try S_env.find_expr var env
  with Not_found -> expr

and subst_fn env fn =
  let param_names = List.map (fun p -> p.param_name) fn.fn_parameters in
  let names = match fn.fn_name with
    | None -> param_names
    | Some n -> n :: param_names
  in
  let env' = S_env.remove_exprs names env in
  let fn_body, _ = subst_stmts env' fn.fn_body in
  { fn with fn_body }

and subst_ctor env ctor =
  let ctor_arguments = (option @@ list subst_expr) env ctor.ctor_arguments in
  { ctor with ctor_arguments }

and subst_record env r =
  let aux (name, v) =
    (name, subst_expr env v)
  in
  List.map aux r

and subst_field_access env f =
  let record = subst_expr env f.record in
  { f with record }

and subst_binop env op =
  let bin_lhs = subst_expr env op.bin_lhs in
  let bin_rhs = subst_expr env op.bin_rhs in
  let bin_generic_arguments_ty = List.map (subst_ty env) op.bin_generic_arguments_ty in
  { op with bin_lhs; bin_rhs; bin_generic_arguments_ty }

and subst_match env m =
  let match_value = subst_expr env m.match_value in
  let cases = List.map (subst_case env) m.cases in
  { match_value; cases }

and subst_case env c =
  let case_value, _ = subst_stmts env c.case_value in
  { c with case_value }

and subst_app env app =
  let callee = subst_expr env app.callee in
  let arguments = (option @@ list subst_expr) env app.arguments in
  let generic_arguments_ty = List.map (subst_ty env) app.generic_arguments_ty in
  { app with callee; arguments; generic_arguments_ty }

(* Stmt *)
and subst_stmts env stmts =
  let aux (stmts, env) stmt =
    let stmt, env = subst_stmt env stmt in
    stmt :: stmts, env
  in
  let stmts, env = List.fold_left aux ([], env) stmts in
  List.rev stmts, env

and subst_stmt env stmt =
  let mk_stmt s = { stmt with stmt_desc = s } in
  match stmt.stmt_desc with
  | Expr expr ->
    mk_stmt (Expr (subst_expr env expr)), env
  | Let l ->
    let l, env = subst_let env l in
    mk_stmt (Let l), env
  | FunctionStmt fn ->
    let fn, env = subst_fn_stmt env fn in
    mk_stmt (FunctionStmt fn), env

and subst_let env let_ =
  let value = subst_expr env let_.let_value in
  let env = S_env.remove_exprs [let_.let_var] env in
  { let_ with let_value = value }, env

and subst_fn_stmt env fn =
  let fn = subst_fn env fn in
  let env = match fn.fn_name with
    | None -> assert false
    | Some n -> S_env.remove_exprs [n] env
  in
  fn, env

(* entry point *)

let fn_of_value generics = function
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

let subst generics arguments fn =
  let fn = fn_of_value generics fn in
  let subst, params = combine ([], []) (fn.fn_parameters, arguments) in
  let subst_ty = combine_ty [] (fn.fn_generics, generics) in
  let env = S_env.of_substs subst subst_ty in
  let body, _ = subst_stmts env fn.fn_body in
  let mk_stmt stmt_desc = { stmt_loc = dummy_loc; stmt_desc } in
  match params, body with
  | [], [] -> [mk_stmt @@ Expr { expr_loc = dummy_loc; expr_desc = Unit }]
  | [], _ -> body
  | _ -> [mk_stmt @@ Expr { expr_loc = dummy_loc; expr_desc = Function { fn with fn_parameters = params; fn_body = body } }]
