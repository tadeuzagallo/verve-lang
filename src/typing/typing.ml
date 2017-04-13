module T = Types
open Absyn

exception TypeError of string

type ty_env = (name * T.ty) list
let ty_int = T.Const "Int"
let ty_unit = T.Const "Unit"

let default_env = [
  ("Type", T.Type);
  ("Int", ty_int);
  ("Void", ty_unit);
]

let unify t1 t2 = ()

let check_literal = function
  | Int _ -> ty_int

let get_type env v =
  try List.assoc v env
  with Not_found ->
    raise (TypeError "Unknown Type")

let rec check_type env : type_ -> T.ty * ty_env = function
  | Var _ -> assert false (* TODO *)
  | Con t -> (get_type env t, env)
  | Arrow (parameters, return_type) ->
      let (ret, _) = check_type env return_type in
      let fn_type = List.fold_right
        (fun p t ->
          let (ty, _) = check_type env p in
          T.Arrow (ty , t))
        parameters ret
      in (fn_type, env) 

let rec check_fn env { name; parameters; return_type; body } =
  let (ret_type, _) = check_type env return_type in
  let (fn_type, env') = List.fold_right
    (fun p (t, env') ->
      let (ty, _) = check_type env p.type_ in
      (T.Arrow (ty , t), (p.name, ty)::env'))
    parameters (ret_type, env)
  in
  let (ret, _) = check_exprs env' body in
  unify ret ret_type;
  match name with
  | Some n -> (fn_type, (n, fn_type)::env)
  | None -> (fn_type, env)

and check_app env { callee; arguments } =
  let (ty_callee, _) = check_expr env callee in
  let check call argument =
    let (ty_arg, _) = check_expr env argument in
    match call with
    | T.Arrow (t1, t2) ->
        unify t1 ty_arg;
        t2
    | _ -> raise (TypeError "Invalid type for function call")
  in
  (List.fold_left check ty_callee arguments, env)

and check_var env v =
  (get_type env v, env)

and check_expr env : expr -> T.ty * ty_env = function
  | Unit -> (ty_unit, env)
  | Literal l -> (check_literal l, env)
  | Function fn -> check_fn env fn
  | Application app -> check_app env app
  | Var v -> check_var env v

and check_exprs env exprs =
  List.fold_left
    (fun (_, env) node -> check_expr env node)
    (ty_unit, env) exprs

let check program = check_exprs default_env program.body
