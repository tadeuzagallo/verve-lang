open Type_error

module A = Absyn
module T = Types

type t = {
  types: (string * T.texpr) list;
  values: (string * T.texpr) list;
  ctors: (string * T.texpr) list;
  modules: (string * t) list;
}

let empty = {
  types = [];
  values = [];
  ctors = [];
  modules = [];
}
let extend env (x, t) = (x, t)::env
let merge e1 e2 = {
  types = e1.types @ e2.types;
  values = e1.values @ e2.values;
  ctors = e1.ctors @ e2.ctors;
  modules = e1.modules @ e2.modules;
}

(* Type variable helpers*)

let link_ty t1 t2 =
  (T.repr t1).T.texpr <- T.Link t2

let _fresh_tbl = Hashtbl.create 256
let _default_id = 1
let _fresh name =
  try
    let type_id = Hashtbl.find _fresh_tbl name in
    incr type_id;
    !type_id
  with Not_found ->
    Hashtbl.add _fresh_tbl name (ref _default_id);
    _default_id

let fresh t =
  match T.desc t with
  | T.Var var ->
    T.var { var with T.id = _fresh var.T.name }
  | _ -> t

let make_var () =
  let name = "𝜏" in
  T.var { T.id = _fresh name; T.name; T.constraints = []; }

(* Types of builtin types *)
let ty_int = T.type_ctor ("Int", [])
let ty_bool = T.type_ctor ("Bool", [])
let ty_type = T.type_ctor ("Type", [])
let ty_void = T.type_ctor ("Void", [])
let ty_string = T.type_ctor ("String", [])

(* Values of builtin types *)

let binop ty = T.arrow ty (T.arrow ty ty)

let default_env = {
  types  = [
    ("Type", ty_type);
    ("Int", ty_int);
    ("String", ty_string);
    ("Bool", ty_bool);
  ];
  values = [
    ("int_add", binop ty_int);
    ("int_sub", binop ty_int);
    ("int_mul", binop ty_int);
    ("int_div", binop ty_int);
  ];
  ctors = [
    ("True", ty_bool);
    ("False", ty_bool);
  ];
  modules = [];
}

let rec find var = function
  | [] -> raise Not_found
  | (k, v) :: _ when k.T.name = var.T.name && k.T.id == var.T.id -> v
  | _ :: rest -> find var rest

let map_type fn t =
  let t = T.repr t in
  let desc = match T.desc t with
  | T.TypeCtor (n, ts) -> T.TypeCtor (n, List.map fn ts)
  | T.Arrow (t1, t2) -> T.Arrow (fn t1, fn t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (fn var, fn ty)
  | T.Record r -> T.Record (List.map (fun (n, t) -> n, fn t) r)
  | t -> t
  in T._texpr desc

let loosen t =
  let rec loosen s t =
    let t = T.repr t in
    try List.assoc t s with Not_found ->
    match T.desc t with
    | T.RigidVar var -> T.var var
    | T.TypeArrow (var, ty) ->
      let var' = loosen s var in
      let ty' = loosen ((var, var') :: s) ty in
      T.type_arrow var' ty'
    | _ -> map_type (loosen s) t
  in loosen [] t

let instantiate t =
  let rec instantiate s t =
    let t = T.repr t in
    try List.assoc t s with Not_found ->
    match T.desc t with
    | T.TypeArrow (var, ty) ->
      let var' = fresh var in
      let ty' = instantiate ((var, var') :: s) ty in
      T.type_arrow var' ty'
    | _ -> map_type (instantiate s) t
  in instantiate [] t

(* getters and setters *)
let rec find name env proj =
  match name with
  | [] -> assert false
  | [x] -> List.assoc x.A.str (proj env)
  | x :: y :: rest ->
    let env' = find_module env x in
    find (y :: rest) env' proj

and find_module env name =
  try List.assoc name.A.str env.modules
  with Not_found ->
    raise (Error (Unknown_module name))

let add_module env name mod_ =
  { env with modules = extend env.modules (name.A.str, mod_) }

let add_type env name ty =
  { env with types = extend env.types (name.A.str, ty) }

let find_type env v =
  try find v env (fun env -> env.types)
  with Not_found ->
    raise (Error (Unknown_type v))

let add_ctor env name ctor =
  { env with ctors = extend env.ctors (name.A.str, ctor) }

let find_ctor env name =
  try instantiate (find name env (fun env -> env.ctors))
  with Not_found ->
    raise (Error (Unknown_ctor name))

let add_value env name value =
  { env with values = extend env.values (name.A.str, value) }

let find_value env name =
  try instantiate (find name env (fun env -> env.values))
  with Not_found ->
    raise (Error (Unknown_value name))

let var_of_generic env { A.name; A.constraints } =
  let resolve n =
    let t = find_type env n in
    match T.desc t with
    | T.Interface i -> i
    | _ -> raise (Error (Invalid_constraint (name, t)))
  in
  let intfs = List.map resolve constraints in
  { T.id = _fresh name.A.str; T.name = name.A.str; T.constraints = intfs; }

(* Unification *)

let check_implementations t intf_desc =
  match T.desc t with
  | T.Var var
  | T.RigidVar var ->
      if not (List.mem intf_desc var.T.constraints) then
        raise (Error (Instance_not_found (t, intf_desc)))
  | _ ->
      if not (List.mem_assoc t intf_desc.T.intf_impls) then
        raise (Error (Instance_not_found (t, intf_desc)))

let rec unify ~expected:t1 t2 =
  let t1 = T.repr t1 in
  let t2 = T.repr t2 in
  if t1 == t2 then () else
  match T.desc t1, T.desc t2 with
  | T.RigidVar v1, T.RigidVar v2 when v1 = v2 -> ()

  | T.TypeCtor (n2, t2s), T.TypeCtor (n1, t1s)
  when n1 = n2 ->
      let aux t1 t2 = unify ~expected:t1 t2 in
      List.iter2 aux t1s t2s

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      unify ~expected:t11 t21;
      unify ~expected:t12 t22

  (* Order matters: Var must come before TypeArrow *)
  | T.Var { T.constraints }, _ ->
      List.iter (check_implementations t2) constraints;
      link_ty t1 t2

  | _, T.Var { T.constraints } ->
      List.iter (check_implementations t1) constraints;
      link_ty t2 t1

  | T.TypeArrow (_, t1), _ ->
      unify ~expected:t1 t2

  | _, T.TypeArrow (_, t2) ->
      unify ~expected:t1 t2

  | T.Record r1, T.Record r2 ->
    let validate (field, t1') =
      try
        unify ~expected:t1' (List.assoc field r2)
      with Not_found ->
        raise (Error (Unification_error (t1, t2)))
    in List.iter validate r1

  | _, _ ->
    raise (Error (Unification_error (t1, t2)))
