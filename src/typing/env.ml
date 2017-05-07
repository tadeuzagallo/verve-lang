open Type_error

module A = Absyn
module T = Types

type t = (A.name * T.texpr) list

let empty = []
let extend env (x, t) = (x, t)::env
let merge e1 e2 = e1 @ e2

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
  | T.RigidVar var ->
    T.rigid_var { var with T.id = _fresh var.T.name }
  | _ -> assert false

let make_var () =
  let name = "𝜏" in
  T.var { T.id = _fresh name; T.name; T.constraints = []; }

(* Types of builtin types *)
let ty_int = T.type_ctor ("Int", [])
let ty_type = T.type_ctor ("Type", [])
let ty_void = T.type_ctor ("Void", [])
let ty_string = T.type_ctor ("String", [])

(* Values of builtin types *)
let val_int = T.type_inst ("Int", [])
let val_type = T.type_inst ("Type", [])
let val_void = T.type_inst ("Void", [])
let val_string = T.type_inst ("String", [])

let binop ty = T.arrow ty (T.arrow ty ty)

let default_env = [
  ("Type", ty_type);
  ("Int", ty_int);
  ("Void", ty_void);
  ("String", ty_string);
  ("int_add", binop val_int);
  ("int_sub", binop val_int);
  ("int_mul", binop val_int);
  ("int_div", binop val_int);
]

let rec find var = function
  | [] -> raise Not_found
  | (k, v) :: _ when k.T.name = var.T.name && k.T.id == var.T.id -> v
  | _ :: rest -> find var rest

let map_type fn t =
  let t = T.repr t in
  let desc = match T.desc t with
  | T.TypeCtor (n, ts) -> T.TypeCtor (n, List.map fn ts)
  | T.TypeInst (n, ts) -> T.TypeInst (n, List.map fn ts)
  | T.Arrow (t1, t2) -> T.Arrow (fn t1, fn t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (fn var, fn ty)
  | T.Record r -> T.Record (List.map (fun (n, t) -> n, fn t) r)
  | t -> t
  in T._texpr desc

let rec to_value t =
  let t = T.repr t in
  match T.desc t with
  | T.TypeCtor (n, ts) -> T.type_inst (n, List.map (map_type to_value) ts)
  | _ -> map_type to_value t

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

let get_type env v =
  try instantiate (List.assoc v env);
  with Not_found ->
    raise (Error (Unknown_type v))

let var_of_generic env { A.name; A.constraints } =
  let resolve n =
    let t = get_type env n in
    match T.desc t with
    | T.Interface i -> i
    | _ -> raise (Error (Invalid_constraint (name, t)))
  in
  let intfs = List.map resolve constraints in
  { T.id = _fresh name; T.name; T.constraints = intfs; }

let ctor_marker = "mk#"
let add_ctor env (name, ty) =
  extend env (ctor_marker ^ name, ty)

let get_ctor env name =
  try get_type env (ctor_marker ^ name)
  with Error (Unknown_type _) ->
    raise (Error (Unknown_ctor name))

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

  | T.TypeInst (n2, t2s), T.TypeInst (n1, t1s)
  when n1 = n2 ->
      let aux t1 t2 = unify ~expected:t1 t2 in
      List.iter2 aux t1s t2s

  | T.TypeCtor _, _ when t2 = val_type -> ()
  | _, T.TypeCtor _ when t1 = val_type -> ()

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      unify ~expected:t11 t21;
      unify ~expected:t12 t22

  (* Order matters: Var must come before TypeArrow *)
  | T.Var { T.constraints = cs1 },
    T.Var ({ T.constraints = cs2 } as var2) ->
      (*let aux c1 c2 = String.compare c1.T.intf_name c2.T.intf_name in*)
      (*let ty = T.var { var2 with T.constraints = List.sort_uniq aux (cs1 @ cs2)} in*)
      link_ty t1 t2

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
