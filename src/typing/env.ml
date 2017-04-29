open Type_error

module A = Absyn
module T = Types

type ty_env = (A.name * T.ty) list
type subst = (T.tvar * T.ty) list

let extend_env env (x, t) = (x, t)::env

(* Types of builtin types *)
let ty_int = T.TypeCtor ("Int", [])
let ty_type = T.TypeCtor ("Type", [])
let ty_void = T.TypeCtor ("Void", [])

(* Values of builtin types *)
let val_int = T.TypeInst ("Int", [])
let val_type = T.TypeInst ("Type", [])
let val_void = T.TypeInst ("Void", [])

let default_env = [
  ("Type", ty_type);
  ("Int", ty_int);
  ("Void", ty_void);
]

let rec find var = function
  | [] -> raise Not_found
  | (k, v) :: _ when k.T.name = var.T.name && k.T.id == var.T.id -> v
  | _ :: rest -> find var rest

let (>>) s1 s2 =
  let apply s1 s2 =
    let aux (k, v) =
      try (k, find k s1)
      with Not_found -> (k, v)
    in List.map aux s2
  in apply s1 s2 @ s1

let rec apply s = function
  | T.Arrow (t1, t2) ->
      T.Arrow (apply s t1, apply s t2)
  | T.TypeCtor (name, types) ->
      let types' = List.map (apply s) types in
      T.TypeCtor (name, types')
  | T.TypeInst (name, types) ->
      let types' = List.map (apply s) types in
      T.TypeInst (name, types')
  | T.Interface i -> T.Interface i
  | T.Implementation i -> T.Implementation i
  | T.RigidVar var -> T.RigidVar var
  | T.Var ({ T.name; T.constraints } as var) ->
    begin try find var s
    with Not_found -> T.Var var
    end
  | T.Record r ->
    T.Record (List.map (fun (n, t) -> (n, apply s t)) r)
  | T.TypeArrow (v1, t2) ->
      let v1' =
        try find v1 s
        with Not_found -> T.Var v1
      in match v1' with
      | T.Var v -> T.TypeArrow (v, apply s t2)
      | _ -> apply s t2

let rec unify = function
  | T.TypeInst (n2, t2s), T.TypeInst (n1, t1s)
  when n1 = n2 ->
      let aux s t1 t2 =
        unify (t1, t2) >> s
      in List.fold_left2 aux [] t1s t2s

  | T.TypeCtor _, t when t = val_type -> []
  | t, T.TypeCtor _ when t = val_type -> []

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      let s1 = unify (t11, t21) in
      let s2 = unify (apply s1 t12, apply s1 t22) in
      s2 >> s1

  (* Order matters: Var must come before TypeArrow *)
  | T.Var ({ T.constraints = cs1 } as var1),
    T.Var ({ T.constraints = cs2 } as var2) ->
      let aux c1 c2 = String.compare c1.T.intf_name c2.T.intf_name in
      let ty = T.Var { var2 with T.constraints = List.sort_uniq aux (cs1 @ cs2)} in
      var1.T.resolved_ty <- Some ty;
      [(var1, ty)]

  | T.Var ({ T.constraints } as var), t
  | t, T.Var ({ T.constraints } as var) ->
      let impls intf_desc =
        match t with
        | T.Var var
        | T.RigidVar var ->
            if not (List.mem intf_desc var.T.constraints) then
              raise (Error (Instance_not_found (t, intf_desc)))
        | t ->
            if not (List.mem_assoc t intf_desc.T.intf_impls) then
              raise (Error (Instance_not_found (t, intf_desc)))
      in
      List.iter impls constraints;
      var.T.resolved_ty <- Some t;
      [ var, t ]

  | T.TypeArrow (_, t1), t2
  | t2, T.TypeArrow (_, t1) ->
      unify (t1, t2)

  | T.RigidVar v1, T.RigidVar v2 when v1 = v2 -> []

  | T.Record r1, T.Record r2
  when List.map fst r1 = List.map fst r2 ->
    let aux s (_, t1) (_, t2) =
      unify (t1, t2) >> s
    in List.fold_left2 aux [] r1 r2

  | t1, t2 ->
      raise (Error (Unification_error (t1, t2)))

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

let fresh ({ T.name } as var) =
  { var with T.id = _fresh name }

let rec instantiate s1 = function
  | T.Arrow (t1, t2) ->
      T.Arrow (instantiate s1 t1, instantiate s1 t2)
  | T.TypeArrow (var, ty) ->
      let var' = fresh var in
      let s = [(var, T.Var var')] >> s1 in
      T.TypeArrow (var', instantiate s ty)
  | T.TypeCtor (n, ts) ->
      let ts' = List.map (instantiate s1) ts in
      T.TypeCtor (n, ts')
  | T.Record r ->
      T.Record (List.map (fun (n, t) -> (n, instantiate s1 t)) r)
  | t -> apply s1 t

let rec loosen = function
  | T.Arrow (t1, t2) -> T.Arrow (loosen t1, loosen t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (var, loosen ty)
  | T.TypeInst (n, ts) -> T.TypeInst (n, List.map loosen ts)
  | T.RigidVar var -> T.Var var
  | T.Record r -> T.Record (List.map (fun (n, t) -> (n, loosen t)) r)
  | t -> t

let get_type env v =
  try instantiate [] (List.assoc v env)
  with Not_found ->
    raise (Error (Unknown_type v))

let rec to_value = function
  | T.TypeCtor (n, ts) -> T.TypeInst (n, List.map to_value ts)
  | T.TypeInst (n, ts) -> T.TypeInst (n, List.map to_value ts)
  | T.Arrow (t1, t2) -> T.Arrow (to_value t1, to_value t2)
  | T.TypeArrow (var, ty) -> T.TypeArrow (var, to_value ty)
  | T.Record r -> T.Record (List.map (fun (n, t) -> (n, to_value t)) r)
  | t -> t

let make_var () =
  let name = "𝜏" in
  T.Var { T.id = _fresh name; T.name; T.constraints = []; T.resolved_ty = None }

let var_of_generic env { A.name; A.constraints } =
  let resolve n = match get_type env n with
    | T.Interface i -> i
    | t -> raise (Error (Invalid_constraint (name, t)))
  in
  let intfs = List.map resolve constraints in
  { T.id = _fresh name; T.name; T.constraints = intfs; T.resolved_ty = None }
