open Type_error

module A = Absyn
module T = Types

type ty_env = (A.name * T.ty) list
type subst = (T.tvar * T.ty) list

let extend_env env (x, t) = (x, t)::env

(* Type variable helpers*)
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

let make_var () =
  let name = "𝜏" in
  T.Var { T.id = _fresh name; T.name; T.constraints = []; T.resolved_ty = None }

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
  ("int_add", T.Arrow (val_int, T.Arrow (val_int, val_int)));
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

let rec apply_type fn = function
  | T.TypeCtor (n, ts) -> fn @@ T.TypeCtor (n, List.map (apply_type fn) ts)
  | T.TypeInst (n, ts) -> fn @@ T.TypeInst (n, List.map (apply_type fn) ts)
  | T.Arrow (t1, t2) -> fn @@ T.Arrow (apply_type fn t1, apply_type fn t2)
  | T.TypeArrow (var, ty) -> fn @@ T.TypeArrow (var, (apply_type fn) ty)
  | T.Record r -> fn @@ T.Record (List.map (fun (n, t) -> n, apply_type fn t) r)
  | t -> fn t

let to_value = apply_type @@ function
    | T.TypeCtor (n, ts) -> T.TypeInst (n, ts)
    | t -> t

let rec loosen = apply_type @@ function
  | T.RigidVar var -> T.Var var
  | t -> t

let rec apply s = apply_type @@ function
  | T.Var ({ T.name; T.constraints } as var) -> begin
      try find var s
      with Not_found -> T.Var var
    end

  | T.TypeArrow (v1, t2) -> begin
      let v1' =
        try find v1 s
        with Not_found -> T.Var v1
      in match v1' with
      | T.Var v -> T.TypeArrow (v, t2)
      | _ -> t2
    end

  | t -> t

let rec instantiate s1 = apply_type @@ function
  | T.TypeArrow (var, ty) ->
      let var' = fresh var in
      let s = [(var, T.Var var')] >> s1 in
      T.TypeArrow (var', instantiate s ty)
  | t -> apply s1 t

let get_type env v =
  try instantiate [] (List.assoc v env)
  with Not_found ->
    raise (Error (Unknown_type v))

let var_of_generic env { A.name; A.constraints } =
  let resolve n = match get_type env n with
    | T.Interface i -> i
    | t -> raise (Error (Invalid_constraint (name, t)))
  in
  let intfs = List.map resolve constraints in
  { T.id = _fresh name; T.name; T.constraints = intfs; T.resolved_ty = None }

(* Unification *)
let rec unify ~expected ~actual = match expected, actual with
  | T.TypeInst (n2, t2s), T.TypeInst (n1, t1s)
  when n1 = n2 ->
      let aux s t1 t2 =
        unify ~expected:t1 ~actual:t2 >> s
      in List.fold_left2 aux [] t1s t2s

  | T.TypeCtor _, t when t = val_type -> []
  | t, T.TypeCtor _ when t = val_type -> []

  | T.Arrow (t11, t12), T.Arrow (t21, t22) ->
      let s1 = unify ~expected:t11 ~actual:t21 in
      let s2 = unify ~expected:(apply s1 t12) ~actual:(apply s1 t22) in
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
  | t1, T.TypeArrow (_, t2) ->
      unify ~expected:t1 ~actual:t2

  | T.RigidVar v1, T.RigidVar v2 when v1 = v2 -> []

  | T.Record r1, T.Record r2
  when List.map fst r1 = List.map fst r2 ->
    let aux s (_, t1) (_, t2) =
      unify ~expected:t1 ~actual:t2 >> s
    in List.fold_left2 aux [] r1 r2

  | t1, t2 ->
      raise (Error (Unification_error (t1, t2)))
