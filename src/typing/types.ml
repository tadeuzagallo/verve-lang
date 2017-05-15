type texpr = {
  mutable texpr : node;
}

and node =
  | Desc of ty
  | Link of texpr

and ty =
  | TypeCtor of string * texpr list
  | Arrow of texpr * texpr
  | TypeArrow of texpr * texpr
  | Var of tvar
  | RigidVar of tvar
  | Interface of interface_desc
  | Implementation of implementation_desc
  | Record of (string * texpr) list

and tvar = {
  id : int;
  name : string;
  constraints : interface_desc list;
}

and interface_desc = {
  intf_name : string;
  mutable intf_items : (string * texpr) list;
  mutable intf_impls : (texpr * implementation_desc) list;
}

and implementation_desc = {
  impl_name : string list;
  impl_type: texpr;
  impl_items : texpr list;
}

let _texpr d = { texpr = Desc d }
let rec repr t =
  match t.texpr with
  | Desc _ -> t
  | Link u ->
    let v = repr u in
    t.texpr <- Link v;
    v

let desc t =
  match (repr t).texpr with
  | Link u -> assert false
  | Desc d -> d

let var v = _texpr (Var v)

let rigid_var v = _texpr (RigidVar v)
let record r = _texpr (Record r)
let type_ctor (n, ts) = _texpr @@ TypeCtor (n, ts)
let interface i = _texpr @@ Interface i
let implementation i = _texpr @@ Implementation i
let arrow t1 t2 = _texpr @@ Arrow (t1, t2)
let type_arrow t1 t2 = _texpr @@ TypeArrow (t1, t2)

let rec clean_type' used t =
  let t = repr t in
  match desc t with
  | TypeCtor (n, ts) ->
    let aux (acc, used) t = let t, used = clean_type' used t in (t::acc), used in
    let ts, used = List.fold_left aux ([], used) ts in
    type_ctor (n, List.rev ts), used
  | Arrow (t1, t2) ->
    let t1, used = clean_type' used t1 in
    let t2, used = clean_type' used t2 in
    arrow t1 t2, used
  | TypeArrow (var, ty) ->
    let ty, used = clean_type' used ty in
    if List.mem var used then
      type_arrow var ty, used
    else
      clean_type' used ty
  | Record r ->
    let aux (acc, used) (n, t) = let t, used = clean_type' used t in ((n, t)::acc), used in
    let fields, env = List.fold_left aux ([], used) r in
    record (List.rev fields), env
  | Var _ | RigidVar _ -> t, t :: used
  | _ -> t, used

let clean_type t = clean_type' [] t |> fst
