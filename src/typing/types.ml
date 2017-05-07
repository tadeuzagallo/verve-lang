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
  mutable intf_impls : (texpr * implementation_desc) list;
}

and implementation_desc = {
  impl_name : string;
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
