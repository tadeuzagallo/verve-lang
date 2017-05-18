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
  | Class of class_desc

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

and class_desc = {
  cls_name : string;
  cls_generics : texpr list;
  cls_props : (string * texpr) list;
  cls_fns : (string * texpr) list ref;
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
let class_ c = _texpr @@ Class c

let eq_var k var =
  k.name = var.name && k.id = var.id

let rec eq_type t1 t2 =
  let t1 = desc t1 and t2 = desc t2 in
  if t1 == t2 then true else
  match t1, t2 with
  | Var v1, Var v2
  | RigidVar v1, RigidVar v2
  -> eq_var v1 v2

  | Arrow (t11, t12), Arrow(t21, t22)
  | TypeArrow (t11, t12), TypeArrow(t21, t22)
  -> eq_type t11 t21 && eq_type t12 t22

  | TypeCtor (n1, t1s), TypeCtor (n2, t2s)
  when String.equal n1 n2 && List.length t1s = List.length t2s ->
    List.for_all2 eq_type t1s t2s
  | Class c1, Class c2
  when c1.cls_name = c2.cls_name && List.length c1.cls_generics = List.length c2.cls_generics ->
    List.for_all2 eq_type c1.cls_generics c2.cls_generics

  | _ -> false

let rec clean_type' used t =
  let t = repr t in
  match desc t with
  | TypeCtor (n, ts) ->
    let aux (acc, used) t = let t, used = clean_type' used t in (t::acc), used in
    let ts, used = List.fold_left aux ([], used) ts in
    type_ctor (n, List.rev ts), used
  | Class c ->
    let aux (acc, used) t = let t, used = clean_type' used t in (t::acc), used in
    let cls_generics, used = List.fold_left aux ([], used) c.cls_generics in
    class_ { c with cls_generics }, used
  | Arrow (t1, t2) ->
    let t1, used = clean_type' used t1 in
    let t2, used = clean_type' used t2 in
    arrow t1 t2, used
  | TypeArrow (var, ty) ->
    let ty, used = clean_type' used ty in
    if List.exists (eq_type var) used then
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
