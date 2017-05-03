type ty =
  | TypeCtor of string * ty list
  | TypeInst of string * ty list
  | Arrow of ty * ty
  | TypeArrow of tvar * ty
  | Var of tvar
  | RigidVar of tvar
  | Interface of interface_desc
  | Implementation of implementation_desc
  | Record of (string * ty) list

and tvar = {
  id : int;
  name : string;
  constraints : interface_desc list;
  mutable resolved_ty : ty option;
}

and interface_desc = {
  intf_name : string;
  mutable intf_impls : (ty * implementation_desc) list;
}

and implementation_desc = {
  impl_name : string;
  impl_type: ty;
  impl_items : ty list;
}
