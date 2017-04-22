type tvar = TV of int * string

type ty =
  | Type of string
  | Arrow of ty * ty
  | TypeArrow of tvar * ty
  | Var of tvar
  | ConstrainedVar of tvar * interface_desc list
  | TypeCtor of string * ty list
  | Interface of interface_desc
  | Implementation of implementation_desc

and interface_desc = {
  intf_name : string;
  mutable intf_impls : (ty * implementation_desc) list;
}

and implementation_desc = {
  impl_name : string;
  impl_type: ty;
  impl_functions : ty list;
}

let rec to_string = function
  | Type s -> s
  | Var (TV (uid, name)) ->
      "'" ^ name ^ string_of_int uid
  | ConstrainedVar (var, cs) ->
      let cs' = List.map (fun c -> c.intf_name) cs in
      to_string (Var var) ^ ": (" ^ String.concat ", " cs' ^ ")"
  | Arrow (t1, t2) ->
      "(" ^ (to_string t1) ^ ") -> " ^ (to_string t2)
  | TypeArrow (t1, t2) ->
      (to_string (Var t1)) ^ " : Type -> " ^ (to_string t2)
  | TypeCtor (n, ts) ->
      let ts' = List.map to_string ts in
      n ^ "<" ^ String.concat ", " ts' ^ ">"
  | Interface i ->
      "interface " ^ i.intf_name
  | Implementation i ->
      "implementation " ^ i.impl_name ^ "<" ^ to_string i.impl_type ^ ">"
