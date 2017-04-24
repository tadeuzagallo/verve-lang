type ty =
  | Type of string
  | Arrow of ty * ty
  | TypeArrow of tvar * ty
  | Var of tvar
  | RigidVar of tvar
  | TypeCtor of string * ty list
  | Interface of interface_desc
  | Implementation of implementation_desc

and tvar = {
  id : int;
  name : string;
  constraints : interface_desc list
}

and interface_desc = {
  intf_name : string;
  mutable intf_impls : (ty * implementation_desc) list;
}

and implementation_desc = {
  impl_name : string;
  impl_type: ty;
  impl_functions : ty list;
}

let rec subscript_of_number = function
  | n when n < 10 ->
      "\xE2\x82" ^ String.make 1 @@ char_of_int (0x80 + n)
  | n ->
      subscript_of_number (n / 10) ^ subscript_of_number (n mod 10)

let string_of_constraints = function
  | [] -> ""
  | [ c ] -> ": " ^ c.intf_name
  | cs ->
      let cs' = List.map (fun c -> c.intf_name) cs in
      ": (" ^ String.concat ", " cs' ^ ")"

let rec to_string = function
  | Type s -> s
  | Var { id; name; constraints } ->
      name ^ subscript_of_number id ^ string_of_constraints constraints
  | RigidVar var ->
      "'" ^ to_string (Var var)
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
