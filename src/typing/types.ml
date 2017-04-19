type tvar = TV of int * string

type ty = 
  | Type of string
  | Arrow of ty * ty
  | TypeArrow of tvar * ty
  | Var of tvar
  | TypeCtor of string * ty list

let rec to_string = function
  | Type s -> s
  | Var (TV (uid, name)) ->
      "'" ^ name ^ string_of_int uid
  | Arrow (t1, t2) ->
      "(" ^ (to_string t1) ^ ") -> " ^ (to_string t2)
  | TypeArrow (t1, t2) ->
      (to_string (Var t1)) ^ " : Type -> " ^ (to_string t2)
  | TypeCtor (n, ts) ->
      let ts' = List.map to_string ts in
      n ^ "<" ^ String.concat ", " ts' ^ ">"
