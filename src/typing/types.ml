type tvar = TV of int * string

type ty = 
  | Const of string
  | Arrow of ty * ty
  | TypeArrow of tvar * ty
  | Var of tvar

let rec to_string = function
  | Const s -> s
  | Var (TV (_, name)) ->
      "'" ^ name
  | Arrow (t1, t2) ->
      "(" ^ (to_string t1) ^ ") -> " ^ (to_string t2)
  | TypeArrow (t1, t2) ->
      (to_string (Var t1)) ^ " : Type -> " ^ (to_string t2)
