type ty = 
  | Const of string
  | Arrow of ty * ty
  | Type

let rec to_string = function
  | Type -> "Type"
  | Const s -> s
  | Arrow (t1, t2) ->
      (to_string t1) ^ " -> " ^ (to_string t2)
