type ty = 
  | Const of string
  | Arrow of ty * ty
  | Type
