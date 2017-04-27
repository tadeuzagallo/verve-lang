open Fmt

(* Combinators *)
let angles pp_v ppf v = pf ppf "<%a>" pp_v v
let parens pp_v ppf v = pf ppf "(%a)" pp_v v
let brackets pp_v ppf v = pf ppf "[%a]" pp_v v
let braces pp_v ppf v = pf ppf "{%a}" pp_v v
let list v = Fmt.list ~sep:sp v

let comma_sep pp_v ppf v =
  let sep ppf () = pf ppf ",@ " in
  Fmt.list ~sep pp_v ppf v

(* AST Printing *)
open Absyn

module Absyn = struct
  let pp_literal ppf = function
    | Int i -> int ppf i

  let pp_generic ppf { name; constraints } =
    string ppf name;
    match constraints with
    | [] -> ()
    | [ x ] -> pf ppf "@,: %s" x
    | constraints' ->
      pf ppf "@,: %a" (box @@ parens @@ comma_sep string) constraints'

  let pp_generics ppf = function
    | [] -> ()
    | generics -> (box @@ angles @@ comma_sep pp_generic) ppf generics

  let rec pp_type ppf = function
    | Arrow (ps, r) -> pf ppf "%a -> %a" (parens @@ comma_sep pp_type) ps pp_type r
    | Inst (n, ts) -> pf ppf "%s%a" n pp_generic_arguments ts

  and pp_param ppf { param_name; param_type } =
    pf ppf "%s@,: %a" param_name pp_type param_type

  and pp_fn ppf { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
    pf ppf "@[<v>@[<v 2>fn %a%a%a -> %a {@ %a@]@ }@]"
      (option string) fn_name
      pp_generics fn_generics
      (hvbox @@ parens @@ comma_sep pp_param) fn_parameters
      pp_type fn_return_type
      (list pp) fn_body

  and pp_generic_arguments ppf = function
    | [] -> ()
    | args -> (box @@ angles @@ comma_sep pp_type) ppf args

  and pp_app ppf { callee; generic_arguments; arguments } =
    pf ppf "%a%a"
      pp callee
      (option @@ parens @@ comma_sep pp) arguments

  and pp_ctor ppf { ctor_name;  ctor_generic_arguments; ctor_arguments } =
    pf ppf "%s%a%a"
      ctor_name
      pp_generic_arguments ctor_generic_arguments
      (option @@ hvbox @@ parens @@ comma_sep pp) ctor_arguments

  and pp' ppf = function
    | Function fn -> pp_fn ppf fn
    | Ctor ctor -> pp_ctor ppf ctor
    | Application app -> pp_app ppf app
    | Var str -> string ppf str
    | Literal l -> pp_literal ppf l
    | Unit -> string ppf "()"

  and pp ppf v = (box ~indent:2 pp') ppf v

  let pp_enum_item ppf { enum_item_name; enum_item_generics; enum_item_parameters } =
    pf ppf "%s%a%a"
      enum_item_name
      pp_generics enum_item_generics
      (option @@ box @@ parens @@ comma_sep pp_type) enum_item_parameters

  let pp_enum ppf { enum_name; enum_generics; enum_items } =
    pf ppf "@[<v>@[<v 2>enum %s%a = {@ %a@]@ }@ @]"
      enum_name
      pp_generics enum_generics
      (list pp_enum_item) enum_items

  let pp_prototype ppf proto =
    pf ppf "fn %s%a%a -> %a"
      proto.proto_name
      pp_generics proto.proto_generics
      (parens @@ hvbox ~indent:2 @@ comma_sep pp_type) proto.proto_params
      pp_type proto.proto_ret_type

  let pp_interface ppf { intf_name; intf_param; intf_functions } =
    pf ppf "@[<v>@[<v 2>interface %s<%a> {@ %a@]}@]@ "
      intf_name
      pp_generic intf_param
      (list pp_prototype) intf_functions

  let pp_implementation ppf { impl_name; impl_arg; impl_functions } =
    pf ppf "@[<v>@[<v 2>implementation %s<%a> {@ %a@]@ }@]"
      impl_name
      pp_type impl_arg
      (list pp_fn) impl_functions

  let pp_decl ppf = function
    | Enum e -> pp_enum ppf e
    | Interface i -> pp_interface ppf i
    | Implementation i -> pp_implementation ppf i
    | Expr e -> pp ppf e

  let pp_program ppf { imports; exports; body } =
    (*"@[<v>%a@ @ %a@ @ %a@]@."*)
    pf ppf "@[<v>%a@]@."
      (Fmt.list ~sep:(suffix sp sp)pp_decl) body
end

(* Value Printing *)

module Value = struct
  open Value

  let rec pp' ppf = function
    | Function fn -> Absyn.pp_fn ppf fn
    | Ctor ctor -> Absyn.pp_ctor ppf ctor
    | Literal l -> Absyn.pp_literal ppf l
    | Unit -> string ppf "()"
    | Type t -> string ppf t
    | InterfaceFunction t -> string ppf t

  and pp ppf v = (box ~indent:2 pp') ppf v

end

(* Type Printing *)
module Type = struct
  open Types

  let rec subscript_of_number = function
    | n when n < 10 ->
        "\xE2\x82" ^ String.make 1 @@ char_of_int (0x80 + n)
    | n ->
        subscript_of_number (n / 10) ^ subscript_of_number (n mod 10)

  let pp_intf_name ppf { intf_name } = string ppf intf_name

  let pp_constraints ppf = function
    | [] -> ()
    | [ c ] -> pf ppf "@,: %a" pp_intf_name c
    | cs -> pf ppf "@,: %a" (box @@ parens @@ comma_sep pp_intf_name) cs

  let rec pp' ppf = function
    | Var { id; name; constraints } ->
        pf ppf "%s%s%a" name (subscript_of_number id) pp_constraints constraints
    | RigidVar var ->
        pf ppf "'%a" pp (Var var)
    | Arrow (t1, t2) ->
        pf ppf "%a@ -> %a"
          (box @@ parens pp) t1
          pp t2
    | TypeArrow (t1, t2) ->
        pf ppf "forall %a,@ %a" pp (Var t1) pp t2
    | TypeCtor (n, ts) ->
        string ppf "Type"
    | TypeInst (n, ts) ->
        pf ppf "%s%a" n pp_generics ts
    | Interface i ->
        pf ppf "interface %s" i.intf_name
    | Implementation i ->
        pf ppf "implementation %s<%a>" i.impl_name pp i.impl_type
  and pp ppf v = (box ~indent:2 pp') ppf v

  and pp_generics ppf = function
    | [] -> ()
    | g -> (box @@ angles @@ comma_sep pp) ppf g
end

(* Entry Point *)
let print_raw ppf value ty =
  pf ppf "@[<hov 2>%a@ : %a@]" Value.pp value Type.pp ty

let print value ty =
  print_raw stdout value ty;
  Format.pp_print_newline stdout ();
