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

let dot_sep pp ppf v =
  let sep ppf () = char ppf '.' in
  Fmt.list ~sep pp ppf v

let pp_name ppf name = string ppf name.Absyn.str

let print_record sep pp_field pp_value  ppf fields =
  let sep ppf () = pf ppf " %c " sep in
  (braces @@ comma_sep @@ pair ~sep pp_field pp_value) ppf fields

(* AST Printing *)
open Absyn

module Absyn = struct
  let pp_qualified_name ppf name = dot_sep pp_name ppf name

  let pp_literal ppf = function
    | Int i -> int ppf i
    | String s -> quote string ppf s

  let pp_generic ppf { name; constraints } =
    pp_name ppf name;
    match constraints with
    | [] -> ()
    | [ x ] -> pf ppf "@,: %a" pp_qualified_name x
    | constraints' ->
      pf ppf "@,: %a" (box @@ parens @@ comma_sep pp_qualified_name) constraints'

  let pp_generics ppf = function
    | [] -> ()
    | generics -> (box @@ angles @@ comma_sep pp_generic) ppf generics

  let rec pp_type ppf ty =
    match ty.type_desc with
    | Arrow (ps, r) -> pf ppf "%a -> %a" (parens @@ comma_sep pp_type) ps pp_type r
    | Inst (n, ts) -> pf ppf "%a%a" pp_qualified_name n pp_generic_arguments ts
    | RecordType fields -> print_record ':' pp_name pp_type ppf fields

  and pp_param ppf { param_name; param_type } =
    pf ppf "%a@,: %a" pp_name param_name pp_type param_type

  and pp_record ppf fields = print_record '=' pp_name pp ppf fields

  and pp_ret_type ppf = function
    | None -> ()
    | Some t -> pp_type ppf t

  and pp_fn ppf { fn_name; fn_generics; fn_parameters; fn_return_type; fn_body } =
    pf ppf "@[<v>@[<v 2>fn %a%a%a %a {@ %a@]@ }@]"
      (option pp_name) fn_name
      pp_generics fn_generics
      (hvbox @@ parens @@ comma_sep pp_param) fn_parameters
      pp_ret_type fn_return_type
      (list pp_stmt) fn_body

  and pp_generic_arguments ppf = function
    | [] -> ()
    | args -> (box @@ angles @@ comma_sep pp_type) ppf args

  and pp_app ppf { callee; generic_arguments; arguments } =
    pf ppf "%a%a"
      pp callee
      (option @@ parens @@ comma_sep pp) arguments

  and pp_ctor : 'a. 'a Fmt.t -> 'a ctor Fmt.t = fun pp ppf c ->
    pf ppf "%a%a%a"
      pp_qualified_name c.ctor_name
      pp_generic_arguments c.ctor_generic_arguments
      (option @@ hvbox @@ parens @@ comma_sep pp) c.ctor_arguments

  and pp_field_access ppf { record; field } =
    pf ppf "%a.%a" pp record pp_name field

  and pp_match ppf { match_value; cases } =
    pf ppf "@[<v>@[<v 2>match %a {@ %a@]@ }@]"
      pp match_value
      (list pp_case) cases

  and pp_case ppf { pattern; case_value } =
    pf ppf "case %a: %a"
      pp_pattern pattern
      (list pp_stmt) case_value

  and pp_pattern ppf pat =
    match pat.pat_desc with
    | Pany -> string ppf "_"
    | Pvar v -> pp_name ppf v
    | Pctor (n, ps) ->
      pf ppf "%a%a"
        pp_qualified_name n
        (option @@ hvbox @@ parens @@ comma_sep pp_pattern) ps


  and pp_attribute ppf attr =
    let rec pp_attribute ppf attr =
      pf ppf "%a%a" pp_name attr.attr_name (option @@ parens pp_value) attr.attr_value
    and pp_value ppf = function
      | AttrOp o -> pp_name ppf o
      | AttrInt i -> int ppf i
      | Attribute attr -> pp_attribute ppf attr
    in
    pf ppf "#%a" pp_attribute attr

  and pp_operator ppf op =
    pf ppf "@[<v>@[<v 2>%a@ operator%a (%a) %a (%a) -> %a {@ %a@]@ }@ @]"
      (list pp_attribute) op.op_attributes
      pp_generics op.op_generics
      pp_param op.op_lhs
      pp_name op.op_name
      pp_param op.op_rhs
      pp_ret_type op.op_ret_type
      (list pp_stmt) op.op_body

  and pp_binop ppf op=
    pf ppf "%a %a %a"
      pp op.bin_lhs
      pp_name op.bin_op
      pp op.bin_rhs

  and pp_let ppf l =
    pf ppf "let %a = %a"
      pp_name l.let_var
      pp l.let_value

  and pp' ppf expr =
    match expr.expr_desc with
    | Function fn -> pp_fn ppf fn
    | Ctor ctor -> pp_ctor pp' ppf ctor
    | Application app -> pp_app ppf app
    | Var str -> pp_qualified_name ppf str
    | Literal l -> pp_literal ppf l
    | Unit -> string ppf "()"
    | Record r -> pp_record ppf r
    | Field_access f -> pp_field_access ppf f
    | Match m -> pp_match ppf m
    | Wrapped expr -> pf ppf "(%a)" pp' expr
    | Binop op -> pp_binop ppf op

  and pp ppf v = (box ~indent:2 pp') ppf v

  and pp_stmt ppf stmt =
    match stmt.stmt_desc with
    | Let l -> pp_let ppf l
    | FunctionStmt f -> pp_fn ppf f
    | Expr e -> pp ppf e

  let pp_enum_item ppf { enum_item_name; enum_item_generics; enum_item_parameters } =
    pf ppf "%a%a%a"
      pp_name enum_item_name
      pp_generics enum_item_generics
      (option @@ box @@ parens @@ comma_sep pp_type) enum_item_parameters

  let pp_enum ppf { enum_name; enum_generics; enum_items } =
    pf ppf "@[<v>@[<v 2>enum %a%a = {@ %a@]@ }@ @]"
      pp_name enum_name
      pp_generics enum_generics
      (list pp_enum_item) enum_items

  let pp_prototype ppf proto =
    pf ppf "fn %a%a%a -> %a"
      pp_name proto.proto_name
      pp_generics proto.proto_generics
      (parens @@ hvbox ~indent:2 @@ comma_sep pp_type) proto.proto_params
      pp_ret_type proto.proto_ret_type

  let pp_op_proto ppf op =
    pf ppf "%a@ operator%a (%a) %a (%a) -> %a"
      (list pp_attribute) op.oproto_attributes
      pp_generics op.oproto_generics
      pp_type op.oproto_lhs
      pp_name op.oproto_name
      pp_type op.oproto_rhs
      pp_ret_type op.oproto_ret_type

  let pp_intf_item ppf = function
    | Prototype p -> pp_prototype ppf p
    | OperatorPrototype op -> pp_op_proto ppf op

  let pp_interface ppf { intf_name; intf_param; intf_items } =
    pf ppf "@[<v>@[<v 2>interface %a<%a> {@ %a@]}@]@ "
      pp_name intf_name
      pp_generic intf_param
      (list pp_intf_item) intf_items

  let pp_impl_item ppf = function
    | ImplFunction f -> pp_fn ppf f
    | ImplOperator op -> pp_operator ppf op

  let pp_implementation ppf { impl_name; impl_arg; impl_items } =
    pf ppf "@[<v>@[<v 2>implementation %a<%a> {@ %a@]@ }@]"
      pp_qualified_name impl_name
      pp_type impl_arg
      (list pp_impl_item) impl_items

  let pp_decl ppf decl =
    match decl.decl_desc with
    | Enum e -> pp_enum ppf e
    | Interface i -> pp_interface ppf i
    | Implementation i -> pp_implementation ppf i
    | Operator op -> pp_operator ppf op
    | Stmt e -> pp_stmt ppf e

  let pp_program ppf { imports; exports; body } =
    (*"@[<v>%a@ @ %a@ @ %a@]@."*)
    pf ppf "@[<v>%a@]@."
      (Fmt.list ~sep:(suffix sp sp)pp_decl) body

  let dump_expr expr =
    pf stderr "%a@." pp expr
end

(* Value Printing *)

module Value = struct
  open Value

  let rec pp' ppf = function
    | Function fn -> Absyn.pp_fn ppf fn
    | Ctor ctor -> Absyn.pp_ctor pp' ppf ctor
    | Literal l -> Absyn.pp_literal ppf l
    | Unit -> string ppf "()"
    | Type t -> string ppf t
    | Builtin (name, _) -> string ppf name
    | InterfaceFunction t -> string ppf t
    | Record r -> print_record '=' string pp' ppf r

  and pp ppf v = (box ~indent:2 pp') ppf v

  let dump t = pf stderr "%a@." pp t

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

  let rec pp' ppf t =
    match desc t with
    | Var { id; name; constraints } ->
        pf ppf "%s%s%a" name (subscript_of_number id) pp_constraints constraints
    | RigidVar v ->
        pf ppf "'%a" pp (var v)
    | Arrow (t1, t2) ->
        pf ppf "%a@ -> %a"
          (box @@ parens pp) t1
          pp t2
    | TypeArrow (t1, t2) ->
        pf ppf "forall %a,@ %a" pp t1 pp t2
    | TypeCtor (n, ts) ->
        pf ppf "%s%a" n pp_generics ts
    | Interface i ->
        pf ppf "interface %s" i.intf_name
    | Implementation i ->
        pf ppf "implementation %a<%a>" (dot_sep string) i.impl_name pp i.impl_type
    | Record r -> print_record ':' string pp ppf r
  and pp ppf v = (box ~indent:2 pp') ppf v

  and pp_generics ppf = function
    | [] -> ()
    | g -> (box @@ angles @@ comma_sep pp) ppf g

  let dump ty = pf stdout "%a@." pp ty
end

(* Entry Point *)
let print_raw ppf value ty =
  pf ppf "@[<hov 2>%a@ : %a@]" Value.pp value Type.pp ty

let print value ty =
  print_raw stdout value ty;
  Format.pp_print_newline stdout ();

and print_subst s =
  let arrow ppf () = Fmt.pf ppf " => " in
  let print_var ppf var = Type.pp ppf (Types.var var) in
  (brackets @@ comma_sep @@ pair ~sep:arrow print_var Type.pp) stdout s
