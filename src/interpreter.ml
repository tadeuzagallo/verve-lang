open Absyn

exception Unbound_variable
exception Runtime_error of string

let subst arguments fn =
  let rec aux args = function
    | Unit -> Unit
    | Literal l -> Literal l
    | Var x as v -> begin
        try List.assoc x args
        with Not_found -> v
    end
    | Application ({ callee; arguments } as app) ->
        Application { app with callee=(aux args callee); arguments=(List.map (aux args) arguments) }
    | Function ({ parameters; body } as fn) ->
        let args' = List.filter
          (fun (x, _) -> not (List.exists (fun (p: parameter) -> p.name = x) parameters))
          args
        in Function { fn with body=(List.map (aux args') body) }
  in
  let { parameters; body } as fn = match fn with
    | Function f -> f
    | _ -> assert false
  in
  let subst, params =
    let rec combine (subst, params) : parameter list * expr list -> 'a * 'b = function
      | x::xs, y::ys ->
          combine ((x.name,y)::subst, params) (xs, ys)
      | x::xs, [] ->
          combine (subst, x::params) (xs, [])
      | [], [] ->
          (List.rev subst, List.rev params)
      | [], _ ->
          raise (Runtime_error "Function applied to too many arguments")
    in combine ([], []) (parameters, arguments)
  in
  let body' = List.rev_map (aux subst) body in
  match params, body' with
  | [], [] -> Unit
  | [], _ -> List.hd body'
  | _ -> Function { fn with parameters = params; body = body' }

let rec eval env = function
  | Unit -> (Unit, env)
  | Literal l -> (Literal l, env)
  | Application { callee; generic_arguments; arguments } ->
      let (callee', _) = eval env callee in
      let arguments' = List.map (fun a -> fst @@ eval env a) arguments in
      let (v, _) = eval env (subst arguments' callee') in
      (v, env)
  | Var v -> begin
      try (List.assoc v env, env)
      with Not_found ->
        raise Unbound_variable
  end
  | (Function { name }) as fn ->
      match name with
      | Some n -> (fn, (n, fn)::env)
      | None -> (fn, env)

let eval { body } =
  List.fold_left (fun (_, env) node -> eval env node) (Unit, []) body
  |> fst
