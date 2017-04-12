open Absyn

exception Unbound_variable

let subst arguments fn =
  let rec aux args = function
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
  let parameters' = List.map (fun (p: parameter) -> p.name) parameters in
  let args = List.combine parameters' arguments in
  let body' = List.map (aux args) body in
  let length = List.length body' in
  if length = 0 then
    Unit
  else List.nth body' (length - 1)

let rec eval = function
  | Application { callee; generic_arguments; arguments } ->
      let callee' = eval callee in
      let arguments' = List.map eval arguments in
      eval (subst arguments callee)
  | Var v -> raise Unbound_variable
  | v -> v
