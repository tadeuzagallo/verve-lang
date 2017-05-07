type t

val empty : t
val default_env : t
val extend : t -> (string * Types.texpr) -> t
val merge : t -> t -> t

val val_int : Types.texpr
val val_string : Types.texpr
val val_void : Types.texpr

val unify : expected: Types.texpr -> Types.texpr -> unit

val get_type : t -> string -> Types.texpr

val add_ctor : t -> (string * Types.texpr) -> t
val get_ctor : t -> string -> Types.texpr

val to_value : Types.texpr -> Types.texpr
val loosen : Types.texpr -> Types.texpr

val var_of_generic : t -> Absyn.generic -> Types.tvar
val make_var : unit -> Types.texpr

val instantiate : Types.texpr -> Types.texpr
