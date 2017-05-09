type t

val empty : t
val default_env : t
val merge : t -> t -> t

val ty_int : Types.texpr
val ty_string : Types.texpr
val ty_void : Types.texpr
val ty_type : Types.texpr

val unify : expected: Types.texpr -> Types.texpr -> unit

val add_type : t -> Absyn.name -> Types.texpr -> t
val find_type : t -> Absyn.name -> Types.texpr

val add_value : t -> Absyn.name -> Types.texpr -> t
val find_value : t -> Absyn.name -> Types.texpr

val add_ctor : t -> Absyn.name -> Types.texpr -> t
val find_ctor : t -> Absyn.name -> Types.texpr

val loosen : Types.texpr -> Types.texpr

val var_of_generic : t -> Absyn.generic -> Types.tvar
val make_var : unit -> Types.texpr

val instantiate : Types.texpr -> Types.texpr
