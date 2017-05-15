type t

val empty : t
val default_env : t
val merge : t -> t -> t

val ty_int : Types.texpr
val ty_string : Types.texpr
val ty_bool : Types.texpr
val ty_void : Types.texpr
val ty_type : Types.texpr

val assoc_ty : Types.texpr -> (Types.texpr * 'a) list -> 'a
val eq_type : Types.texpr -> Types.texpr -> bool
val unify : expected: Types.texpr -> Types.texpr -> unit

val add_type : t -> Absyn.name -> Types.texpr -> t
val find_type : t -> Absyn.qualified_name -> Types.texpr

val add_value : t -> Absyn.name -> Types.texpr -> t
val find_value : t -> Absyn.qualified_name -> Types.texpr

val add_ctor : t -> Absyn.name -> Types.texpr -> t
val find_ctor : t -> Absyn.qualified_name -> Types.texpr

val add_module : t -> Absyn.name -> t -> t
val find_module : t -> Absyn.name -> t

val loosen : Types.texpr -> Types.texpr
val constrain : Types.texpr list -> Types.texpr -> Types.texpr

val var_of_generic : t -> Absyn.generic -> Types.tvar
val make_var : unit -> Types.texpr

val instantiate : Types.texpr -> Types.texpr
