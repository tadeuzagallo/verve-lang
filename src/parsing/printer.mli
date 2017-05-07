val print : Value.t -> Types.texpr -> unit
val print_raw : Format.formatter -> Value.t -> Types.texpr -> unit

module Absyn : sig
  val pp_pattern : Absyn.pattern Fmt.t
  val pp_program : Absyn.program Fmt.t
end

module Type : sig
  val dump : Types.texpr -> unit
  val pp : Types.texpr Fmt.t
end

module Value : sig
  val dump : Value.t -> unit
end
