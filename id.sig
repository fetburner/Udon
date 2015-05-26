signature ID =
sig
  type t
  val toString : t -> string
  val fromString : string -> t option
  val seqToString : t list -> string
  val gensym : unit -> t
end
