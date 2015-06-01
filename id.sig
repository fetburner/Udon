signature ID =
sig
  type t = string * int
  val toString : t -> string
  (* val fromString : string -> t option *)
  val seqToString : t list -> string
  val gensym : string -> t
end
