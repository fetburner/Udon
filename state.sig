signature STATE = sig
  type ('s, 'a) state
  val return : 'a -> ('s, 'a) state
  val >>= : ('s, 'a) state * ('a -> ('s, 'b) state) -> ('s, 'b) state
  val >> : ('s, 'a) state * ('s, 'b) state -> ('s, 'b) state
  val runState : ('s, 'a) state -> 's -> 'a * 's
  val get : ('s, 's) state
  val put : 's -> ('s, unit) state
  val modify : ('s -> 's) -> ('s, unit) state
end
