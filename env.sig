signature ENV = sig
  type 'a t

  val empty : 'a t
  val insert : ('a t * Id.t * 'a) -> 'a t
  val insertList : ('a t * (Id.t * 'a) list) -> 'a t
  val find : ('a t * Id.t) -> 'a option
  val findName : (Type.t t * string) -> (Id.t * Type.t) option
end
