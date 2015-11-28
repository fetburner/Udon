signature TYPING = sig
  exception UnboundVar of string
  val typing : int -> Type.scheme Env.t -> Syntax.exp -> TypedSyntax.exp
end
