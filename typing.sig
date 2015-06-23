signature TYPING = sig
  exception UnboundVar of Id.t
  val typing : int -> Type.t Env.t -> Syntax.exp -> TypedSyntax.exp
end
