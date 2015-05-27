signature TYPING = sig
  exception Unify of Type.t * Type.t
  exception UnboundVar of Id.t
  val f : Type.t Env.t -> Syntax.exp -> TypedSyntax.exp
end
