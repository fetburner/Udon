signature TYPING = sig
  exception Unify of Type.t * Type.t
  exception UnboundVar of string
  val f : Type.t Env.t -> Syntax.exp -> TypedSyntax.exp
end
