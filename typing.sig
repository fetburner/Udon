signature TYPING = sig
  exception UnboundVar of string
  val typing : Syntax.exp -> TypedSyntax.exp
end
