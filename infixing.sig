signature INFIXING = sig
  type assoc
  exception UnboundVar of string
  exception SyntaxError
  val infixing : (int * assoc) option Env.t -> ConcreteSyntax.exp -> Syntax.exp
end
