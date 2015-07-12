signature INFIXING = sig
  exception UnboundVar of string
  exception SyntaxError
  val infixing : (int * Infixer.Assoc.assoc) option Env.t -> ConcreteSyntax.exp -> Syntax.exp
end
