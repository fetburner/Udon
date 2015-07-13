signature INFIXING = sig
  exception UnboundVar of string
  val infixing :
    ((Syntax.exp * Syntax.exp -> Syntax.exp) * int * Infixer.Assoc.assoc) option Env.t
      -> ConcreteSyntax.exp -> Syntax.exp
end
