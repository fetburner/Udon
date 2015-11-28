signature INFIXING = sig
  exception UnboundVar of string
  val infixing :
    ((Syntax.exp * Syntax.exp -> Syntax.exp) * int * Infixer.Assoc.assoc) StringMap.map
      -> ConcreteSyntax.exp -> Syntax.exp
end
