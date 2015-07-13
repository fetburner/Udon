structure Injection = struct
  open Prim

  fun reduceBinOp p (m, n) = Syntax.PRIM (p, [m, n])

  val infixInfo =
    Env.fromList
      (map (fn (x, p) =>
        case Prim.priority p of
             NONE => (x, NONE)
           | SOME (d, assoc) =>
               (x, SOME (reduceBinOp p, d, assoc))) primitives)

  val typeInfo = Env.empty
end

