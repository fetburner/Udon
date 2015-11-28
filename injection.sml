structure Injection = struct
  open Prim

  fun reduceBinOp p (m, n) = Syntax.PRIM (p, [m, n])

  val infixInfo =
    foldl StringMap.insert' StringMap.empty
      (map (fn (x, p) =>
        let val (d, assoc) = Prim.priority p in
          (x, (reduceBinOp p, d, assoc))
        end) primitives)

  val typeInfo = Env.empty
end

