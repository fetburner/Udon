structure ControlFlow = struct
  structure IdGraph = Graph (IdSet)
  open Cps

  fun termControlFlowAnalysis (t as CONST _) = t
    | termControlFlowAnalysis (t as VAR _) = t
    | termControlFlowAnalysis (ABS (xs, e)) =
        ABS (xs, expControlFlowAnalysis e)
    | termControlFlowAnalysis (t as PRIM _) = t

  and expControlFlowAnalysis (e as APP _) = e
    | expControlFlowAnalysis (LET_REC (bindings, e)) =
        let
          val bindings' =
            map (fn (x, t) => (x, termControlFlowAnalysis t)) bindings
          val env = Env.fromList bindings'
          val xs = IdSet.fromList (map #1 bindings')
          val edges =
            Env.fromList (map (fn (x, t) => (x, termFreeVar t)) bindings')
        in
          foldl (fn (xs, e) =>
            LET_REC (map (fn x => (x, valOf (Env.find (env, x)))) xs, e))
            (expControlFlowAnalysis e)
            (IdGraph.scc (xs, fn x => valOf (Env.find (edges, x))))
        end
    | expControlFlowAnalysis (IF (x, e1, e2)) =
        IF (x, expControlFlowAnalysis e1, expControlFlowAnalysis e2)

  val controlFlowAnalysis = expControlFlowAnalysis
end
