structure DataFlow = struct
  structure IdGraph = Graph (IdSet)
  open Cps

  fun termDataFlowAnalysis (t as CONST _) = t
    | termDataFlowAnalysis (t as VAR _) = t
    | termDataFlowAnalysis (ABS (xs, e)) =
        ABS (xs, expDataFlowAnalysis e)
    | termDataFlowAnalysis (t as PRIM _) = t

  and expDataFlowAnalysis (e as APP _) = e
    | expDataFlowAnalysis (LET_REC (bindings, e)) =
        let
          val bindings' =
            map (fn (x, t) => (x, termDataFlowAnalysis t)) bindings
          val env = Env.fromList bindings'
          val xs = IdSet.fromList (map #1 bindings')
          val edges =
            Env.fromList (map (fn (x, t) => (x, termFreeVar t)) bindings')
        in
          foldl (fn (xs, e) =>
            LET_REC (map (fn x => (x, valOf (Env.find (env, x)))) xs, e))
            (expDataFlowAnalysis e)
            (IdGraph.scc (xs, fn x => valOf (Env.find (edges, x))))
        end
    | expDataFlowAnalysis (IF (x, e1, e2)) =
        IF (x, expDataFlowAnalysis e1, expDataFlowAnalysis e2)

  val dataFlowAnalysis = expDataFlowAnalysis
end
