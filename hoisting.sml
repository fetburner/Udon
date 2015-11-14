structure Hoisting = struct
  open Cps

  fun isEvaluable env fv = IdSet.isSubset (fv, env)

  fun termCollectEvaluable env k (t as CONST _) = NONE
    | termCollectEvaluable env k (t as VAR _) = NONE
    | termCollectEvaluable env k (ABS ((x, k'), m)) =
        expCollectEvaluable env (fn m' => k (ABS ((x, k'), m'))) m
    | termCollectEvaluable env k (ABS_CONT (x, m)) =
        expCollectEvaluable env (fn m' => k (ABS_CONT (x, m'))) m
    | termCollectEvaluable env k (t as PRIM _) = NONE

  and expCollectEvaluable env k (APP _) = NONE
    | expCollectEvaluable env k (APP_TAIL _) = NONE
    | expCollectEvaluable env k (LET_REC (bindings, m)) =
        if isEvaluable env (bindingsFreeVar bindings) then
          SOME (bindings, k m)
        else
          (case expCollectEvaluable env (fn m' => k (LET_REC (bindings, m'))) m of
                result as SOME _ => result
              | NONE =>
                  bindingsCollectEvaluable env
                    (fn bindings' => k (LET_REC (bindings', m))) [] bindings)
    | expCollectEvaluable env k (IF (x, m, n)) =
        (case expCollectEvaluable env (fn m' => k (IF (x, m', n))) m of
              result as SOME _ => result
            | NONE =>
                expCollectEvaluable env (fn n' => k (IF (x, m, n'))) n)

  and bindingsCollectEvaluable env k revBindings1 [] = NONE
    | bindingsCollectEvaluable env k revBindings1 ((x, t) :: bindings2) =
        (case termCollectEvaluable env
          (fn t' => k (rev revBindings1 @ (x, t') :: bindings2)) t of
              result as SOME _ => result
            | NONE =>
                bindingsCollectEvaluable env k
                  ((x, t) :: revBindings1) bindings2)


  fun expHoisting env (m as APP _) = m
    | expHoisting env (m as APP_TAIL _) = m
    | expHoisting env (LET_REC (bindings, e)) =
        let val env' = IdSet.addList (env, map #1 bindings) in
          case bindingsCollectEvaluable env' (fn x => x) [] bindings of
               NONE =>
                 LET_REC
                   (map (fn (x, t) => (x, termHoisting env' t)) bindings,
                    expHoisting env' e)
             | SOME (binding, bindings') =>
                 expHoisting env (LET_REC (binding @ bindings', e))
        end
    | expHoisting env (IF (x, e1, e2)) =
        IF (x, expHoisting env e1, expHoisting env e2)

  and termHoisting env (t as CONST _) = t
    | termHoisting env (t as VAR _) = t
    | termHoisting env (ABS ((x, k), e)) =
        ABS ((x, k), expHoisting (IdSet.addList (env, [x, k])) e)
    | termHoisting env (ABS_CONT (x, e)) =
        ABS_CONT (x, expHoisting (IdSet.add (env, x)) e)
    | termHoisting env (t as PRIM _) = t

  val hoisting = expHoisting IdSet.empty
end
