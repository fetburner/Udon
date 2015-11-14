structure Hoisting = struct
  open Cps

  fun isEvaluable env fv = IdSet.isSubset (fv, env)

  fun collectBindingOfTerm env k (t as CONST _) = NONE
    | collectBindingOfTerm env k (t as VAR _) = NONE
    | collectBindingOfTerm env k (ABS ((x, k'), m)) =
        collectBindingOfExp env (fn m' => k (ABS ((x, k'), m'))) m
    | collectBindingOfTerm env k (ABS_CONT (x, m)) =
        collectBindingOfExp env (fn m' => k (ABS_CONT (x, m'))) m
    | collectBindingOfTerm env k (t as PRIM _) = NONE

  and collectBindingOfExp env k (APP _) = NONE
    | collectBindingOfExp env k (APP_TAIL _) = NONE
    | collectBindingOfExp env k (LET_REC (bindings, m)) =
        if isEvaluable env (freeVarOfBindings bindings) then
          SOME (bindings, k m)
        else
          (case collectBindingOfExp env (fn m' => k (LET_REC (bindings, m'))) m of
                result as SOME _ => result
              | NONE =>
                  collectBindingOfBindings env
                    (fn bindings' => k (LET_REC (bindings', m))) [] bindings)
    | collectBindingOfExp env k (IF (x, m, n)) =
        (case collectBindingOfExp env (fn m' => k (IF (x, m', n))) m of
              result as SOME _ => result
            | NONE =>
                collectBindingOfExp env (fn n' => k (IF (x, m, n'))) n)

  and collectBindingOfBindings env k revBindings1 [] = NONE
    | collectBindingOfBindings env k revBindings1 ((x, t) :: bindings2) =
        (case collectBindingOfTerm env
          (fn t' => k (rev revBindings1 @ (x, t') :: bindings2)) t of
              result as SOME _ => result
            | NONE =>
                collectBindingOfBindings env k
                  ((x, t) :: revBindings1) bindings2)


  fun hoistingExp env (m as APP _) = m
    | hoistingExp env (m as APP_TAIL _) = m
    | hoistingExp env (LET_REC (bindings, e)) =
        let val env' = IdSet.addList (env, map #1 bindings) in
          case collectBindingOfBindings env' (fn x => x) [] bindings of
               NONE =>
                 LET_REC
                   (map (fn (x, t) => (x, hoistingTerm env' t)) bindings,
                    hoistingExp env' e)
             | SOME (binding, bindings') =>
                 hoistingExp env (LET_REC (binding @ bindings', e))
        end
    | hoistingExp env (IF (x, e1, e2)) =
        IF (x, hoistingExp env e1, hoistingExp env e2)

  and hoistingTerm env (t as CONST _) = t
    | hoistingTerm env (t as VAR _) = t
    | hoistingTerm env (ABS ((x, k), e)) =
        ABS ((x, k), hoistingExp (IdSet.addList (env, [x, k])) e)
    | hoistingTerm env (ABS_CONT (x, e)) =
        ABS_CONT (x, hoistingExp (IdSet.add (env, x)) e)
    | hoistingTerm env (t as PRIM _) = t

  val hoisting = hoistingExp IdSet.empty
end
