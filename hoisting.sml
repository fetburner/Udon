structure Hoisting = struct
  local
    open Cps
  in
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
      | collectBindingOfExp env k (LET_REC ((x, t), m)) =
          if isEvaluable env (IdSet.subtract (freeVarOfTerm t, x)) then
            SOME (fn m' => LET_REC ((x, t), m'), k m)
          else
            (case collectBindingOfTerm env (fn t' => k (LET_REC ((x, t'), m))) t of
                    result as SOME _ => result
                | NONE =>
                    collectBindingOfExp env (fn m' => k (LET_REC ((x, t), m'))) m)
      | collectBindingOfExp env k (IF (x, m, n)) =
          (case collectBindingOfExp env (fn m' => k (IF (x, m', n))) m of
                result as SOME _ => result
              | NONE =>
                  collectBindingOfExp env (fn n' => k (IF (x, m, n'))) n)


    fun hoistingExp env (m as APP _) = m
      | hoistingExp env (m as APP_TAIL _) = m
      | hoistingExp env (LET_REC ((x, t), e)) =
          let val env' = IdSet.add (env, x) in
            case collectBindingOfTerm env (fn x => x) t of
                 NONE =>
                   LET_REC ((x, hoistingTerm env' t), hoistingExp env' e)
               | SOME (binding, t') =>
                   hoistingExp env (binding (LET_REC ((x, t'), e)))
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
  end

  val hoisting = hoistingExp IdSet.empty
end
