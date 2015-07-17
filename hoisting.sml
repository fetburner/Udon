structure Hoisting = struct
  local
    open Cps
  in
    fun isEvaluable env fv = IdSet.isSubset (fv, env)

    fun collectBindingOfTerm env k (t as VAL _) = NONE
      | collectBindingOfTerm env k (ABS ((xs, k'), m)) =
          collectBindingOfExp env (fn m' => k (ABS ((xs, k'), m'))) m
      | collectBindingOfTerm env k (t as PRIM _) = NONE

    and collectBindingOfExp env k (APP (x, (vs, c))) =
          collectBindingOfCont env (fn c' => k (APP (x, (vs, c')))) c
      | collectBindingOfExp env k (APP_TAIL _) = NONE
      | collectBindingOfExp env k (LET ((x, t), m)) =
          if isEvaluable env (freeVarOfTerm t) then
            SOME (fn m' => LET ((x, t), m'), k m)
          else
            (case collectBindingOfTerm env (fn t' => k (LET ((x, t'), m))) t of
                  result as SOME _ => result
                | NONE =>
                    collectBindingOfExp env (fn m' => k (LET ((x, t), m'))) m)
      | collectBindingOfExp env k (LET_REC ((x, t), m)) =
          if isEvaluable env (IdSet.subtract (freeVarOfTerm t, x)) then
            SOME (fn m' => LET_REC ((x, t), m'), k m)
          else
            (case collectBindingOfTerm env (fn t' => k (LET_REC ((x, t'), m))) t of
                    result as SOME _ => result
                | NONE =>
                    collectBindingOfExp env (fn m' => k (LET_REC ((x, t), m'))) m)
      | collectBindingOfExp env k (LET_CONT ((k', c), m)) =
          if isEvaluable env (IdSet.subtract (freeVarOfCont c, k')) then
            SOME (fn m' => LET_CONT ((k', c), m'), k m)
          else
            (case collectBindingOfCont env (fn c' => k (LET_CONT ((k', c'), m))) c of
                  result as SOME _ => result
                | NONE =>
                    collectBindingOfExp env (fn m' => k (LET_CONT ((k', c), m'))) m)
      | collectBindingOfExp env k (IF (v, m, n)) =
          (case collectBindingOfExp env (fn m' => k (IF (v, m', n))) m of
                result as SOME _ => result
              | NONE =>
                  collectBindingOfExp env (fn n' => k (IF (v, m, n'))) n)

    and collectBindingOfCont env k (CVAR _) = NONE
      | collectBindingOfCont env k (CABS (x, m)) =
          collectBindingOfExp env (fn m' => k (CABS (x, m'))) m


    fun hoistingExp env (APP (x, (vs, c))) = APP (x, (vs, hoistingCont env c))
      | hoistingExp env (e as APP_TAIL _) = e
      | hoistingExp env (LET ((x, t), e)) =
          (case collectBindingOfTerm env (fn x => x) t of
                NONE =>
                  LET ((x, hoistingTerm env t), hoistingExp (IdSet.add (env, x)) e)
              | SOME (binding, t') =>
                  hoistingExp env (binding (LET ((x, t'), e))))
      | hoistingExp env (LET_REC ((x, t), e)) =
          let val env' = IdSet.add (env, x) in
            case collectBindingOfTerm env (fn x => x) t of
                 NONE =>
                   LET_REC ((x, hoistingTerm env' t), hoistingExp env' e)
               | SOME (binding, t') =>
                   hoistingExp env (binding (LET_REC ((x, t'), e)))
          end
      | hoistingExp env (LET_CONT ((k, c), e)) =
          let val env' = IdSet.add (env, k) in
            case collectBindingOfCont env (fn x => x) c of
                 NONE =>
                   LET_CONT ((k, hoistingCont env' c), hoistingExp env' e)
               | SOME (binding, c') =>
                   hoistingExp env (binding (LET_CONT ((k, c'), e)))
          end
      | hoistingExp env (IF (v, e1, e2)) =
          IF (v, hoistingExp env e1, hoistingExp env e2)

    and hoistingTerm env (t as VAL _) = t
      | hoistingTerm env (ABS ((xs, k), e)) =
          let val env' = foldl IdSet.add' env (k :: xs) in
            ABS ((xs, k), hoistingExp env' e)
          end
      | hoistingTerm env (t as PRIM _) = t

    and hoistingCont env (c as CVAR _) = c
      | hoistingCont env (CABS (x, e)) =
          CABS (x, hoistingExp (IdSet.add (env, x)) e)
  end

  val hoisting = hoistingExp IdSet.empty
end
