structure Beta = struct
  open Cps

  (* beta reductions *)
  fun betaReductionId env x =
    getOpt (Env.find (env, x), x)

  fun betaReductionTerm env (t as CONST _) = t
    | betaReductionTerm env (VAR x) = VAR (betaReductionId env x)
    | betaReductionTerm env (ABS ((x, k), e)) =
        ABS ((x, k), betaReductionExp env e)
    | betaReductionTerm env (ABS_CONT (x, e)) =
        ABS_CONT (x, betaReductionExp env e)
    | betaReductionTerm env (PRIM (p, xs)) =
        PRIM (p, map (betaReductionId env) xs)

  and betaReductionExp env (APP ((x, y), k)) =
        APP ((betaReductionId env x, betaReductionId env y), betaReductionId env k)
    | betaReductionExp env (APP_TAIL (x, y)) =
        APP_TAIL (betaReductionId env x, betaReductionId env y)
    | betaReductionExp env (LET ((x, t), e)) =
        (case betaReductionTerm env t of
             VAR (y) =>
               betaReductionExp (Env.insert (env, x, y)) e
           | t' =>
               LET ((x, t'), betaReductionExp env e))
    | betaReductionExp env (LET_REC ((x, t), e)) =
        LET_REC ((x, betaReductionTerm env t), betaReductionExp env e)
    | betaReductionExp env (IF (x, e1, e2)) =
        IF (betaReductionId env x, betaReductionExp env e1, betaReductionExp env e2)

  val betaReduction = betaReductionExp
end
