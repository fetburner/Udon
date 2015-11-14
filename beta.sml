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
    | betaReductionExp env (LET_REC (bindings, e)) =
        let
          val bindings' =
            map (fn (x, t) => (x, betaReductionTerm env t)) bindings
          val env' =
            Env.insertList
              (env,
               List.mapPartial
                 (fn (x, VAR y) => SOME (x, y) | _ => NONE) bindings')
        in
          LET_REC (bindings', betaReductionExp env' e)
        end
    | betaReductionExp env (IF (x, e1, e2)) =
        IF
          (betaReductionId env x,
           betaReductionExp env e1,
           betaReductionExp env e2)

  val betaReduction = betaReductionExp
end
