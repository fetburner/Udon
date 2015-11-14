structure Beta = struct
  open Cps

  (* beta reductions *)
  fun idBetaReduction env x =
    getOpt (Env.find (env, x), x)

  fun termBetaReduction env (t as CONST _) = t
    | termBetaReduction env (VAR x) = VAR (idBetaReduction env x)
    | termBetaReduction env (ABS ((x, k), e)) =
        ABS ((x, k), expBetaReduction env e)
    | termBetaReduction env (ABS_CONT (x, e)) =
        ABS_CONT (x, expBetaReduction env e)
    | termBetaReduction env (PRIM (p, xs)) =
        PRIM (p, map (idBetaReduction env) xs)

  and expBetaReduction env (APP ((x, y), k)) =
        APP ((idBetaReduction env x, idBetaReduction env y), idBetaReduction env k)
    | expBetaReduction env (APP_TAIL (x, y)) =
        APP_TAIL (idBetaReduction env x, idBetaReduction env y)
    | expBetaReduction env (LET_REC (bindings, e)) =
        let
          val bindings' =
            map (fn (x, t) => (x, termBetaReduction env t)) bindings
          val env' =
            Env.insertList
              (env,
               List.mapPartial
                 (fn (x, VAR y) => SOME (x, y) | _ => NONE) bindings')
        in
          LET_REC (bindings', expBetaReduction env' e)
        end
    | expBetaReduction env (IF (x, e1, e2)) =
        IF
          (idBetaReduction env x,
           expBetaReduction env e1,
           expBetaReduction env e2)

  val betaReduction = expBetaReduction
end
