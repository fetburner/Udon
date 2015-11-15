structure Beta = struct
  open Cps

  (* beta reductions *)
  fun idBetaReduction env x =
    getOpt (Env.find (env, x), x)

  fun termBetaReduction env (t as CONST _) = t
    | termBetaReduction env (VAR x) = VAR (idBetaReduction env x)
    | termBetaReduction env (ABS (xs, e)) =
        ABS (xs, expBetaReduction env e)
    | termBetaReduction env (PRIM (p, xs)) =
        PRIM (p, map (idBetaReduction env) xs)

  and expBetaReduction env (APP (x, ys)) =
        APP (idBetaReduction env x, map (idBetaReduction env) ys)
    | expBetaReduction env (LET_REC (bindings, e)) =
        let
          val env' =
            Env.insertList
              (env,
               List.mapPartial
                 (fn (x, VAR y) => SOME (x, idBetaReduction env y) | _ => NONE) bindings)
          val bindings' =
            map (fn (x, t) => (x, termBetaReduction env' t)) bindings
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
