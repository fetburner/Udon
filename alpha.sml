structure Alpha = struct
  open Cps

  (* beta reductions *)
  fun idAlphaConversion env x =
    getOpt (Env.find (env, x), x)

  fun termAlphaConversion env (t as CONST _) = t
    | termAlphaConversion env (VAR x) = VAR (idAlphaConversion env x)
    | termAlphaConversion env (ABS ((x, k), e)) =
        let
          val x' = Id.gensym "x"
          val k' = Id.gensym "k"
          val env' = Env.insertList (env, [(x, x'), (k, k')])
        in
          ABS ((x', k'), expAlphaConversion env' e)
        end
    | termAlphaConversion env (ABS_CONT (x, e)) =
        let
          val x' = Id.gensym "x"
          val env' = Env.insert (env, x, x')
        in
          ABS_CONT (x', expAlphaConversion env' e)
        end
    | termAlphaConversion env (PRIM (p, xs)) =
        PRIM (p, map (idAlphaConversion env) xs)

  and expAlphaConversion env (APP ((x, y), k)) =
        APP ((idAlphaConversion env x, idAlphaConversion env y), idAlphaConversion env k)
    | expAlphaConversion env (APP_TAIL (x, y)) =
        APP_TAIL (idAlphaConversion env x, idAlphaConversion env y)
    | expAlphaConversion env (LET_REC (bindings, e)) =
        let
          val x' = Id.gensym "x"
          val bindings' = map (fn x => (x, Id.gensym "x")) bindings
          val env' = Env.insertList (env, map (fn ((x, _), x') => (x, x')) bindings')
        in
          LET_REC
            (map (fn ((_, t), x') => (x', termAlphaConversion env' t)) bindings',
             expAlphaConversion env' e)
        end
    | expAlphaConversion env (IF (x, e1, e2)) =
        IF (idAlphaConversion env x, expAlphaConversion env e1, expAlphaConversion env e2)

  val alphaConversion = expAlphaConversion
end
