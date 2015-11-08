structure Alpha = struct
  open Cps

  (* beta reductions *)
  fun alphaConversionId env x =
    getOpt (Env.find (env, x), x)

  fun alphaConversionTerm env (t as CONST _) = t
    | alphaConversionTerm env (VAR x) = VAR (alphaConversionId env x)
    | alphaConversionTerm env (ABS ((x, k), e)) =
        let
          val x' = Id.gensym "x"
          val k' = Id.gensym "k"
          val env' = Env.insertList (env, [(x, x'), (k, k')])
        in
          ABS ((x', k'), alphaConversionExp env' e)
        end
    | alphaConversionTerm env (ABS_CONT (x, e)) =
        let
          val x' = Id.gensym "x"
          val env' = Env.insert (env, x, x')
        in
          ABS_CONT (x', alphaConversionExp env' e)
        end
    | alphaConversionTerm env (PRIM (p, xs)) =
        PRIM (p, map (alphaConversionId env) xs)

  and alphaConversionExp env (APP ((x, y), k)) =
        APP ((alphaConversionId env x, alphaConversionId env y), alphaConversionId env k)
    | alphaConversionExp env (APP_TAIL (x, y)) =
        APP_TAIL (alphaConversionId env x, alphaConversionId env y)
    | alphaConversionExp env (LET ((x, t), e)) =
        let
          val x' = Id.gensym "x"
          val env' = Env.insert (env, x, x')
        in
          LET ((x', alphaConversionTerm env t), alphaConversionExp env' e)
        end
    | alphaConversionExp env (LET_REC ((x, t), e)) =
        let
          val x' = Id.gensym "x"
          val env' = Env.insert (env, x, x')
        in
          LET_REC ((x', alphaConversionTerm env' t), alphaConversionExp env' e)
        end
    | alphaConversionExp env (IF (x, e1, e2)) =
        IF (alphaConversionId env x, alphaConversionExp env e1, alphaConversionExp env e2)

  val alphaConversion = alphaConversionExp
end
