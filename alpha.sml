structure Alpha = struct
  open Cps

  (* alpha conversions *)
  fun alphaConvId env x =
    getOpt (Env.find (env, x), x)

  fun alphaConvValue env (c as (CONST _)) = c
    | alphaConvValue env (VAR x) =
        VAR (alphaConvId env x)

  fun alphaConvValueSeq env vs =
    map (alphaConvValue env) vs

  fun alphaConvTerm env (VAL v) =
        VAL (alphaConvValue env v)
    | alphaConvTerm env (ABS ((xs, k), e)) =
        ABS ((xs, k), alphaConvExp env e)
    | alphaConvTerm env (PRIM (p, vs)) =
        PRIM (p, alphaConvValueSeq env vs)

  and alphaConvExp env (APP (f, (vs, c))) =
        let
          val f' = alphaConvId env f
          val vs' = alphaConvValueSeq env vs
          val c' = alphaConvCont env c
        in
          APP (f', (vs', c'))
        end
    | alphaConvExp env (APP_TAIL (k, v)) =
        let
          val k' = alphaConvId env k
          val v' = alphaConvValue env v
        in
          APP_TAIL (k', v')
        end
    | alphaConvExp env (LET ((x, t), e)) =
        (case alphaConvTerm env t of
             VAL (VAR (y)) =>
               alphaConvExp (Env.insert (env, x, y)) e
           | t' =>
               LET ((x, t'), alphaConvExp env e))
    | alphaConvExp env (LET_REC ((x, t), e)) =
        LET_REC ((x, alphaConvTerm env t), alphaConvExp env e)
    | alphaConvExp env (LET_CONT ((k, c), e)) =
        (case alphaConvCont env c of
              CVAR k' =>
                alphaConvExp (Env.insert (env, k, k')) e
            | c' =>
                LET_CONT ((k, c'), alphaConvExp env e))
    | alphaConvExp env (IF (v, e1, e2)) =
        let
          val v' = alphaConvValue env v
          val e1' = alphaConvExp env e1
          val e2' = alphaConvExp env e2
        in
          IF (v', e1', e2')
        end

  and alphaConvCont env (CVAR k) = CVAR (alphaConvId env k)
    | alphaConvCont env (CABS (x, e)) =
        CABS (x, alphaConvExp env e)

  val alphaConv = alphaConvExp
end
