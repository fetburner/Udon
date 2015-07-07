structure ConstFold = struct
  open Cps

  (* auxiliary functions *)
  fun findConst env (CONST c) = SOME c
    | findConst env (VAR x) =
        case Env.find (env, x) of
             SOME (VAL (CONST c)) => SOME c
           | _ => NONE

  fun findTuple env (CONST _) = NONE
    | findTuple env (VAR x) =
        case Env.find (env, x) of
             SOME (PRIM (Prim.TUPLE, vs)) => SOME vs
           | _ => NONE

  (* constant foldings *)
  fun constFoldValue env v =
    getOpt (Option.map CONST (findConst env v), v)

  fun constFoldValueSeq env vs =
    map (constFoldValue env) vs

  fun constFoldTerm env (VAL v) =
        VAL (constFoldValue env v)
    | constFoldTerm env (ABS ((xs, k), e)) =
        ABS ((xs, k), constFoldExp env e)
    | constFoldTerm env (t as (PRIM (p, vs))) =
        let
          val vs' = constFoldValueSeq env vs
          val t' = PRIM (p, vs')
        in
          case (p, vs') of
               (Prim.PLUS, [CONST (Const.INT m), CONST (Const.INT n)]) =>
                 VAL (CONST (Const.INT (m + n)))
             | (Prim.MINUS, [CONST (Const.INT m), CONST (Const.INT n)]) =>
                 VAL (CONST (Const.INT (m - n)))
             | (Prim.TIMES, [CONST (Const.INT m), CONST (Const.INT n)]) =>
                 VAL (CONST (Const.INT (m * n)))
             | (Prim.LE, [CONST (Const.INT m), CONST (Const.INT n)]) =>
                 VAL (CONST (Const.BOOL (m <= n)))
             | (Prim.TUPLE_GET n, [v]) =>
                 getOpt
                   (Option.map (fn vs => VAL (List.nth (vs, n - 1))) 
                     (findTuple env v), t')
             | _ => t'
        end

  and constFoldExp env (APP (x, (vs, c))) =
        APP (x, (constFoldValueSeq env vs, constFoldCont env c))
    | constFoldExp env (APP_TAIL (k, v)) =
        APP_TAIL (k, constFoldValue env v)
    | constFoldExp env (LET ((x, t), e)) =
        let
          val t' = constFoldTerm env t
          val e' = constFoldExp (Env.insert (env, x, t')) e
        in
          LET ((x, t'), e')
        end
    | constFoldExp env (LET_REC ((x, t), e)) =
        LET_REC ((x, constFoldTerm env t), constFoldExp env e)
    | constFoldExp env (LET_CONT ((k, c), e)) =
        LET_CONT ((k, constFoldCont env c), constFoldExp env e)
    | constFoldExp env (IF (v, e1, e2)) =
        (case constFoldValue env v of
              CONST (Const.BOOL true) =>
                constFoldExp env e1
            | CONST (Const.BOOL false) =>
                constFoldExp env e2
            | v' as (VAR x) =>
                IF
                  (v',
                   constFoldExp
                     (Env.insert (env, x, VAL (CONST (Const.BOOL true))))
                     e1,
                   constFoldExp
                     (Env.insert (env, x, VAL (CONST (Const.BOOL false))))
                     e2))

  and constFoldCont env (c as (CVAR _)) = c
    | constFoldCont env (CABS (x, e)) =
        CABS (x, constFoldExp env e)

  val constFold = constFoldExp
end
