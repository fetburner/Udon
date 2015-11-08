structure ConstFold = struct
  open Cps

  (* auxiliary functions *)
  fun findInt env x =
    case Env.find (env, x) of
         SOME (CONST (Const.INT n)) => SOME n
       | _ => NONE

  fun findBool env x =
    case Env.find (env, x) of
         SOME (CONST (Const.BOOL b)) => SOME b
       | _ => NONE

  fun findTuple env x =
    case Env.find (env, x) of
         SOME (PRIM (Prim.TUPLE, xs)) => SOME xs
       | _ => NONE

  fun constFoldTerm env (t as CONST _) = t
    | constFoldTerm env (t as VAR _) = t
    | constFoldTerm env (ABS ((x, k), e)) =
        ABS ((x, k), constFoldExp env e)
    | constFoldTerm env (ABS_CONT (x, e)) =
        ABS_CONT (x, constFoldExp env e)
    | constFoldTerm env (t as PRIM (Prim.PLUS, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m + n))
            | _ => t)
    | constFoldTerm env (t as PRIM (Prim.MINUS, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m - n))
            | _ => t)
    | constFoldTerm env (t as PRIM (Prim.TIMES, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m * n))
            | _ => t)
    | constFoldTerm env (t as PRIM (Prim.LE, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.BOOL (m <= n))
            | _ => t)
    | constFoldTerm env (t as PRIM (Prim.TUPLE_GET n, [x])) =
        getOpt
          (Option.map (fn xs => VAR (List.nth (xs, n - 1)))
            (findTuple env x), t)
    | constFoldTerm env (t as PRIM _) = t

  and constFoldExp env (e as APP _) = e
    | constFoldExp env (e as APP_TAIL _) = e
    | constFoldExp env (LET ((x, t), e)) =
        let
          val t' = constFoldTerm env t
          val e' = constFoldExp (Env.insert (env, x, t')) e
        in
          LET ((x, t'), e')
        end
    | constFoldExp env (LET_REC ((x, t), e)) =
        let
          val t' = constFoldTerm env t
          val e' = constFoldExp (Env.insert (env, x, t')) e
        in
          LET_REC ((x, t'), e')
        end
    | constFoldExp env (IF (x, e1, e2)) =
        (case findBool env x of
              SOME true => constFoldExp env e1
            | SOME false => constFoldExp env e2
            | NONE =>
                IF (x,
                  constFoldExp (Env.insert (env, x, CONST (Const.BOOL true))) e1,
                  constFoldExp (Env.insert (env, x, CONST (Const.BOOL false))) e2))

  val constFold = constFoldExp
end
