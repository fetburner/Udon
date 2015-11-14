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

  fun termConstFold env (t as CONST _) = t
    | termConstFold env (t as VAR _) = t
    | termConstFold env (ABS ((x, k), e)) =
        ABS ((x, k), expConstFold env e)
    | termConstFold env (ABS_CONT (x, e)) =
        ABS_CONT (x, expConstFold env e)
    | termConstFold env (t as PRIM (Prim.PLUS, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m + n))
            | _ => t)
    | termConstFold env (t as PRIM (Prim.MINUS, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m - n))
            | _ => t)
    | termConstFold env (t as PRIM (Prim.TIMES, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.INT (m * n))
            | _ => t)
    | termConstFold env (t as PRIM (Prim.LE, xs)) =
        (case map (findInt env) xs of
              [ SOME m, SOME n ] => CONST (Const.BOOL (m <= n))
            | _ => t)
    | termConstFold env (t as PRIM (Prim.TUPLE_GET n, [x])) =
        getOpt
          (Option.map (fn xs => VAR (List.nth (xs, n - 1)))
            (findTuple env x), t)
    | termConstFold env (t as PRIM _) = t

  and expConstFold env (e as APP _) = e
    | expConstFold env (e as APP_TAIL _) = e
    | expConstFold env (LET_REC (bindings, e)) =
        let
          val bindings' = map (fn (x, t) => (x, termConstFold env t)) bindings
          val e' = expConstFold (Env.insertList (env, bindings')) e
        in
          LET_REC (bindings', e')
        end
    | expConstFold env (IF (x, e1, e2)) =
        (case findBool env x of
              SOME true => expConstFold env e1
            | SOME false => expConstFold env e2
            | NONE =>
                IF (x,
                  expConstFold (Env.insert (env, x, CONST (Const.BOOL true))) e1,
                  expConstFold (Env.insert (env, x, CONST (Const.BOOL false))) e2))

  val constFold = expConstFold
end
