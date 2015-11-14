functor InliningFun (P : sig val threshold : int end) = struct
  open P
  open Cps

  (* inlinings *)
  fun termInlining env (t as CONST _) = t
    | termInlining env (t as VAR _) = t
    | termInlining env (ABS ((x, k), e)) =
        ABS ((x, k), expInlining env e)
    | termInlining env (ABS_CONT (x, e)) =
        ABS_CONT (x, expInlining env e)
    | termInlining env (t as PRIM _) = t

  and expInlining env (t as APP ((x, y), k)) =
        (case Env.find (env, x) of
              SOME (ABS ((y', k'), e)) =>
                Alpha.alphaConversion (Env.fromList [(y', y), (k', k)]) e
            | _ => t)
    | expInlining env (t as APP_TAIL (x, y)) =
        (case Env.find (env, x) of
              SOME (ABS_CONT (y', e)) =>
                Alpha.alphaConversion (Env.fromList [(y', y)]) e
            | _ => t)
    | expInlining env (LET_REC (bindings, e)) =
        let
          val bindings' = map (fn (x, t) => (x, termInlining env t)) bindings
          val env' = Env.insertList (env,
            List.mapPartial (fn (x, t') =>
              if termSize t' <= threshold then SOME (x, t')
              else NONE) bindings')
        in
          LET_REC (bindings', expInlining env' e)
        end
    | expInlining env (IF (x, e1, e2)) =
        IF (x, expInlining env e1, expInlining env e2)

  val inlining = expInlining
end
