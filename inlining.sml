functor InliningFun (P : sig val threshold : int end) = struct
  open P
  open Cps

  (* inlinings *)
  fun inliningTerm env (t as CONST _) = t
    | inliningTerm env (t as VAR _) = t
    | inliningTerm env (ABS ((x, k), e)) =
        ABS ((x, k), inliningExp env e)
    | inliningTerm env (ABS_CONT (x, e)) =
        ABS_CONT (x, inliningExp env e)
    | inliningTerm env (t as PRIM _) = t

  and inliningExp env (t as APP ((x, y), k)) =
        (case Env.find (env, x) of
              SOME (ABS ((y', k'), e)) =>
                Alpha.alphaConversion (Env.fromList [(y', y), (k', k)]) e
            | _ => t)
    | inliningExp env (t as APP_TAIL (x, y)) =
        (case Env.find (env, x) of
              SOME (ABS_CONT (y', e)) =>
                Alpha.alphaConversion (Env.fromList [(y', y)]) e
            | _ => t)
    | inliningExp env (LET_REC (bindings, e)) =
        let
          val bindings' = map (fn (x, t) => (x, inliningTerm env t)) bindings
          val env' = Env.insertList (env,
            List.mapPartial (fn (x, t') =>
              if sizeOfTerm t' <= threshold then SOME (x, t')
              else NONE) bindings')
        in
          LET_REC (bindings', inliningExp env' e)
        end
    | inliningExp env (IF (x, e1, e2)) =
        IF (x, inliningExp env e1, inliningExp env e2)

  val inlining = inliningExp
end
