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
    | inliningExp env (LET ((x, t), e)) =
        let val t' = inliningTerm env t in
          if sizeOfTerm t' <= threshold then
            LET ((x, t'), inliningExp (Env.insert (env, x, t')) e)
          else
            LET ((x, t'), inliningExp env e)
        end
    | inliningExp env (LET_REC ((x, t), e)) =
        let val t' = inliningTerm env t in
          if sizeOfTerm t' <= threshold then
            LET ((x, t'), inliningExp (Env.insert (env, x, t')) e)
          else
            LET ((x, t'), inliningExp env e)
        end
    | inliningExp env (IF (x, e1, e2)) =
        IF (x, inliningExp env e1, inliningExp env e2)

  val inlining = inliningExp
end
