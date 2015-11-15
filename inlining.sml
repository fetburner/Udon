functor InliningFun (P : sig val threshold : int end) = struct
  open P
  open Cps

  (* inlinings *)
  fun termInlining env (t as CONST _) = t
    | termInlining env (t as VAR _) = t
    | termInlining env (ABS (xs, e)) =
        ABS (xs, expInlining env e)
    | termInlining env (t as PRIM _) = t

  and expInlining env (t as APP (x, ys)) =
        (case Env.find (env, x) of
              SOME (ABS (ys', e)) =>
                Alpha.alphaConversion (Env.fromList (ListPair.zipEq (ys', ys))) e
            | _ => t)
    | expInlining env (LET_REC (bindings, e)) =
        let
          val env' = Env.insertList (env,
            List.mapPartial (fn (x, t) =>
              if termSize t <= threshold then SOME (x, t)
              else NONE) bindings)
          val bindings' = map (fn (x, t) => (x, termInlining env' t)) bindings
        in
          LET_REC (bindings', expInlining env' e)
        end
    | expInlining env (IF (x, e1, e2)) =
        IF (x, expInlining env e1, expInlining env e2)

  val inlining = expInlining
end
