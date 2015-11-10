structure Sinking = struct
  open Cps

  fun sinkingTerm (t as CONST _) = t
    | sinkingTerm (t as VAR _) = t
    | sinkingTerm (ABS ((x, k), e)) = ABS ((x, k), sinkingExp e)
    | sinkingTerm (ABS_CONT (x, e)) = ABS_CONT (x, sinkingExp e)
    | sinkingTerm (t as PRIM _) = t

  and sinkingExpAux (e as APP _) = (IdSet.empty, fn c => c e)
    | sinkingExpAux (e as APP_TAIL _) = (IdSet.empty, fn c => c e)
    | sinkingExpAux (LET_REC ((x, t), e)) =
        let
          val t' = sinkingTerm t
          val (fv, cont) = sinkingExpAux e
        in
          if IdSet.member (fv, x) then
            (IdSet.union (fv, freeVarOfBinding (x, t')),
             fn c => LET_REC ((x, t'), cont c))
          else
            (fv, fn c => cont (fn e => c (LET_REC ((x, t'), e))))
        end
    | sinkingExpAux (IF (x, e1, e2)) =
        (IdSet.singleton x,
         fn c => IF (x, sinkingExp (c e1), sinkingExp (c e2)))

  and sinkingExp e = (#2 (sinkingExpAux e)) (fn e => e)

  val sinking = sinkingExp
end
