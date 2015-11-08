structure Eta = struct
  open Cps

  exception EtaReduction

  (* eta reductions *)
  fun isRedex x (APP ((_, _), x')) = x = x'
    | isRedex x (APP_TAIL (_, x')) = x = x'
    | isRedex x (LET ((_, t), n)) =
        isRedex x n andalso not (IdSet.member (Cps.freeVarOfTerm t, x))
    | isRedex x (LET_REC ((_, t), n)) =
        isRedex x n andalso not (IdSet.member (Cps.freeVarOfTerm t, x))
    | isRedex x (IF (_, m, n)) = isRedex x m andalso isRedex x n

  fun reductionBody (APP ((x, y), _)) = APP_TAIL (x, y)
    | reductionBody (APP_TAIL _) = raise EtaReduction
    | reductionBody (LET ((x, t), m)) = LET ((x, t), reductionBody m)
    | reductionBody (LET_REC ((x, t), m)) = LET_REC ((x, t), reductionBody m)
    | reductionBody (IF (x, m, n)) = IF (x, reductionBody m, reductionBody n)

  fun etaReductionTerm (t as CONST _) = t
    | etaReductionTerm (t as VAR _) = t
    | etaReductionTerm (t as ABS_CONT (x, APP_TAIL (k', x'))) =
        if x = x' then VAR k' else t
    | etaReductionTerm (ABS ((x, k), e)) =
        if isRedex k e then ABS_CONT (x, etaReductionExp (reductionBody e))
        else ABS ((x, k), etaReductionExp e)
    | etaReductionTerm (ABS_CONT (x, e)) = ABS_CONT (x, etaReductionExp e)
    | etaReductionTerm (t as PRIM _) = t

  and etaReductionExp (t as APP _) = t
    | etaReductionExp (t as APP_TAIL _) = t
    | etaReductionExp (LET ((x, t), e)) =
        LET ((x, etaReductionTerm t), etaReductionExp e)
    | etaReductionExp (LET_REC ((x, t), e)) =
        LET_REC ((x, etaReductionTerm t), etaReductionExp e)
    | etaReductionExp (IF (x, e1, e2)) =
        IF (x, etaReductionExp e1, etaReductionExp e2)

  val etaReduction = etaReductionExp
end
