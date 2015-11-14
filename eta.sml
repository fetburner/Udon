structure Eta = struct
  open Cps

  (* eta reductions *)
  fun etaReductionTerm (t as CONST _) = t
    | etaReductionTerm (t as VAR _) = t
    | etaReductionTerm (t as ABS ((x, k), APP ((f, x'), k'))) =
        if x = x' andalso k = k' then VAR f else t
    | etaReductionTerm (t as ABS_CONT (x, APP_TAIL (k', x'))) =
        if x = x' then VAR k' else t
    | etaReductionTerm (ABS ((x, k), e)) = ABS ((x, k), etaReductionExp e)
    | etaReductionTerm (ABS_CONT (x, e)) = ABS_CONT (x, etaReductionExp e)
    | etaReductionTerm (t as PRIM _) = t

  and etaReductionExp (t as APP _) = t
    | etaReductionExp (t as APP_TAIL _) = t
    | etaReductionExp (LET_REC (bindings, e)) =
        LET_REC
          (map (fn (x, t) => (x, etaReductionTerm t)) bindings, etaReductionExp e)
    | etaReductionExp (IF (x, e1, e2)) =
        IF (x, etaReductionExp e1, etaReductionExp e2)

  val etaReduction = etaReductionExp
end
