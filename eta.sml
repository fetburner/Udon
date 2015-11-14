structure Eta = struct
  open Cps

  (* eta reductions *)
  fun termEtaReduction (t as CONST _) = t
    | termEtaReduction (t as VAR _) = t
    | termEtaReduction (t as ABS ((x, k), APP ((f, x'), k'))) =
        if x = x' andalso k = k' then VAR f else t
    | termEtaReduction (t as ABS_CONT (x, APP_TAIL (k', x'))) =
        if x = x' then VAR k' else t
    | termEtaReduction (ABS ((x, k), e)) = ABS ((x, k), expEtaReduction e)
    | termEtaReduction (ABS_CONT (x, e)) = ABS_CONT (x, expEtaReduction e)
    | termEtaReduction (t as PRIM _) = t

  and expEtaReduction (t as APP _) = t
    | expEtaReduction (t as APP_TAIL _) = t
    | expEtaReduction (LET_REC (bindings, e)) =
        LET_REC
          (map (fn (x, t) => (x, termEtaReduction t)) bindings, expEtaReduction e)
    | expEtaReduction (IF (x, e1, e2)) =
        IF (x, expEtaReduction e1, expEtaReduction e2)

  val etaReduction = expEtaReduction
end
