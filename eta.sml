structure Eta = struct
  open Cps

  (* eta reductions *)
  fun termEtaReduction (t as CONST _) = t
    | termEtaReduction (t as VAR _) = t
    | termEtaReduction (t as ABS (ys, APP (x, ys'))) =
        if ys = ys' then VAR x else t
    | termEtaReduction (ABS (xs, e)) = ABS (xs, expEtaReduction e)
    | termEtaReduction (t as PRIM _) = t
    | termEtaReduction (t as TUPLE _) = t
    | termEtaReduction (t as PROJ _) = t

  and expEtaReduction (t as APP _) = t
    | expEtaReduction (LET_REC (bindings, e)) =
        LET_REC
          (map (fn (x, t) => (x, termEtaReduction t)) bindings, expEtaReduction e)
    | expEtaReduction (IF (x, e1, e2)) =
        IF (x, expEtaReduction e1, expEtaReduction e2)

  val etaReduction = expEtaReduction
end
