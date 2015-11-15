structure Sinking = struct
  open Cps

  fun termSinking (t as CONST _) = t
    | termSinking (t as VAR _) = t
    | termSinking (ABS (xs, e)) = ABS (xs, expSinking e)
    | termSinking (t as PRIM _) = t

  and expSinkingAux (e as APP _) = (IdSet.empty, fn c => c e)
    | expSinkingAux (LET_REC (bindings, e)) =
        let
          val bindings' = map (fn (x, t) => (x, termSinking t)) bindings
          val (fv, cont) = expSinkingAux e
        in
          if IdSet.isEmpty (IdSet.intersection
            (fv, IdSet.fromList (map #1 bindings'))) then
            (fv, fn c => cont (fn e => c (LET_REC (bindings', e))))
          else
            (IdSet.union (fv, bindingsFreeVar bindings'),
             fn c => LET_REC (bindings', cont c))
        end
    | expSinkingAux (IF (x, e1, e2)) =
        (IdSet.singleton x,
         fn c => IF (x, expSinking (c e1), expSinking (c e2)))

  and expSinking e = (#2 (expSinkingAux e)) (fn e => e)

  val sinking = expSinking
end
