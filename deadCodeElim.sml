structure DeadCodeElim = struct
  open Cps

  (* dead code eliminations *)
  fun termDeadCodeElim (t as CONST _) = t
    | termDeadCodeElim (t as VAR _) = t
    | termDeadCodeElim (ABS (xs, e)) = ABS (xs, expDeadCodeElim e)
    | termDeadCodeElim (t as PRIM _) = t

  and expDeadCodeElim (e as APP _) = e
    | expDeadCodeElim (LET_REC (bindings, e)) =
        let val e' = expDeadCodeElim e in
          if IdSet.isEmpty (IdSet.intersection (expFreeVar e',
              IdSet.fromList (map #1 bindings))) then e'
          else LET_REC (map (fn (x, t) => (x, termDeadCodeElim t)) bindings, e')
        end
    | expDeadCodeElim (IF (x, e1, e2)) =
        IF (x, expDeadCodeElim e1, expDeadCodeElim e2)

  val deadCodeElim = expDeadCodeElim
end
