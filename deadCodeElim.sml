structure DeadCodeElim = struct
  open Cps

  (* dead code eliminations *)
  fun deadCodeElimTerm (t as CONST _) = t
    | deadCodeElimTerm (t as VAR _) = t
    | deadCodeElimTerm (ABS ((x, k), e)) = ABS ((x, k), deadCodeElimExp e)
    | deadCodeElimTerm (ABS_CONT (x, e)) = ABS_CONT (x, deadCodeElimExp e)
    | deadCodeElimTerm (t as PRIM _) = t

  and deadCodeElimExp (e as APP _) = e
    | deadCodeElimExp (e as APP_TAIL _) = e
    | deadCodeElimExp (LET_REC (bindings, e)) =
        let val e' = deadCodeElimExp e in
          if IdSet.isEmpty (IdSet.intersection (freeVarOfExp e',
              IdSet.fromList (map #1 bindings))) then e'
          else LET_REC (map (fn (x, t) => (x, deadCodeElimTerm t)) bindings, e')
        end
    | deadCodeElimExp (IF (x, e1, e2)) =
        IF (x, deadCodeElimExp e1, deadCodeElimExp e2)

  val deadCodeElim = deadCodeElimExp
end
