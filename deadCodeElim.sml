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
    | deadCodeElimExp (LET_REC ((x, t), e)) =
        let val e' = deadCodeElimExp e in
          if IdSet.member (freeVarOfExp e', x) then
            LET_REC ((x, deadCodeElimTerm t), e')
          else e'
        end
    | deadCodeElimExp (IF (v, e1, e2)) =
        IF (v, deadCodeElimExp e1, deadCodeElimExp e2)

  val deadCodeElim = deadCodeElimExp
end
