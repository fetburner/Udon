structure DeadCodeElim = struct
  open Cps

  (* dead code eliminations *)
  fun deadCodeElimTerm (t as VAL _) = t
    | deadCodeElimTerm (ABS ((xs, k), e)) = ABS ((xs, k), deadCodeElimExp e)
    | deadCodeElimTerm (t as PRIM _) = t

  and deadCodeElimExp (APP (x, (vs, c))) = APP (x, (vs, deadCodeElimCont c))
    | deadCodeElimExp (e as APP_TAIL _) = e
    | deadCodeElimExp (LET ((x, t), e)) =
        let
          val t' = deadCodeElimTerm t
          val e' = deadCodeElimExp e
        in
          if IdSet.member (freeVarOfExp e, x) then LET ((x, t'), e')
          else e'
        end
    | deadCodeElimExp (LET_REC ((x, t), e)) =
        let
          val t' = deadCodeElimTerm t
          val e' = deadCodeElimExp e
        in
          if IdSet.member (freeVarOfExp e, x) then LET_REC ((x, t'), e')
          else e'
        end
    | deadCodeElimExp (LET_CONT ((k, c), e)) =
        let
          val c' = deadCodeElimCont c
          val e' = deadCodeElimExp e
        in
          if IdSet.member (freeVarOfCont c, k) then LET_CONT ((k, c'), e')
          else e'
        end
    | deadCodeElimExp (IF (v, e1, e2)) =
        IF (v, deadCodeElimExp e1, deadCodeElimExp e2)

  and deadCodeElimCont (c as CVAR _) = c
    | deadCodeElimCont (CABS (x, e)) = CABS (x, deadCodeElimExp e)

  val deadCodeElim = deadCodeElimExp
end
