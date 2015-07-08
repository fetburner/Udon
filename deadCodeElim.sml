structure DeadCodeElim = struct
  open Cps

  datatype binding =
      LET_BINDING of term
    | LET_REC_BINDING of term
    | LET_CONT_BINDING of cont

  fun freeVarOfValue (CONST _) = IdSet.empty
    | freeVarOfValue (VAR x) = IdSet.singleton x

  fun freeVarOfValueSeq vs =
    foldl IdSet.union IdSet.empty (map freeVarOfValue vs)

  (* dead code eliminations *)
  fun deadCodeElimTerm (t as (VAL v)) =
        (freeVarOfValue v, t)
    | deadCodeElimTerm (ABS ((xs, k), e)) =
        let
          val (freevar1, e') = deadCodeElimExp e
          val freevar = IdSet.subtractList (freevar1, k :: xs)
        in
          (freevar, ABS ((xs, k), e'))
        end
    | deadCodeElimTerm (t as PRIM (_, vs)) =
        (freeVarOfValueSeq vs, t)

  and deadCodeElimExp (APP (x, (vs, c))) =
        let
          val freevar1 = IdSet.add (freeVarOfValueSeq vs, x)
          val (freevar2, c') = deadCodeElimCont c
          val freevar = IdSet.union (freevar1, freevar2)
        in
          (freevar, APP (x, (vs, c')))
        end
    | deadCodeElimExp (e as (APP_TAIL (k, v))) =
        (IdSet.add (freeVarOfValue v, k), e)
    | deadCodeElimExp (LET ((x, t), e)) =
        let
          val (freevar1, t') = deadCodeElimTerm t
          val (freevar2, e') = deadCodeElimExp e
          val freevar = IdSet.subtract (IdSet.union (freevar1, freevar2), x)
        in
          (freevar,
            if IdSet.member (freevar2, x) then LET ((x, t'), e')
            else e')
        end
    | deadCodeElimExp (LET_REC ((x, t), e)) =
        let
          val (freevar1, t') = deadCodeElimTerm t
          val (freevar2, e') = deadCodeElimExp e
          val freevar = IdSet.subtract (IdSet.union (freevar1, freevar2), x)
        in
          (freevar,
            if IdSet.member (freevar2, x) then LET_REC ((x, t'), e')
            else e')
        end
    | deadCodeElimExp (LET_CONT ((k, c), e)) =
        let
          val (freevar1, c') = deadCodeElimCont c
          val (freevar2, e') = deadCodeElimExp e
          val freevar = IdSet.subtract (IdSet.union (freevar1, freevar2), k)
        in
          (freevar,
            if IdSet.member (freevar2, k) then LET_CONT ((k, c'), e')
            else e')
        end
    | deadCodeElimExp (IF (v, e1, e2)) =
        let
          val (freevar1, e1') = deadCodeElimExp e1
          val (freevar2, e2') = deadCodeElimExp e2
          val freevar =
            IdSet.difference (IdSet.union (freevar1, freevar2), freeVarOfValue v)
        in
          (freevar, IF (v, e1', e2'))
        end

  and deadCodeElimCont (c as (CVAR k)) =
        (IdSet.singleton k, c)
    | deadCodeElimCont (CABS (x, e)) =
        let
          val (freevar1, e') = deadCodeElimExp e
          val freevar = IdSet.subtract (freevar1, x)
        in
          (freevar, CABS (x, e'))
        end

  val deadCodeElim = #2 o deadCodeElimExp
end
