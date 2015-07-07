structure Inlining = struct
  open Cps

  datatype abs = FUN_ABS of (Id.t list * Id.t) * exp
               | CONT_ABS of Id.t * exp

  (* size of abstract syntax tree *)
  fun sizeOfTerm acc (VAL _) = 1 + acc
    | sizeOfTerm acc (ABS (_, e)) = sizeOfExp (acc + 1) e
    | sizeOfTerm acc (PRIM _) = 1 + acc

  and sizeOfExp acc (APP (_, (_, c))) = sizeOfCont (acc + 1) c
    | sizeOfExp acc (APP_TAIL _) = acc + 1
    | sizeOfExp acc (LET ((_, t), e)) = sizeOfExp (sizeOfTerm (acc + 1) t) e
    | sizeOfExp acc (LET_REC ((_, t), e)) = sizeOfExp (sizeOfTerm (acc + 1) t) e
    | sizeOfExp acc (LET_CONT ((_, c), e)) = sizeOfExp (sizeOfCont (acc + 1) c) e
    | sizeOfExp acc (IF (_, e1, e2)) = sizeOfExp (sizeOfExp (acc + 1) e1) e2

  and sizeOfCont acc (CVAR _) = acc + 1
    | sizeOfCont acc (CABS (_, e)) = sizeOfExp (acc + 1) e

  val sizeOfTerm = sizeOfTerm 0
  val sizeOfExp = sizeOfExp 0
  val sizeOfCont = sizeOfCont 0

  (* increase count of variable *)
  fun incr count x =
    Env.insert (count, x, getOpt (Env.find (count, x), 0) + 1)

  (* increase count of variables *)
  fun incrList count xs =
    foldl (fn (x, count) => incr count x) count xs

  fun countAppearInValue count (CONST _) = count
    | countAppearInValue count (VAR x) = incr count x

  fun countAppearInValueSeq count vs =
    foldl (fn (v, count) => countAppearInValue count v) count vs

  (* inlinings *)
  fun inliningApp ((xs, k), e) (vs, c) =
    foldl (fn (binding, e) => LET (binding, e)) (LET_CONT ((k, c), e))
      (ListPair.map (fn (x, v) => (x, VAL v)) (xs, vs))

  fun inliningAppTail (x, e) v = LET ((x, VAL v), e)

  fun inliningTerm threshold count (t as (VAL v)) =
        (countAppearInValue count v, fn _ => t)
    | inliningTerm threshold count (ABS ((xs, k), e)) =
        let
          val (count, e') = inliningExp threshold count e
        in
          (count, fn inlinings => ABS ((xs, k), e' inlinings))
        end
    | inliningTerm threshold count (t as (PRIM (_, vs))) =
        (countAppearInValueSeq count vs, fn _ => t)

  and inliningExp threshold count (APP (x, (vs, c))) =
        let
          val count' = countAppearInValueSeq (incr count x) vs
          val (count'', c') = inliningCont threshold count' c
        in
          (count'', fn inlinings =>
            case Env.find (inlinings, x) of
                 SOME (FUN_ABS abs) => inliningApp abs (vs, c' inlinings)
               | _ => APP (x, (vs, c' inlinings)))
        end
    | inliningExp threshold count (e as (APP_TAIL (k, v))) =
        let
          val count' = incr (countAppearInValue count v) k
        in
          (count', fn inlinings =>
            case Env.find (inlinings, k) of
                 SOME (CONT_ABS abs) => inliningAppTail abs v
               | _ => e)
        end
    | inliningExp threshold count (LET ((x, t), e)) =
        let
          val (count', t') = inliningTerm threshold count t
          val (count'', e') = inliningExp threshold count' e
        in
          (count'', fn inlinings =>
            let 
              val inlinings' =
                case t of
                     ABS (abs as (_, body)) =>
                       if
                         Env.find (count'', x) = SOME 1
                         orelse sizeOfExp body <= threshold
                       then Env.insert (inlinings, x, FUN_ABS abs)
                       else inlinings
                   | _ => inlinings
            in
              LET ((x, t' inlinings'), e' inlinings')
            end)
        end
    | inliningExp threshold count (LET_REC ((x, t), e)) =
        let
          val (count', t') = inliningTerm threshold count t
          val (count'', e') = inliningExp threshold count' e
        in
          (count'', fn inlinings =>
            let 
              val inlinings' =
                case t of
                     ABS (abs as (_, body)) =>
                       if
                         Env.find (count'', x) = SOME 1
                         orelse sizeOfExp body <= threshold
                       then Env.insert (inlinings, x, FUN_ABS abs)
                       else inlinings
                   | _ => inlinings
            in
              LET_REC ((x, t' inlinings'), e' inlinings')
            end)
        end
    | inliningExp threshold count (LET_CONT ((k, c), e)) =
        let
          val (count', c') = inliningCont threshold count c
          val (count'', e') = inliningExp threshold count' e
        in
          (count'', fn inlinings =>
            let 
              val inlinings' =
                case c of
                     CABS (abs as (_, body)) =>
                       if
                         Env.find (count'', k) = SOME 1
                         orelse sizeOfExp body <= threshold
                       then Env.insert (inlinings, k, CONT_ABS abs)
                       else inlinings
                   | _ => inlinings
            in
              LET_CONT ((k, c' inlinings'), e' inlinings')
            end)
        end
    | inliningExp threshold count (IF (v, e1, e2)) =
        let
          val count' = countAppearInValue count v
          val (count'', e1') = inliningExp threshold count' e1
          val (count''', e2') = inliningExp threshold count'' e2
        in
          (count''', fn inlinings =>
            IF (v, e1' inlinings, e2' inlinings))
        end

  and inliningCont threshold count (c as (CVAR k)) =
        (incr count k, fn _ => c)
    | inliningCont threshold count (CABS (x, e)) =
        let
          val (count', e') = inliningExp threshold count e
        in
          (count', fn inlinings => CABS (x, e' inlinings))
        end

  fun inlining threshold e =
    (#2 (inliningExp threshold Env.empty e)) Env.empty
end
