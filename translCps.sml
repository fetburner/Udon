structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

  (* main translation function *)
  fun transl (E (_, CONST c, _)) (Cps.CVAR k) =
        Cps.APP_TAIL (k, Cps.CONST c)
    | transl (E (_, CONST c, _)) (Cps.CABS (x, e)) =
        Cps.LET ((x, Cps.VAL (Cps.CONST c)), e)
    | transl (E (_, VAR x, _)) (Cps.CVAR k) =
        Cps.APP_TAIL (k, Cps.VAR x)
    | transl (E (_, VAR x, _)) (Cps.CABS (y, e)) =
        Cps.LET ((y, Cps.VAL (Cps.VAR x)), e)
    | transl (E (_, IF (m, n1, n2), _)) (c as (Cps.CVAR _)) =
        translIf (m, n1, n2) c
    | transl (E (_, IF (m, n1, n2), _)) (c as (Cps.CABS _)) =
        let
          val k = Id.gensym "if_label"
        in
          Cps.LET_CONT
            ((k, c),
             translIf (m, n1, n2) (Cps.CVAR k))
        end
    | transl (E (_, ABS (xs, m), _)) (Cps.CVAR k) =
        let
          val x = Id.gensym "fn"
        in
          translAbs x false (map #1 xs) m (Cps.APP_TAIL (k, Cps.VAR x))
        end
    | transl (E (_, ABS (xs, m), _)) (Cps.CABS (x, e)) =
        translAbs x false (map #1 xs) m e
    | transl (E (_, APP (m, ns), _)) c =
        translApp m ns c
    | transl (E (_, LET (d, m), _)) c =
        translLet d m c
    | transl (E (_, TUPLE ms, _)) (Cps.CVAR k) =
        let
          val x = Id.gensym "tuple"
        in
          translTuple ms x (Cps.APP_TAIL (k, Cps.VAR x))
        end
    | transl (E (_, TUPLE ms, _)) (Cps.CABS (x, e)) =
        translTuple ms x e
    | transl (E (_, CASE (m, xs, n), _)) c =
        translCase m xs n c

  and translIf (m, n1, n2) c =
      let
        val x = Id.gensym "if_cond"
      in
        transl m
          (Cps.CABS
            (x,
             Cps.IF
               (Cps.VAR x,
                transl n1 c,
                transl n2 c)))
      end

  and translAbs name recflag ids body e =
      let
        val k = Id.gensym "k"
        val binding =
          (name, Cps.ABS ((ids, k), transl body (Cps.CVAR k)))
      in
        if recflag then Cps.LET_REC (binding, e)
        else Cps.LET (binding, e)
      end

  and translCase e1 ids e2 cont =
      let
        val newId = Id.gensym "t"
        fun loop exp cont n [] = transl exp cont
          | loop exp cont n (id :: ids) =
              Cps.LET
                ((id, Cps.PRIM (Prim.TUPLE_GET n, [Cps.VAR newId])),
                 loop exp cont (n + 1) ids)
          val exp = loop e2 cont 1 (map #1 ids)
      in
        transl e1 (Cps.CABS (newId, exp))
      end

  and translLet [] exp cont = transl exp cont
    | translLet (dec::decs) exp cont =
      case dec of
         VAL (f, exp') =>
           transl exp' (Cps.CABS (#1 f, translLet decs exp cont))
       | VALREC (f, E (_, ABS (ids, body), _)) =>
         let
           val ids = map #1 ids
         in
           translAbs (#1 f) true ids body (translLet decs exp cont)
         end
       | VALREC _ => raise (Fail "translLet: VALREC")

  and translApp funcExp argsExp cont =
      let
        val funcId = Id.gensym "fn"
      in
        transl funcExp
          (Cps.CABS
            (funcId,
             translExpSeq argsExp [] (fn ids =>
               Cps.APP (funcId, (ids, cont)))))
      end

  and translTuple ms x e =
        translExpSeq ms [] (fn ids =>
          Cps.LET
            ((x, Cps.PRIM (Prim.TUPLE, ids)), e))

  and translExpSeq [] ids body = body (rev ids)
    | translExpSeq (e :: exps) ids body =
        let
          val id = Id.gensym "e"
        in
          transl e
            (Cps.CABS (id, translExpSeq exps (Cps.VAR id :: ids) body))
        end

end
