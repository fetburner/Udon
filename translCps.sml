structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

  (* main translation function *)
  fun transl (CONST c, _) cont =
        cont (Cps.CONST c)
    | transl (VAR (x, _), _) cont =
        cont (Cps.VAR x)
    | transl (IF (m, n1, n2), _) cont =
        let val x = Id.gensym "if_cond" in
          transl m (fn m' =>
            Cps.LET ((x, m'),
              Cps.IF (x,
                transl n1 cont,
                transl n2 cont)))
        end
    | transl (ABS ((x, _), m), _) cont =
        let
          val k = Id.gensym "k"
          val y = Id.gensym "x"
        in
          cont (Cps.ABS ((x, k),
            transl m (fn m' =>
              Cps.LET ((y, m'), Cps.APP_TAIL (k, y)))))
        end
    | transl (APP (m, n), _) cont =
        let
          val f = Id.gensym "fn"
          val arg = Id.gensym "arg"
          val k = Id.gensym "k"
          val x = Id.gensym "x"
        in
          Cps.LET ((k, Cps.ABS_CONT (x, cont (Cps.VAR x))),
          transl n (fn n' =>
            Cps.LET ((arg, n'),
            transl m (fn m' =>
              Cps.LET ((f, m'),
              Cps.APP ((f, arg), k))))))
        end
    | transl (LET (d, m), _) cont =
        foldr (fn
            (VAL ((x, _), n), m') =>
              transl n (fn n' =>
                Cps.LET ((x, n'), m'))
          | (VALREC ((x, _), n), m') =>
              transl n (fn n' =>
                Cps.LET_REC ((x, n'), m')))
          (transl m cont) d
    | transl (TUPLE ms, _) cont =
        translPrim Prim.TUPLE ms cont
    | transl (CASE (m, xs, n), _) cont =
        translCase m xs n cont
    | transl (PRIM (p, ms), _) cont =
        translPrim p ms cont

  and translCase e1 ids e2 cont =
      let
        val newId = Id.gensym "t"
        fun loop exp cont n [] = transl exp cont
          | loop exp cont n (id :: ids) =
              Cps.LET
                ((id, Cps.PRIM (Prim.TUPLE_GET n, [newId])),
                 loop exp cont (n + 1) ids)
          val exp = loop e2 cont 1 (map #1 ids)
      in
        transl e1 (fn e1' =>
          Cps.LET ((newId, e1'), exp))
      end

  and translPrim p ms cont =
    translExpSeq ms [] (fn ids =>
      cont (Cps.PRIM (p, ids)))

  and translExpSeq [] ids body = body (rev ids)
    | translExpSeq (e :: exps) ids body =
        let val id = Id.gensym "e" in
          translExpSeq exps (id :: ids) (fn ids =>
            transl e (fn e' =>
              Cps.LET ((id, e'), body ids)))
        end
end
