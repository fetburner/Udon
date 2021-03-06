structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

  (* main translation function *)
  fun transl (CONST c) cont =
        cont (Cps.CONST c)
    | transl (VAR (x, _)) cont =
        cont (Cps.VAR x)
    | transl (IF (m, n1, n2)) cont =
        let val x = Id.gensym "if_cond" in
          transl m (fn m' =>
            Cps.LET_REC ([(x, m')],
              Cps.IF (x,
                transl n1 cont,
                transl n2 cont)))
        end
    | transl (ABS (xs, m)) cont =
        let
          val k = Id.gensym "k"
          val y = Id.gensym "x"
        in
          cont (Cps.ABS (map #1 xs @ [k],
            transl m (fn m' =>
              Cps.LET_REC ([(y, m')], Cps.APP (k, [y])))))
        end
    | transl (APP (m, ns)) cont =
        let
          val f = Id.gensym "fn"
          val k = Id.gensym "k"
          val x = Id.gensym "x"
        in
          Cps.LET_REC ([(k, Cps.ABS ([x], cont (Cps.VAR x)))],
          translExpSeq ns [] (fn ns' =>
            transl m (fn m' =>
              Cps.LET_REC ([(f, m')],
              Cps.APP (f, ns' @ [k])))))
        end
    | transl (LET (d, m)) cont =
        foldr (fn
            (VAL ((x, _), n), m') =>
              transl n (fn n' =>
                Cps.LET_REC ([(x, n')], m'))
          | (VALREC ((x, _), n), m') =>
              transl n (fn n' =>
                Cps.LET_REC ([(x, n')], m')))
          (transl m cont) d
    | transl (TUPLE ms) cont =
        translExpSeq ms [] (fn ids => cont (Cps.TUPLE ids))
    | transl (CASE (m, xs, n)) cont =
        translCase m xs n cont
    | transl (PRIM (p, ms)) cont =
        translExpSeq ms [] (fn ids => cont (Cps.PRIM (p, ids)))

  and translCase e1 ids e2 cont =
      let
        val newId = Id.gensym "t"
        fun loop exp cont n [] = transl exp cont
          | loop exp cont n (id :: ids) =
              Cps.LET_REC
                ([(id, Cps.PROJ (n, newId))],
                 loop exp cont (n + 1) ids)
          val exp = loop e2 cont 1 (map #1 ids)
      in
        transl e1 (fn e1' =>
          Cps.LET_REC ([(newId, e1')], exp))
      end

  and translExpSeq [] ids body = body (rev ids)
    | translExpSeq (e :: exps) ids body =
        let val id = Id.gensym "e" in
          translExpSeq exps (id :: ids) (fn ids =>
            transl e (fn e' =>
              Cps.LET_REC ([(id, e')], body ids)))
        end
end
