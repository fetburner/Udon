structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

(*
  fun substitute cont newId =
    let open Cps
        fun subValue old new (v as VAR id) =
          if id = old then VAR new else v
          | subValue _ _ (c as CONST _) = c
        and subCont old new (CVAR id) =
            CVAR (if id = old then new else id)
          | subCont old new (CABS (id, e)) =
            CABS (if id = old then new else id, subExp old new e)
        and subExp old new exp =
          case exp of
             APP (v1, v2, c) =>
             APP (subValue old new v1, subValue old new v2, subCont old new c)
           | APP_TAIL (c, v) =>
             APP_TAIL (subCont old new c, subValue old new v)
           | LET ((id, abs), exp) =>
             LET ((id, subAbs old new abs), subExp old new exp)
           | LET_REC ((id, abs), exp) =>
             LET_REC ((id, subAbs old new abs), subExp old new exp)
           | IF (v, e1, e2) =>
             IF (subValue old new v, subExp old new e1, subExp old new e2)
        and subAbs old new (ABS (id1, id2, e)) =
            ABS (id1, id2, subExp old new e)
          | subAbs old new (ABS_TAIL (id, e)) =
            ABS_TAIL (id, subExp old new e)
          | subAbs old new (TUPLE vs)  =
            TUPLE (List.map (subValue old new) vs)
    in 
      case cont of
          Cps.CVAR _ => raise (Fail "metaContApp")
        | Cps.CABS (id, exp) => subExp id newId exp
    end
*)
                      
  fun transl (E (exp, _)) cont = 
    case exp of
        CONST c => Cps.APP_TAIL (cont, (Cps.CONST c))
      | VAR id => Cps.APP_TAIL (cont, (Cps.VAR id))
      | TUPLE exps => translTuple exps cont
      | IF (e1, e2, e3) => translIf e1 e2 e3 cont
      | ABS (id, exp) => translAbs id exp cont
      | APP (e1, e2) => translApp e1 e2 cont
      | LET (decs, exp) => translLet decs exp cont
      | CASE (e1, ids, e2) => translCase e1 ids e2 cont
  and translCase e1 ids e2 cont =
      let fun loop v exp cont n [] = transl exp cont
            | loop v exp cont n (id::ids) =
              Cps.LET ((id, Cps.GET (v, n)), loop v exp cont (n + 1) ids)
          val newId = Id.gensym "t"
          val exp = loop (Cps.VAR newId) e2 cont 1 (List.map #1 ids)
      in
        transl e1 (Cps.CABS (newId, exp))
      end
  and translLet [] exp cont = transl exp cont
    | translLet (dec::decs) exp cont =
      case dec of
        VAL (f, exp) =>
        transl exp (Cps.CABS (#1 f, translLet decs exp cont))
      | VALREC (f, arg, exp) => (* XXX : introduce dummy type here *)
        transl (E (ABS (arg, exp), Type.INT)) (Cps.CABS (#1 f, translLet decs exp cont))
  and translApp e1 e2 cont =
      let val (arg1, arg2) = (Id.gensym "f", Id.gensym "arg") in
        transl e1 (Cps.CABS (arg1, transl e2 (Cps.CABS (arg2, Cps.APP (Cps.VAR arg1, Cps.VAR arg2, cont)))))
      end
  and translIf e1 e2 e3 cont =
      let val newId = Id.gensym "cond" in
        transl e1 (Cps.CABS (newId, (Cps.IF (Cps.VAR newId, transl e2 cont, transl e3 cont))))
      end
  and translAbs id exp cont =
      let val (fname, arg, k) = (Id.gensym "fun", Id.gensym "arg", Id.gensym "k")
          val z = Id.gensym "z"
      in
        Cps.LET_REC ((fname, Cps.ABS (k, arg, transl exp (Cps.CABS (z, Cps.APP_TAIL (Cps.CVAR k, Cps.VAR z))))),
                     Cps.APP_TAIL (cont, Cps.VAR fname)
                    )
      end
  and translTuple (exps : exp list) (cont : Cps.cont) =
      let
        fun translTuple' acc (e :: exps) (id :: ids) =
          transl e (Cps.CABS (id, translTuple' acc exps ids))
          | translTuple' acc [] [] = acc
          | translTuple' _ _ _ = raise (Fail "translTuple")
        val ids = List.foldl (fn (_, acc) => Id.gensym "e" :: acc) [] exps
        val values = List.map Cps.VAR ids
        val tuple = Id.gensym "t"
        val acc = Cps.LET ((tuple, Cps.TUPLE values), Cps.APP_TAIL (cont, Cps.VAR tuple))
      in
        translTuple' acc exps ids
      end
        
  (* fun translIds ids = *)
  (*   List.foldl (fn (exp, cps) => ) *)
  (* fun translDec dec cont = *)
  (*   case dec of *)
  (*       VAL (id, exp) =>  *)
  (*    |  CONST _ => APP_TAIL cont value *)
                            
end          
    

