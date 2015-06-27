structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

  (* utility functions *)
  fun assoc [] a = NONE
    | assoc ((key, value)::xs) a =
      if a = key then SOME value else assoc xs a

  local
    open Cps
  in
  fun simpValue map (c as CONST _) = c
    | simpValue map (v as VAR id) =
      case assoc map id of NONE => v | SOME v => v
  and simpExp map (APP (v, vs, c)) = APP (simpValue map v, List.map (simpValue map) vs, simpCont map c)
    | simpExp map (APP_TAIL (CABS (id, exp), v)) = simpExp ((id, v) :: map) exp
    | simpExp map (APP_TAIL (CVAR id, v)) = APP_TAIL (CVAR id, simpValue map v)
    | simpExp map (LET ((id, abs), exp)) = LET ((id, simpAbs map abs), simpExp map exp)
    | simpExp map (LET_REC ((id, abs), exp)) = LET ((id, simpAbs map abs), simpExp map exp)
    | simpExp map (IF (v, e1, e2)) = IF (simpValue map v, simpExp map e1, simpExp map e2)
  and simpAbs map (ABS (k, ids, exp)) = ABS (k, ids, simpExp map exp)
    | simpAbs map (TUPLE vs) = TUPLE (List.map (simpValue map) vs)
    | simpAbs map (GET (v, i)) = GET (simpValue map v, i)
  and simpCont map (c as CVAR id) = c
    | simpCont map (CABS (id, exp as APP_TAIL (cont, VAR id'))) =
      if id = id' then cont else CABS (id, simpExp map exp)
    | simpCont map (CABS (id, exp)) = CABS (id, simpExp map exp)
  end

  (* main translation function *)
  fun transl (E (_, exp, _)) cont =
    case exp of
        CONST c => Cps.APP_TAIL (cont, (Cps.CONST c))
      | VAR id => Cps.APP_TAIL (cont, (Cps.VAR id))
      | TUPLE exps => translTuple exps cont
      | IF (e1, e2, e3) => translIf e1 e2 e3 cont
      | ABS (ids, body) => translAbs NONE false (List.map #1 ids) body cont
      | APP (func, args) => translApp func args cont
      | LET (decs, exp') => translLet decs exp' cont
      | CASE (e1, ids, e2) => translCase e1 ids e2 cont
  and translAbs nameopt recflag ids body cont =
      let
        val f = case nameopt of NONE => Id.gensym "fn" |  SOME id => id
        val k = Id.gensym "k"
        val pair = ((f, Cps.ABS (k, ids, transl body (Cps.CVAR k))),
                    Cps.APP_TAIL (cont, Cps.VAR f))
      in
        if recflag then Cps.LET_REC pair else Cps.LET pair
      end

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
        VAL (f, E (_, ABS (ids, body), _)) =>
        let val arg' = Id.gensym "arg"
            val ids = List.map #1 ids
        in
          translAbs (SOME (#1 f)) false ids body
                    (Cps.CABS (arg', translLet decs exp cont))
        end
       | VAL (f, exp') => transl exp' (Cps.CABS (#1 f, translLet decs exp cont))
       | VALREC (f, E (_, ABS (ids, body), _)) =>
         let val arg' = Id.gensym "arg"
             val ids = List.map #1 ids
         in
           translAbs (SOME (#1 f)) true ids body
                     (Cps.CABS (arg', translLet decs exp cont))
         end
       | VALREC _ => raise (Fail "translLet: VALREC")
  and translApp (funcExp: exp) (argsExp: exp list) (cont: Cps.cont) =
      let
        val funcId = (Id.gensym "fn")
        val argsId = List.map (fn _ => (Id.gensym "arg")) argsExp
        val lastExp = Cps.APP (Cps.VAR funcId, List.map Cps.VAR argsId, cont)
        fun translSeq (e::es) (id::ids) last  =
          transl e (Cps.CABS (id, translSeq es ids last))
          | translSeq [] [] last = last
          | translSeq _ _ _ = raise (Fail "translApp")
      in
        translSeq (funcExp :: argsExp) (funcId :: argsId) lastExp
      end
      (* let val (arg1, arg2) = (Id.gensym "f", Id.gensym "arg") in *)
      (*   transl func (Cps.CABS (arg1, transl e2 (Cps.CABS (arg2, Cps.APP (Cps.VAR arg1, Cps.VAR arg2, cont))))) *)
      (* end *)
  and translIf e1 e2 e3 cont =
      let val newId = Id.gensym "cond" in
        transl e1 (Cps.CABS (newId, (Cps.IF (Cps.VAR newId, transl e2 cont, transl e3 cont))))
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

end
