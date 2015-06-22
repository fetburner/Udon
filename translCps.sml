structure TranslCps = struct
  open TypedSyntax
  exception Fail of string

  fun assoc [] a = NONE
    | assoc ((key, value)::xs) a =
      if a = key then SOME value else assoc xs a
                      
  local
    open Cps
  in
  fun simpValue map (c as CONST _) = c
    | simpValue map (v as VAR id) =
      case assoc map id of NONE => v | SOME v => v
  and simpExp map (APP (v1, v2, c)) = APP (simpValue map v1, simpValue map v2, simpCont map c)
    | simpExp map (APP_TAIL (CABS (id, exp), v)) = simpExp ((id, v) :: map) exp
    | simpExp map (APP_TAIL (CVAR id, v)) = APP_TAIL (CVAR id, simpValue map v)
    | simpExp map (LET ((id, abs), exp)) = LET ((id, simpAbs map abs), simpExp map exp)
    | simpExp map (LET_REC ((id, abs), exp)) = LET ((id, simpAbs map abs), simpExp map exp)
    | simpExp map (IF (v, e1, e2)) = IF (simpValue map v, simpExp map e1, simpExp map e2)
  and simpAbs map (ABS (id1, id2, exp)) = ABS (id1, id2, simpExp map exp)
    | simpAbs map (TUPLE vs) = TUPLE (List.map (simpValue map) vs)
    | simpAbs map (GET (v, i)) = GET (simpValue map v, i)
  and simpCont map (c as CVAR id) = c
    | simpCont map (CABS (id, exp)) = CABS (id, simpExp map exp)
  end
  
  fun transl (E (exp, _)) cont = 
    case exp of
        CONST c => Cps.APP_TAIL (cont, (Cps.CONST c))
      | VAR id => Cps.APP_TAIL (cont, (Cps.VAR id))
      | TUPLE exps => translTuple exps cont
      | IF (e1, e2, e3) => translIf e1 e2 e3 cont
      | ABS (id, exp) => translAbs NONE false id exp cont
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
        VAL (f, E (ABS (arg, exp'), _)) =>
        let val arg' = Id.gensym "arg" in
          translAbs (SOME (#1 f)) false arg exp' (Cps.CABS (arg', translLet decs exp cont))
        end
      | VAL (f, exp') => transl exp' (Cps.CABS (#1 f, translLet decs exp cont))
      | VALREC (f, arg, exp') =>
        let val arg' = Id.gensym "arg" in
          translAbs (SOME (#1 f)) true arg exp' (Cps.CABS (arg', translLet decs exp cont))
        end
  and translApp e1 e2 cont =
      let val (arg1, arg2) = (Id.gensym "f", Id.gensym "arg") in
        transl e1 (Cps.CABS (arg1, transl e2 (Cps.CABS (arg2, Cps.APP (Cps.VAR arg1, Cps.VAR arg2, cont)))))
      end
  and translIf e1 e2 e3 cont =
      let val newId = Id.gensym "cond" in
        transl e1 (Cps.CABS (newId, (Cps.IF (Cps.VAR newId, transl e2 cont, transl e3 cont))))
      end
  and translAbs nameopt recflag (id, _) exp cont =
      let val fname = case nameopt of NONE => Id.gensym "fn" |  SOME id => id
          val k = Id.gensym "k"
          val z = Id.gensym "z"
          val pair = ((fname, Cps.ABS (k, id, transl exp (Cps.CABS (z, Cps.APP_TAIL (Cps.CVAR k, Cps.VAR z))))),
                       Cps.APP_TAIL (cont, Cps.VAR fname))
      in
        if recflag
        then 
          Cps.LET_REC pair
        else
          Cps.LET pair
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
    

