structure Cps = struct
  (* v *)
  datatype value =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
  (* t *)
  and term =
    (* v *)
      VAL of value
    (* fn ((x_1, ... , x_n), k) => e *)
    | ABS of (Id.t list * Id.t) * exp
    (* ( + ) (v_1, ... , v_n) *)
    | PRIM of Prim.t * value list
  (* e *)
  and exp =
    (* x ((v_1, ... , v_n), C) *)
      APP of Id.t * (value list * cont)
    (* k v *)
    | APP_TAIL of Id.t * value
    (* let val x = t in e end *)
    | LET of (Id.t * term) * exp
    (* let val rec f = t in e end *)
    | LET_REC of (Id.t * term) * exp
    (* let val rec k = C in e end *)
    | LET_CONT of (Id.t * cont) * exp
    (* if v then e1 else e2 *)
    | IF of value * exp * exp
  (* C *)
  and cont =
      (* k *)
      CVAR of Id.t
      (* fn x => e *)
    | CABS of Id.t * exp

  fun valueToString (CONST c) = Const.toString c
    | valueToString (VAR id) = Id.toString id

  fun vsToString seq = PP.seqToString (valueToString, "()", ", ", "(", ")") seq

  fun expToString (APP (k, (vs, cont))) =
        Id.toString k
        ^ " ("
        ^ vsToString vs
        ^ ", "
        ^ contToString cont
        ^ ")"
    | expToString (APP_TAIL (k, v)) =
        Id.toString k
        ^ " "
        ^ valueToString v
    | expToString (LET ((x, t), e)) =
        "let val "
        ^ Id.toString x
        ^ " = "
        ^ termToString t
        ^ " in "
        ^ expToString e
        ^ " end"
    | expToString (LET_REC ((x, t), e)) =
        "let val rec "
        ^ Id.toString x
        ^ " = "
        ^ termToString t
        ^ " in "
        ^ expToString e
        ^ " end"
    | expToString (LET_CONT ((k, c), e)) =
        "let val rec "
        ^ Id.toString k
        ^ " = "
        ^ contToString c
        ^ " in "
        ^ expToString e
        ^ " end"
    | expToString (IF (v, e1, e2)) =
        "if "
        ^ valueToString v
        ^ " then "
        ^ expToString e1
        ^ " else "
        ^ expToString e2
  and termToString (VAL v) = valueToString v
    | termToString (ABS ((xs, k), e)) =
        "fn ("
        ^ Id.seqToString xs
        ^ ", "
        ^ Id.toString k
        ^ ") => "
        ^ expToString e
    | termToString (PRIM (p, vs)) =
        Prim.toString p
        ^ " "
        ^ vsToString vs
  and contToString (CVAR k) = Id.toString k
    | contToString (CABS (x, e)) = "fn " ^ Id.toString x ^ " => " ^ expToString e

  local
    val op @ = IdSet.union
    val op - = IdSet.subtract
  in
    fun freeVarOfValue (CONST _) = IdSet.empty
      | freeVarOfValue (VAR x) = IdSet.singleton x

    fun freeVarOfValueSeq vs =
      foldl op @ IdSet.empty (map freeVarOfValue vs)

    fun freeVarOfTerm (VAL v) = freeVarOfValue v
      | freeVarOfTerm (ABS ((xs, k), e)) =
          IdSet.subtractList (freeVarOfExp e, k :: xs)
      | freeVarOfTerm (PRIM (_, vs)) = freeVarOfValueSeq vs

    and freeVarOfExp (APP (x, (vs, c))) =
          IdSet.add (freeVarOfValueSeq vs @ freeVarOfCont c, x)
      | freeVarOfExp (APP_TAIL (k, v)) = IdSet.add (freeVarOfValue v, k)
      | freeVarOfExp (LET ((x, t), e)) =
          IdSet.subtract (freeVarOfTerm t, x) @ freeVarOfExp e
      | freeVarOfExp (LET_REC ((x, t), e)) =
          IdSet.subtract (freeVarOfTerm t @ freeVarOfExp e, x)
      | freeVarOfExp (LET_CONT ((k, c), e)) =
          IdSet.subtract (freeVarOfCont c @ freeVarOfExp e, k)
      | freeVarOfExp (IF (v, e1, e2)) =
          freeVarOfValue v @ freeVarOfExp e1 @ freeVarOfExp e2

    and freeVarOfCont (CVAR k) = IdSet.singleton k
      | freeVarOfCont (CABS (x, e)) = IdSet.subtract (freeVarOfExp e, x)
  end
end
