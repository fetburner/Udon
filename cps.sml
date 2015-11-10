structure Cps = struct
  (* t *)
  datatype term =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
    (* fn (x, k) => e *)
    | ABS of (Id.t * Id.t) * exp
    (* fn x => e *)
    | ABS_CONT of Id.t * exp
    (* ( + ) (x_1, ... , x_n) *)
    | PRIM of Prim.t * Id.t list
  (* e *)
  and exp =
    (* x y k *)
      APP of (Id.t * Id.t) * Id.t
    (* x y *)
    | APP_TAIL of Id.t * Id.t
    (* let val rec f = t in e end *)
    | LET_REC of (Id.t * term) * exp
    (* if x then e1 else e2 *)
    | IF of Id.t * exp * exp

  fun termToString (CONST c) = Const.toString c
    | termToString (VAR x) = Id.toString x
    | termToString (ABS ((x, k), e)) =
        "fn ("
        ^ Id.toString x
        ^ ", "
        ^ Id.toString k
        ^ ") => "
        ^ expToString e
    | termToString (ABS_CONT (x, e)) =
        "fn "
        ^ Id.toString x
        ^ " => "
        ^ expToString e
    | termToString (PRIM (p, xs)) =
        Prim.toString p
        ^ " "
        ^ Id.seqToString xs
  and expToString (APP ((x, y), k)) =
        Id.toString x
        ^ " "
        ^ Id.toString y
        ^ " "
        ^ Id.toString k
    | expToString (APP_TAIL (x, y)) =
        Id.toString x
        ^ " "
        ^ Id.toString y
    | expToString (LET_REC ((x, t), e)) =
        "let val rec "
        ^ Id.toString x
        ^ " = "
        ^ termToString t
        ^ " in "
        ^ expToString e
        ^ " end"
    | expToString (IF (x, e1, e2)) =
        "if "
        ^ Id.toString x
        ^ " then "
        ^ expToString e1
        ^ " else "
        ^ expToString e2


  fun freeVarOfTerm (CONST _) = IdSet.empty
    | freeVarOfTerm (VAR x) = IdSet.singleton x
    | freeVarOfTerm (ABS ((x, k), e)) = IdSet.subtractList (freeVarOfExp e, [x, k])
    | freeVarOfTerm (ABS_CONT (x, e)) = IdSet.subtract (freeVarOfExp e, x)
    | freeVarOfTerm (PRIM (p, xs)) = IdSet.fromList xs

  and freeVarOfExp (APP ((x, y), k)) = IdSet.fromList [x, y, k]
    | freeVarOfExp (APP_TAIL (x, y)) = IdSet.fromList [x, y]
    | freeVarOfExp (LET_REC ((x, t), e)) =
        IdSet.subtract (IdSet.union (freeVarOfTerm t, freeVarOfExp e), x)
    | freeVarOfExp (IF (x, e1, e2)) =
        IdSet.add (IdSet.union (freeVarOfExp e1, freeVarOfExp e2), x)


  fun sizeOfTerm (CONST _) = 1
    | sizeOfTerm (VAR _) = 1
    | sizeOfTerm (ABS ((_, _), e)) = 1 + sizeOfExp e
    | sizeOfTerm (ABS_CONT (_, e)) = 1 + sizeOfExp e
    | sizeOfTerm (PRIM _) = 1

  and sizeOfExp (APP _) = 1
    | sizeOfExp (APP_TAIL _) = 1
    | sizeOfExp (LET_REC ((_, t), e)) = 1 + sizeOfTerm t + sizeOfExp e
    | sizeOfExp (IF (_, e1, e2)) = 1 + sizeOfExp e1 + sizeOfExp e2
end
