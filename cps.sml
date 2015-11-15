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
    | LET_REC of binding list * exp
    (* if x then e1 else e2 *)
    | IF of Id.t * exp * exp
  withtype binding = Id.t * term

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
    | expToString (LET_REC (binding, e)) =
        "let val rec "
        ^ PP.seqToString (bindingToString, "", " and ", "", "") binding
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

  and bindingToString (x, t) = Id.toString x ^ " = " ^ termToString t

  fun termFreeVar (CONST _) = IdSet.empty
    | termFreeVar (VAR x) = IdSet.singleton x
    | termFreeVar (ABS ((x, k), e)) = IdSet.subtractList (expFreeVar e, [x, k])
    | termFreeVar (ABS_CONT (x, e)) = IdSet.subtract (expFreeVar e, x)
    | termFreeVar (PRIM (p, xs)) = IdSet.fromList xs

  and expFreeVar (APP ((x, y), k)) = IdSet.fromList [x, y, k]
    | expFreeVar (APP_TAIL (x, y)) = IdSet.fromList [x, y]
    | expFreeVar (LET_REC (bindings, e)) =
        let val (xs, ts) = ListPair.unzip bindings in
          IdSet.subtractList (foldl IdSet.union (expFreeVar e)
            (map termFreeVar ts), xs)
        end
    | expFreeVar (IF (x, e1, e2)) =
        IdSet.add (IdSet.union (expFreeVar e1, expFreeVar e2), x)

  and bindingsFreeVar bindings =
        let val (xs, ts) = ListPair.unzip bindings in
          IdSet.subtractList (foldl IdSet.union IdSet.empty
            (map termFreeVar ts), xs)
        end

  fun termSize (CONST _) = 1
    | termSize (VAR _) = 1
    | termSize (ABS ((_, _), e)) = 1 + expSize e
    | termSize (ABS_CONT (_, e)) = 1 + expSize e
    | termSize (PRIM _) = 1

  and expSize (APP _) = 1
    | expSize (APP_TAIL _) = 1
    | expSize (LET_REC (bindings, e)) =
        1 + foldl op+ 0 (map (termSize o #2) bindings) + expSize e
    | expSize (IF (_, e1, e2)) = 1 + expSize e1 + expSize e2
end
