structure Cps = struct
  (* t *)
  datatype term =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
    (* fn (x_1, ..., x_n) => e *)
    | ABS of Id.t list * exp
    (* ( + ) (x_1, ... , x_n) *)
    | PRIM of Prim.t * Id.t list
    (* (x_1, ..., x_n) *)
    | TUPLE of Id.t list
    (* #n x *)
    | PROJ of int * Id.t
  (* e *)
  and exp =
    (* x (y_1, ..., y_n) *)
      APP of Id.t * Id.t list
    (* let val rec f = t in e end *)
    | LET_REC of binding list * exp
    (* if x then e1 else e2 *)
    | IF of Id.t * exp * exp
  withtype binding = Id.t * term

  fun termToString (CONST c) = Const.toString c
    | termToString (VAR x) = Id.toString x
    | termToString (ABS (xs, e)) =
        "fn "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString e
    | termToString (PRIM (p, xs)) =
        Prim.toString p
        ^ " "
        ^ Id.seqToString xs
    | termToString (TUPLE xs) =
        Id.seqToString xs
    | termToString (PROJ (n, x)) =
        "#"
        ^ Int.toString n
        ^ " "
        ^ Id.toString x

  and expToString (APP (x, ys)) =
        Id.toString x
        ^ " "
        ^ Id.seqToString ys
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
    | termFreeVar (ABS (xs, e)) = IdSet.subtractList (expFreeVar e, xs)
    | termFreeVar (PRIM (p, xs)) = IdSet.fromList xs
    | termFreeVar (TUPLE xs) = IdSet.fromList xs
    | termFreeVar (PROJ (n, x)) = IdSet.singleton x

  and expFreeVar (APP (x, ys)) = IdSet.fromList (x :: ys)
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
    | termSize (ABS (_, e)) = 1 + expSize e
    | termSize (PRIM _) = 1
    | termSize (TUPLE _) = 1
    | termSize (PROJ _) = 1

  and expSize (APP _) = 1
    | expSize (LET_REC (bindings, e)) =
        1 + foldl op+ 0 (map (termSize o #2) bindings) + expSize e
    | expSize (IF (_, e1, e2)) = 1 + expSize e1 + expSize e2
end
