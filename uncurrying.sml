structure Uncurrying = struct
  open TypedSyntax

  fun uncurryingAbs args args' (ABS (x, m)) =
        let val x' = (Id.gensym "x", idTypeOf x) in
          ABS (x', uncurryingAbs (x :: args) (x' :: args') m)
        end
    | uncurryingAbs args args' m =
        let val x = Id.gensym "x" in
          APP
            (ABS ((x, Type.TUPLE (idSeqTypeOf args)),
               CASE (VAR (x, []), args, uncurryingExp m)),
             TUPLE (map (fn (x, _) => VAR (x, [])) args'))
        end

  and uncurryingExp (m as CONST _) = m
    | uncurryingExp (m as VAR _) = m
    | uncurryingExp (IF (m, n1, n2)) =
        IF (uncurryingExp m, uncurryingExp n1, uncurryingExp n2)
    | uncurryingExp (m as ABS _) = uncurryingAbs [] [] m
    | uncurryingExp (APP (m, n)) =
        APP (uncurryingExp m, uncurryingExp n)
    | uncurryingExp (LET (dec, m)) =
        LET (map uncurryingDec dec, uncurryingExp m)
    | uncurryingExp (TUPLE ms) =
        TUPLE (map uncurryingExp ms)
    | uncurryingExp (CASE (m, xs, n)) =
        CASE (uncurryingExp m, xs, uncurryingExp n)
    | uncurryingExp (PRIM (p, ms)) =
        PRIM (p, map uncurryingExp ms)

  and uncurryingDec (VAL (x, m)) =
        VAL (x, uncurryingExp m)
    | uncurryingDec (VALREC (f, m)) =
        VALREC (f, uncurryingExp m)

  val uncurrying = uncurryingExp
end
