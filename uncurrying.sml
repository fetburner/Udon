
structure Uncurrying = struct
  local
    open Type
  in
    (* collect arguments of function type *)
    fun collectArgs args (FUN (t1s, t2)) =
          collectArgs (rev t1s @ args) t2
      | collectArgs args (VAR (ref (LINK t))) =
          collectArgs args t
      | collectArgs args t = (rev args, t)
    val collectArgs = collectArgs []

    fun uncurryingType (t as VAR (ref (UNBOUND _))) = t
      | uncurryingType (VAR (ref (LINK t))) = uncurryingType t
      | uncurryingType (t as META _) = t
      | uncurryingType (t as INT) = t
      | uncurryingType (t as BOOL) = t
      | uncurryingType (t as FUN _) =
          (case collectArgs t of
             (args, result) =>
               FUN (map uncurryingType args, uncurryingType result))
      | uncurryingType (TUPLE ts) = TUPLE (map uncurryingType ts)
  end

  fun uncurryingId (x, t) = (x, uncurryingType t)

  local
    open TypedSyntax
  in
    (* collect arguments of lambda abstractions *)
    fun collectAbs args (ABS (xs, m), _) =
          collectAbs (rev xs @ args) m
      | collectAbs args m = (rev args, m)
    val collectAbs = fn ABS (xs, m) => collectAbs xs m

    (* collect arguments of applications *)
    fun collectApps args (APP (m, ns), _) =
          collectApps (rev ns @ args) m
      | collectApps args m = (m, args)
    val collectApps = fn APP (m, ns) => collectApps (rev ns) m

    fun uncurryingExp (m, t) =
      (uncurryingExpBody m, uncurryingType t)

    and uncurryingExpBody (m as CONST _) = m
      | uncurryingExpBody (m as VAR _) = m
      | uncurryingExpBody (IF (m, n1, n2)) =
          IF (uncurryingExp m, uncurryingExp n1, uncurryingExp n2)
      | uncurryingExpBody (m as ABS _) =
          let
            val (xs, n) = collectAbs m
            val xs' = map uncurryingId xs
            val n' = uncurryingExp n
            val (t1s, t2) = collectArgs (expTypeOf n')
            val ys = map (fn t => (Id.gensym "dummy", t)) t1s
            val ys' = map (fn (y, t) => (VAR y, t)) ys
          in
            if null t1s then
              ABS (xs', n')
            else
              (* eta expansion *)
              ABS (xs' @ ys, (APP (n', ys'), t2))
          end
      | uncurryingExpBody (m as APP _) =
          let
            val (m, ns) = collectApps m
            val m' = uncurryingExp m
            val ns' = map uncurryingExp ns
            val (t1s, t2) = collectArgs (expTypeOf m')
            val t1s' = List.drop (t1s, length ns')
            val xs = map (fn t => (Id.gensym "dummy", t)) t1s'
            val xs' = map (fn (x, t) => (VAR x, t)) xs
          in
            if null xs then
              APP (m', ns')
            else
              (* eta expansion *)
              ABS (xs, (APP (m', ns' @ xs'), t2))
          end
      | uncurryingExpBody (LET (dec, m)) =
          LET (map uncurryingDec dec, uncurryingExp m)
      | uncurryingExpBody (TUPLE ms) =
          TUPLE (map uncurryingExp ms)
      | uncurryingExpBody (CASE (m, xs, n)) =
          CASE (uncurryingExp m, map uncurryingId xs, uncurryingExp n)
      | uncurryingExpBody (PRIM (p, ms)) =
          PRIM (p, map uncurryingExp ms)
    and uncurryingDec (VAL (x, m)) =
          VAL (uncurryingId x, uncurryingExp m)
      | uncurryingDec (VALREC (f, m)) =
          VALREC (uncurryingId f, uncurryingExp m)

    val uncurrying = uncurryingExp
  end
end

