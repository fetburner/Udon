structure Uncurrying = struct
  local
    open TypedSyntax
  in
    fun uncurryingExp env (m as (E (CONST _, _))) = m
      | uncurryingExp env (E (VAR x, t)) =
          (* need renaming *)
          E (VAR (getOpt (Env.find (env, x), x)), t)
      | uncurryingExp env (E (IF (m, n1, n2), t)) =
          E (IF (uncurryingExp env m, uncurryingExp env n1, uncurryingExp env n2), t)
      | uncurryingExp env (m as (E (ABS _, _))) =
          uncurryingAbs [] env m
      | uncurryingExp env (E (APP (m, ns), t)) =
          E (APP (uncurryingExp env m, map (uncurryingExp env) ns), t)
      | uncurryingExp env (E (LET (dec, m), t)) =
          E (LET (map (uncurryingDec env) dec, uncurryingExp env m), t)
      | uncurryingExp env (E (TUPLE ms, t)) =
          E (TUPLE (map (uncurryingExp env) ms), t)
      | uncurryingExp env (E (CASE (m, xs, n), t)) =
          E (CASE (uncurryingExp env m, xs, uncurryingExp env n), t)
      | uncurryingExp env (E (PRIM (p, ms), t)) =
          E (PRIM (p, map (uncurryingExp env) ms), t)
    and uncurryingDec env (VAL (x, m)) =
          VAL (x, uncurryingExp env m)
      | uncurryingDec env (VALREC (x, m)) =
          VALREC (x, uncurryingExp env m)

    (* convert fn x_1 => ... fn x_n => m *)
    (* into fn x_1 => ... fn x_n => (fn (y_1, ... , y_n) => m) (x_1, ... , x_n) *)
    and uncurryingAbs xs env (E (ABS (ys, m), t)) =
          E (ABS (ys, uncurryingAbs (rev ys @ xs) env m), t)
      | uncurryingAbs xs env (m as (E (_, t))) =
          let
            val xs' = rev xs
            val xs'' = map (fn (x, t) => E (VAR x, t)) xs'
            val (bindings, ys) = ListPair.unzip (map (fn (x, t) =>
              let val y = Id.gensym "y" in
                ((x, y), (y, t))
              end) xs')
            val m' = uncurryingExp (Env.insertList (env, bindings)) m
          in
            E (APP (E (ABS (ys, m'), Type.FUN (idSeqTypeOf xs', t)), xs''), t)
          end

    val uncurrying = uncurryingExp Env.empty
  end
end
