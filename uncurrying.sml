structure Uncurrying = struct
  local
    open TypedSyntax
  in
    fun uncurryingExp env (m as (CONST _, _)) = m
      | uncurryingExp env (VAR x, t) =
          (* need renaming *)
          (VAR (getOpt (Env.find (env, x), x)), t)
      | uncurryingExp env (IF (m, n1, n2), t) =
          (IF (uncurryingExp env m, uncurryingExp env n1, uncurryingExp env n2), t)
      | uncurryingExp env (m as (ABS _, _)) =
          uncurryingAbs [] env m
      | uncurryingExp env (APP (m, ns), t) =
          (APP (uncurryingExp env m, map (uncurryingExp env) ns), t)
      | uncurryingExp env (LET (dec, m), t) =
          (LET (map (uncurryingDec env) dec, uncurryingExp env m), t)
      | uncurryingExp env (TUPLE ms, t) =
          (TUPLE (map (uncurryingExp env) ms), t)
      | uncurryingExp env (CASE (m, xs, n), t) =
          (CASE (uncurryingExp env m, xs, uncurryingExp env n), t)
      | uncurryingExp env (PRIM (p, ms), t) =
          (PRIM (p, map (uncurryingExp env) ms), t)
    and uncurryingDec env (VAL (x, m)) =
          VAL (x, uncurryingExp env m)
      | uncurryingDec env (VALREC (x, m)) =
          VALREC (x, uncurryingExp env m)

    (* convert fn x_1 => ... fn x_n => m *)
    (* into fn x_1 => ... fn x_n => (fn (y_1, ... , y_n) => m) (x_1, ... , x_n) *)
    and uncurryingAbs xs env (ABS (ys, m), t) =
          (ABS (ys, uncurryingAbs (rev ys @ xs) env m), t)
      | uncurryingAbs xs env (m as (_, t)) =
          let
            val xs' = rev xs
            val xs'' = map (fn (x, t) => (VAR x, t)) xs'
            val (bindings, ys) = ListPair.unzip (map (fn (x, t) =>
              let val y = Id.gensym "y" in
                ((x, y), (y, t))
              end) xs')
            val m' = uncurryingExp (Env.insertList (env, bindings)) m
          in
            (APP ((ABS (ys, m'), Type.FUN (idSeqTypeOf xs', t)), xs''), t)
          end

    val uncurrying = uncurryingExp Env.empty
  end
end
