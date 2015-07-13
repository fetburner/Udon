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

    fun uncurryingType (t as (VAR (ref (UNBOUND _)))) = t
      | uncurryingType (VAR (ref (LINK t))) = uncurryingType t
      | uncurryingType (t as (META _)) = t
      | uncurryingType (t as INT) = t
      | uncurryingType (t as BOOL) = t
      | uncurryingType (t as (FUN _)) =
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
    fun collectAbs args (E (ABS (xs, m), _)) =
          collectAbs (rev xs @ args) m
      | collectAbs args m = (rev args, m)
    val collectAbs = collectAbs []

    (* collect arguments of applications *)
    fun collectApps args (E (APP (m, ns), _)) =
          collectApps (rev ns @ args) m
      | collectApps args m = (m, args)
    val collectApps = collectApps []
    
    fun uncurryingExp env (E (m as (CONST _), t)) =
          E (m, t)
      | uncurryingExp env (E (m as (VAR x), t)) =
          (* don't care level because no generalization *)
          E (m, Type.inst 114514 (getOpt (Env.find (env, x), t)))
      | uncurryingExp env (E (IF (m, n1, n2), _)) =
          let
            val m' = uncurryingExp env m
            val n1' = uncurryingExp env n1
            val n2' = uncurryingExp env n2
          in
            E (IF (m', n1', n2'), expTypeOf n1') 
          end
      | uncurryingExp env (m as (E (ABS _, _))) =
          let
            val (xs, n) = collectAbs m
            val xs' = map uncurryingId xs
            val env' = Env.insertList (env, xs')
            val n' = uncurryingExp env' n
            val (t1s, t2) = collectArgs (expTypeOf n')
            val ys = map (fn t => (Id.gensym "dummy", t)) t1s
          in
            if null t1s then
              E (ABS (xs', n'), Type.FUN (idSeqTypeOf xs', expTypeOf n'))
            else
              (* eta expansion *)
              E
                (ABS (xs' @ ys,
                  E (APP (n', map (fn (y, t) =>
                    E (VAR y, t)) ys), t2)),
                Type.FUN (idSeqTypeOf xs' @ idSeqTypeOf ys, t2))
          end
      | uncurryingExp env (e as (E (APP _, _))) =
          let
            val (m, ns) = collectApps e
            val m' = uncurryingExp env m
            val ns' = map (uncurryingExp env) ns
            val (t1s, t2) = collectArgs (expTypeOf m')
            val t1s' = List.drop (t1s, length ns')
            val xs = map (fn t => (Id.gensym "dummy", t)) t1s'
          in
            if null xs then
              E (APP (m', ns'), t2)
            else
              (* eta expansion *)
              E
                (ABS (xs,
                  E
                    (APP (m', ns' @ map (fn (x, t) =>
                      E (VAR x, t)) xs), t2)), Type.FUN (t1s', t2))
          end
      | uncurryingExp env (E (LET (dec, m), _)) =
          uncurryingLet [] env env dec m
      | uncurryingExp env (E (TUPLE ms, _)) =
          let
            val ms' = map (uncurryingExp env) ms
          in
            E (TUPLE ms', Type.TUPLE (expSeqTypeOf ms'))
          end
      | uncurryingExp env (E (CASE (m, xs, n), _)) =
          let
            val m' = uncurryingExp env m
            val xs' = map uncurryingId xs
            val n' = uncurryingExp (Env.insertList (env, xs')) n
          in
            E (CASE (m', xs', n'), expTypeOf n')
          end
      | uncurryingExp env (E (PRIM (p, ms), t)) =
          E (PRIM (p, map (uncurryingExp env) ms), t)
    and uncurryingLet dec' env0 env [] body =
          let
            val body' = uncurryingExp env body
          in
            E (LET (rev dec', body'), expTypeOf body')
          end
      | uncurryingLet dec' env0 env (VAL (x, m) :: dec) body =
          let
            val x' = uncurryingId x
            val m' = uncurryingExp env m
            val env' = Env.insertList (env, [x'])
          in
            uncurryingLet (VAL (x', m') :: dec') env0 env' dec body
          end
      | uncurryingLet dec' env0 env (VALREC (f, m) :: dec) body =
          let
            val f' = uncurryingId f
            val env' = Env.insertList (env, [f'])
            val m' = uncurryingExp env' m
          in
            uncurryingLet (VALREC (f', m') :: dec') env0 env' dec body
          end

    val uncurrying = uncurryingExp
  end
end
