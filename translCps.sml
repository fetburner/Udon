structure TranslCps = struct
  open State
  open TypedSyntax
  exception Fail of string

  infix >>= >>

  (* main translation function *)
  fun insert x args m =
    modify (fn p => IdMap.insert (p, x, { args = args, body = m }))

  datatype cont =
      CVAR of Cps.term
    | CABS of Cps.term -> (Cps.prog, Cps.exp) state

  fun translExp env (E (CONST c, _)) (CVAR x) =
        return (Cps.APP { func = x, args = [Cps.CONST c] })
    | translExp env (E (CONST c, _)) (CABS k) =
        k (Cps.CONST c)
    | translExp env (E (VAR x, _)) (CVAR y) =
        return (Cps.APP { func = y, args = [Env.lookup (env, x)] })
    | translExp env (E (VAR x, _)) (CABS k) =
        k (Env.lookup (env, x))
    | translExp env (E (IF (m, n1, n2), _)) (c as CVAR _) =
        translIf env (m, n1, n2) c
    | translExp env (E (IF (m, n1, n2), _)) (CABS k) =
        let
          val x = Id.gensym "if_label"
          val y = Id.gensym "x"
        in
          k (Cps.ARG { func = x, arg = y })
          >>= insert x [y]
          >> translIf env (m, n1, n2) (CVAR (Cps.LABEL x))
        end
    | translExp env (E (ABS (xs, m), _)) (CVAR x) =
        translAbs (Id.gensym "fn") env (map #1 xs) m
          (fn m' => return (Cps.APP { func = x, args = [m'] }))
    | translExp env (E (ABS (xs, m), _)) (CABS k) =
        translAbs (Id.gensym "fn") env (map #1 xs) m k
    | translExp env (E (APP (m, ns), _)) (CVAR x) =
        translApp env m ns x
    | translExp env (E (APP (m, ns), _)) (CABS k) =
        let
          val x = Id.gensym "k"
          val y = Id.gensym "x"
        in
          k (Cps.ARG { func = x, arg = y })
          >>= insert x [y]
          >> translApp env m ns (Cps.LABEL x)
        end
    | translExp env (E (LET (d, m), _)) c =
        translLet env d m c
    | translExp env (E (TUPLE ms, _)) (CVAR x) =
        translPrim env Prim.TUPLE ms
          (fn m' => return (Cps.APP { func = x, args = [m'] }))
    | translExp env (E (TUPLE ms, _)) (CABS k) =
        translPrim env Prim.TUPLE ms k
    | translExp env (E (CASE (m, xs, n), _)) c =
        translCase env m (map #1 xs) n c
    | translExp env (E (PRIM (p, ms), _)) (CVAR x) =
        translPrim env p ms
          (fn m' => return (Cps.APP { func = x, args = [m'] }))
    | translExp env (E (PRIM (p, ms), _)) (CABS k) =
        translPrim env p ms k

  and translIf env (m, n1, n2) c =
    let
      val then_ = Id.gensym "then"
      val else_ = Id.gensym "else"
    in
      translExp env n1 c
      >>= insert then_ []
      >> translExp env n2 c
      >>= insert else_ []
      >> translExp env m (CABS (fn m' =>
           return
             (Cps.IF
               { cond = m',
                 then_ = Cps.LABEL then_,
                 else_ = Cps.LABEL else_ })))
    end

  and translAbs f env xs m k =
    let
      val y = Id.gensym "k"
      val env' = 
        Env.insertList
          (env, map (fn x => (x, Cps.ARG { func = f, arg = x })) (y :: xs))
    in
      translExp env' m (CVAR (Cps.ARG { func = f, arg = y }))
      >>= insert f (y :: xs)
      >> k (Cps.LABEL f)
    end

  and translApp env m ns k =
    translExp env m (CABS (fn m' =>
      translExpSeq env ns [] (fn ns' =>
        return (Cps.APP { func = m', args = k :: ns' }))))

  and translCase env m xs n c =
    translExp env m (CABS (fn m' =>
      let val env' = #1 (foldl (fn (x, (env, n)) =>
        (Env.insert (env, x,
          Cps.PRIM { prim = Prim.TUPLE_GET n, args = [m']}), n + 1)) (env, 1) xs) in
        translExp env' n c
      end))

  and translLet env [] m c = translExp env m c
    | translLet env (VAL (x, m) :: dec) n c =
        translExp env m (CABS (fn m' =>
          translLet (Env.insert (env, #1 x, m')) dec n c))
    | translLet env (VALREC (f, E (ABS (xs, m), _)) :: dec) n c =
        let val env' = Env.insert (env, #1 f, Cps.LABEL (#1 f)) in
          translAbs (#1 f) env' (map #1 xs) m (fn m' =>
            translLet env' dec n c)
        end
    | translLet env (VALREC _ :: dec) n c =
        raise (Fail "translLet: VALREC")

  and translPrim env p ms k =
    translExpSeq env ms [] (fn ts => k (Cps.PRIM { prim = p, args = ts}))

  and translExpSeq env [] ts body = body (rev ts)
    | translExpSeq env (m :: ms) ts body =
        translExp env m (CABS (fn m' =>
          translExpSeq env ms (m' :: ts) body))

  fun transl entry m =
    let
      val k = Id.gensym "k"
      val (m', prog) = runState
        (translExp Env.empty m (CVAR (Cps.ARG { func = entry, arg = k }))) IdMap.empty
    in
      IdMap.insert (prog, entry, { args = [k], body = m' })
    end
end
