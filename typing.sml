structure Typing : TYPING = struct
  local
    open Type
  in
    exception Unify of t * t

  fun occur r1 (FUN (t21s, t22)) =
        List.exists (occur r1) t21s orelse occur r1 t22
    | occur r1 (VAR (r2 as (ref NONE))) = r1 = r2
    | occur r1 (VAR (r2 as (ref (SOME t2)))) =
        r1 = r2 orelse occur r1 t2
    | occur _ _ = false

    fun unify (INT, INT) = ()
      | unify (BOOL, BOOL) = ()
      | unify (t1 as (VAR (r1 as (ref NONE))), t2) =
          if occur r1 t2 then raise (Unify (t1, t2))
          else r1 := SOME t2
      | unify (t1, t2 as (VAR (r2 as (ref NONE)))) =
          if occur r2 t1 then raise (Unify (t1, t2))
          else r2 := SOME t1
      | unify (VAR (ref (SOME t1)), VAR (ref (SOME t2))) = unify (t1, t2)
      | unify (FUN (t11s, t12), FUN (t21s, t22)) =
          (ListPair.appEq unify (t11s, t21s);
           unify (t12, t22))
      | unify (t1, t2) = raise (Unify (t1, t2))

    fun derefType (VAR (t as (ref NONE))) = t := SOME INT
      | derefType (FUN (t1s, t2)) =
          (List.app derefType t1s;
           derefType t2)
      | derefType _ = ()
  end

  exception UnboundVar of Id.t

  local
    open TypedSyntax
  in
    fun g env (Syntax.CONST c) =
          E (CONST c, Const.typeOf c)
      | g env (Syntax.VAR x) =
          (case Env.find (env, x) of
             NONE => raise (UnboundVar x)
           | SOME t => E (VAR x, t))
      | g env (Syntax.IF (m, n1, n2)) =
          let
            val (m' as (E (_, t1))) = g env m;
            val (n1' as (E (_, t2))) = g env n1;
            val (n2' as (E (_, t3))) = g env n2
          in
            unify (t1, Type.BOOL);
            unify (t2, t3);
            E (IF (m', n1', n2'), t2)
          end
      | g env (Syntax.ABS (xs, m)) =
          let
            val xs' = List.map (fn x => (x, Type.genvar ())) xs;
            val (m' as (E (_, t))) = g (Env.insertList (env, xs')) m
          in
            E (ABS (xs', m'), Type.FUN (map #2 xs', t))
          end
      | g env (Syntax.APP (m, ns)) =
          let 
            val (m' as (E (_, t1))) = g env m;
            val ns' = map (g env) ns;
            val t12 = Type.genvar ()
          in
            unify (t1, Type.FUN (map expTypeOf ns', t12));
            E (APP (m', ns'), t12)
          end
      | g env (Syntax.LET_VAL (x, m, n)) =
          let
            val (m' as (E (_, t1))) = g env m;
            val (n' as (E (_, t2))) = g (Env.insert (env, x, t1)) n
          in
            E (LET_VAL ((x, t1), m', n'), t2)
          end
      | g env (Syntax.LET_VALREC (f, xs, m, n)) =
          let
            val t1 = Type.genvar ();
            val xs' = List.map (fn x => (x, Type.genvar ())) xs;
            val (m' as (E (_, t1'))) =
              g (Env.insertList (env, (f, t1) :: xs')) m
            val (n' as (E (_, t2))) = g (Env.insert (env, f, t1')) n
          in
            unify (t1, t1');
            E (LET_VALREC ((f, t1'), xs', m', n'), t2)
          end
      | g env (Syntax.PRIM (p, ms)) =
          let
            val t = Type.genvar ();
            val ms' = map (g env) ms
          in
            unify (Prim.typeOf p, Type.FUN (map expTypeOf ms', t));
            E (PRIM (p, ms'), t)
          end

    fun derefExp (E (m, t)) = (derefType t; derefExpBody m)
    and derefExpBody (IF (m, n1, n2)) =
          (derefExp m;
           derefExp n1;
           derefExp n2)
      | derefExpBody (ABS (xs, m)) =
          (List.app (derefType o #2) xs;
           derefExp m)
      | derefExpBody (APP (m, ns)) =
          (derefExp m;
           List.app derefExp ns)
      | derefExpBody (LET_VAL ((_, t), m, n)) =
          (derefType t;
           derefExp m;
           derefExp n)
      | derefExpBody (LET_VALREC ((_, t1), xs, m, n)) =
          (derefType t1;
           List.app (derefType o #2) xs;
           derefExp m;
           derefExp n)
      | derefExpBody (PRIM (_, ms)) =
          List.app derefExp ms
      | derefExpBody _ = ()

    fun f env m =
      let val m' = g env m in
        derefExp m';
        m'
      end
  end
end
