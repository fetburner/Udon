structure Typing : TYPING = struct
  local
    open Type
  in
    (* exception that arises when type checker fail to unify types *)
    exception Unify of t * t

    (* occur check *)
    fun occur r1 (FUN (t21s, t22)) =
          List.exists (occur r1) t21s orelse occur r1 t22
      | occur r1 (VAR (r2 as (ref NONE))) = r1 = r2
      | occur r1 (VAR (r2 as (ref (SOME t2)))) =
          r1 = r2 orelse occur r1 t2
      | occur _ _ = false

    (* unifier *)
    fun unify (INT, INT) = ()
      | unify (BOOL, BOOL) = ()
      | unify (FUN (t11s, t12), FUN (t21s, t22)) =
          (ListPair.appEq unify (t11s, t21s);
           unify (t12, t22))
      | unify (VAR (ref (SOME t1)), t2) = unify (t1, t2)
      | unify (t1, VAR (ref (SOME t2))) = unify (t1, t2)
      | unify (VAR (r1 as (ref NONE)), t2 as (VAR (r2 as (ref NONE)))) =
          if r1 = r2 then ()
          else r1 := SOME t2
      | unify (t1 as (VAR (r1 as (ref NONE))), t2) =
          if occur r1 t2 then raise (Unify (t1, t2))
          else r1 := SOME t2
      | unify (t1, t2 as (VAR (r2 as (ref NONE)))) =
          if occur r2 t1 then raise (Unify (t1, t2))
          else r2 := SOME t1
      | unify (t1, t2) = raise (Unify (t1, t2))

    (* replace type variable with appropriate type (int) in type *)
    fun derefType (VAR (t as (ref NONE))) = t := SOME INT
      | derefType (VAR (ref (SOME t))) = derefType t
      | derefType (FUN (t1s, t2)) =
          (List.app derefType t1s;
           derefType t2)
      | derefType _ = ()
  end

  (* exception that arises when unbound variable occur *)
  exception UnboundVar of Id.t
  local
    open TypedSyntax
  in
    (* perform type inference *)
    fun g env (Syntax.CONST c) =
          E (CONST c, Const.typeOf c)
      | g env (Syntax.VAR x) =
          (case Env.find (env, x) of
             NONE => raise (UnboundVar x)
           | SOME t => E (VAR x, t))
      | g env (Syntax.IF (m, n1, n2)) =
          let
            val m' = g env m
            val n1' = g env n1
            val n2' = g env n2
          in
            unify (expTypeOf m', Type.BOOL);
            unify (expTypeOf n1', expTypeOf n2');
            E (IF (m', n1', n2'), expTypeOf n1')
          end
      | g env (Syntax.ABS (xs, m)) =
          let
            val xs' = List.map (fn x => (x, Type.genvar ())) xs
            val m' = g (Env.insertList (env, xs')) m
          in
            E (ABS (xs', m'), Type.FUN (idSeqTypeOf xs', expTypeOf m'))
          end
      | g env (Syntax.APP (m, ns)) =
          let
            val m' = g env m
            val ns' = map (g env) ns
            val t12 = Type.genvar ()
          in
            unify (expTypeOf m', Type.FUN (expSeqTypeOf ns', t12));
            E (APP (m', ns'), t12)
          end
      | g env (Syntax.LET (dec, m)) =
          typingLet [] env dec m
      | g env (Syntax.PRIM (p, ms)) =
          let
            val t = Type.genvar ()
            val ms' = map (g env) ms
          in
            unify (Prim.typeOf p, Type.FUN (expSeqTypeOf ms', t));
            E (PRIM (p, ms'), t)
          end
    and typingLet dec' env [] body =
          let 
            val body' = g env body
          in
            E (LET (rev dec', body'), expTypeOf body')
          end
      | typingLet dec' env (Syntax.VAL (x, m) :: dec) body =
          let
            val m' = g env m
            val env' = Env.insert (env, x, expTypeOf m')
          in
            typingLet (VAL ((x, expTypeOf m'), m') :: dec') env' dec body
          end
      | typingLet dec' env (Syntax.VALREC (f, xs, m) :: dec) body =
          let
            val t1 = Type.genvar ()
            val xs' = List.map (fn x => (x, Type.genvar ())) xs
            val m' = g (Env.insertList (env, (f, t1) :: xs')) m
            val env' = Env.insert (env, f, t1)
          in
            unify (t1, Type.FUN (idSeqTypeOf xs', expTypeOf m'));
            typingLet (VALREC ((f, t1), xs', m') :: dec') env' dec body
          end

    (* replace type variable with appropriate type in typed expression *)
    fun derefExp (E (m, t)) = (derefType t; derefExpBody m)
    (* replace type variable with appropriate type in body of typed expression *)
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
      | derefExpBody (LET (dec, m)) =
          (List.app derefDec dec;
           derefExp m)
      | derefExpBody (PRIM (_, ms)) =
          List.app derefExp ms
      | derefExpBody _ = ()
    (* replace type variable with appropriate type in body of typed declaration *)
    and derefDec (VAL ((_, t), m)) =
          (derefType t;
           derefExp m)
      | derefDec (VALREC ((_, t1), xs, m)) =
          (derefType t1;
           List.app (derefType o #2) xs;
           derefExp m)

    (* typing expression and remove type variable *)
    fun f env m =
      let val m' = g env m in
        derefExp m';
        m'
      end
  end
end
