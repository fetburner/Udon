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
      | occur r1 (TUPLE ts) = List.exists (occur r1) ts
      | occur r1 INT = false
      | occur r1 BOOL = false

    (* unifier *)
    fun unify (INT, INT) = ()
      | unify (BOOL, BOOL) = ()
      | unify (FUN (t11s, t12), FUN (t21s, t22)) =
          (ListPair.appEq unify (t11s, t21s);
           unify (t12, t22))
      | unify (TUPLE t1s, TUPLE t2s) =
           ListPair.appEq unify (t1s, t2s)
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
    fun typingExp env (Syntax.CONST c) =
          E (CONST c, Const.typeOf c)
      | typingExp env (Syntax.VAR x) =
          (case Env.find (env, x) of
             NONE => raise (UnboundVar x)
           | SOME t => E (VAR x, t))
      | typingExp env (Syntax.IF (m, n1, n2)) =
          let
            val m' = typingExp env m
            val n1' = typingExp env n1
            val n2' = typingExp env n2
          in
            unify (expTypeOf m', Type.BOOL);
            unify (expTypeOf n1', expTypeOf n2');
            E (IF (m', n1', n2'), expTypeOf n1')
          end
      | typingExp env (Syntax.ABS (x, m)) =
          let
            val x' = (x, Type.genvar ())
            val m' = typingExp (Env.insertList (env, [x'])) m
          in
            E (ABS (x', m'), Type.FUN ([idTypeOf x'], expTypeOf m'))
          end
      | typingExp env (Syntax.APP (m, n)) =
          let
            val m' = typingExp env m
            val n' = typingExp env n
            val t12 = Type.genvar ()
          in
            unify (expTypeOf m', Type.FUN ([expTypeOf n'], t12));
            E (APP (m', n'), t12)
          end
      | typingExp env (Syntax.LET (dec, m)) =
          typingLet [] env dec m
      | typingExp env (Syntax.TUPLE ms) =
          let
            val ms' = map (typingExp env) ms
          in
            E (TUPLE ms', Type.TUPLE (map expTypeOf ms'))
          end
      | typingExp env (Syntax.CASE (m, xs, n)) =
          let
            val m' = typingExp env m
            val xs' = map (fn x => (x, Type.genvar ())) xs
            val n' = typingExp (Env.insertList (env, xs')) n
          in
            unify (expTypeOf m', Type.TUPLE (idSeqTypeOf xs'));
            E (CASE (m', xs', n'), expTypeOf n')
          end
    and typingLet dec' env [] body =
          let 
            val body' = typingExp env body
          in
            E (LET (rev dec', body'), expTypeOf body')
          end
      | typingLet dec' env (Syntax.VAL (x, m) :: dec) body =
          let
            val m' = typingExp env m
            val env' = Env.insert (env, x, expTypeOf m')
          in
            typingLet (VAL ((x, expTypeOf m'), m') :: dec') env' dec body
          end
      | typingLet dec' env (Syntax.VALREC (f, x, m) :: dec) body =
          let
            val t1 = Type.genvar ()
            val f' = (f, Type.genvar ())
            val x' = (x, Type.genvar ())
            val m' = typingExp (Env.insertList (env, [f', x'])) m
            val env' = Env.insertList (env, [f'])
          in
            unify (t1, Type.FUN ([idTypeOf x'], expTypeOf m'));
            typingLet (VALREC (f', x', m') :: dec') env' dec body
          end

    (* replace type variable with appropriate type in typed expression *)
    fun derefExp (E (m, t)) = (derefType t; derefExpBody m)
    (* replace type variable with appropriate type in body of typed expression *)
    and derefExpBody (IF (m, n1, n2)) =
          (derefExp m;
           derefExp n1;
           derefExp n2)
      | derefExpBody (ABS (x, m)) =
          (derefType (idTypeOf x);
           derefExp m)
      | derefExpBody (APP (m, n)) =
          (derefExp m;
           derefExp n)
      | derefExpBody (LET (dec, m)) =
          (List.app derefDec dec;
           derefExp m)
      | derefExpBody (TUPLE ms) =
          List.app derefExp ms
      | derefExpBody (CASE (m, xs, n)) =
          (derefExp m;
           List.app (derefType o idTypeOf) xs;
           derefExp n)
      | derefExpBody _ = ()
    (* replace type variable with appropriate type in body of typed declaration *)
    and derefDec (VAL ((_, t), m)) =
          (derefType t;
           derefExp m)
      | derefDec (VALREC ((_, t1), x, m)) =
          (derefType t1;
           derefType (idTypeOf x);
           derefExp m)

    (* typing expression and remove type variable *)
    fun typing env m =
      let val m' = typingExp env m in
        derefExp m';
        m'
      end
  end
end
