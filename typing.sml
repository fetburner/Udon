structure Typing : TYPING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string
  local
    open TypedSyntax
  in
    (* perform type inference *)
    fun typingExp l env (Syntax.CONST c) =
          (CONST c, Const.typeOf c)
      | typingExp l env (Syntax.VAR x) =
          (case Env.findName (env, x) of
                NONE => raise (UnboundVar x)
              | SOME (x', t0) =>
                  let val (t, ts) = Type.inst l t0 in
                    (VAR (x', ts), t)
                  end)
      | typingExp l env (Syntax.IF (m, n1, n2)) =
          let
            val (m', t1) = typingExp l env m
            val (n1', t2) = typingExp l env n1
            val (n2', t3) = typingExp l env n2
          in
            Type.unify (t1, Type.BOOL);
            Type.unify (t2, t3);
            (IF (m', n1', n2'), t2)
          end
      | typingExp l env (Syntax.ABS (x, m)) =
          let
            val x' = (Id.gensym x, Type.genvar l)
            val (m', t2) = typingExp l (Env.insertList (env, [idToPolyId x'])) m
          in
            (ABS ([x'], m'), Type.FUN ([idTypeOf x'], t2))
          end
      | typingExp l env (Syntax.APP (m, n)) =
          let
            val (m', t1) = typingExp l env m
            val (n', t2) = typingExp l env n
            val t12 = Type.genvar l
          in
            Type.unify (t1, Type.FUN ([t2], t12));
            (APP (m', [n']), t12)
          end
      | typingExp l env (Syntax.LET (dec, m)) =
          typingLet l [] env dec m
      | typingExp l env (Syntax.TUPLE ms) =
          let
            val (ms', ts) = ListPair.unzip (map (typingExp l env) ms)
          in
            (TUPLE ms', Type.TUPLE ts)
          end
      | typingExp l env (Syntax.CASE (m, xs, n)) =
          let
            val (m', t1) = typingExp l env m
            val xs' = map (fn x => (Id.gensym x, Type.genvar l)) xs
            val (n', t2) = typingExp l (Env.insertList (env, map idToPolyId xs')) n
          in
            Type.unify (t1, Type.TUPLE (idSeqTypeOf xs'));
            (CASE (m', xs', n'), t2)
          end
      | typingExp l env (Syntax.PRIM (p, ms)) =
          let
            val t = Type.genvar l
            val (ms', ts) = ListPair.unzip (map (typingExp l env) ms)
          in
            Type.unify (Prim.typeOf p, Type.FUN (ts, t));
            (PRIM (p, ms'), t)
          end
    and typingLet l dec' env [] body =
          let 
            val (body', t) = typingExp l env body
          in
            (LET (rev dec', body'), t)
          end
      | typingLet l dec' env (Syntax.VAL (x, m) :: dec) body =
          let
            val (m', t1) = typingExp (l + 1) env m
            val x' = (Id.gensym x, Type.generalize l t1)
            val env' = Env.insertList (env, [x'])
          in
            typingLet l (VAL (x', m') :: dec') env' dec body
          end
      | typingLet l dec' env (Syntax.VALREC (f, m) :: dec) body =
          let
            val f' = (Id.gensym f, Type.genvar (l + 1))
            val (m', t1) = typingExp (l + 1) (Env.insertList (env, [idToPolyId f'])) m
          in
            Type.unify (idTypeOf f', t1);
            let
              val f' = (polyIdNameOf f', Type.generalize l (idTypeOf f'))
              val env' = Env.insertList (env, [f'])
            in
              typingLet l (VALREC (f', m') :: dec') env' dec body
            end
          end

    (* typing expression *)
    fun typing e = #1 (typingExp 0 Env.empty e)
  end
end
