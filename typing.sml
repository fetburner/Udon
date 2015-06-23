structure Typing : TYPING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of Id.t
  local
    open TypedSyntax
  in
    (* perform type inference *)
    fun typingExp l env (Syntax.CONST c) =
          E (CONST c, Const.typeOf c)
      | typingExp l env (Syntax.VAR x) =
          (case Env.find (env, x) of
             NONE => raise (UnboundVar x)
              | SOME t => E (VAR x, Type.inst l t))
      | typingExp l env (Syntax.IF (m, n1, n2)) =
          let
            val m' = typingExp l env m
            val n1' = typingExp l env n1
            val n2' = typingExp l env n2
          in
            Type.unify (expTypeOf m', Type.BOOL);
            Type.unify (expTypeOf n1', expTypeOf n2');
            E (IF (m', n1', n2'), expTypeOf n1')
          end
      | typingExp l env (Syntax.ABS (x, m)) =
          let
            val x' = (x, Type.genvar l)
            val m' = typingExp l (Env.insertList (env, [x'])) m
          in
            E (ABS (x', m'), Type.FUN ([idTypeOf x'], expTypeOf m'))
          end
      | typingExp l env (Syntax.APP (m, n)) =
          let
            val m' = typingExp l env m
            val n' = typingExp l env n
            val t12 = Type.genvar l
          in
            Type.unify (expTypeOf m', Type.FUN ([expTypeOf n'], t12));
            E (APP (m', n'), t12)
          end
      | typingExp l env (Syntax.LET (dec, m)) =
          typingLet l [] env dec m
      | typingExp l env (Syntax.TUPLE ms) =
          let
            val ms' = map (typingExp l env) ms
          in
            E (TUPLE ms', Type.TUPLE (map expTypeOf ms'))
          end
      | typingExp l env (Syntax.CASE (m, xs, n)) =
          let
            val m' = typingExp l env m
            val xs' = map (fn x => (x, Type.genvar l)) xs
            val n' = typingExp l (Env.insertList (env, xs')) n
          in
            Type.unify (expTypeOf m', Type.TUPLE (idSeqTypeOf xs'));
            E (CASE (m', xs', n'), expTypeOf n')
          end
    and typingLet l dec' env [] body =
          let 
            val body' = typingExp l env body
          in
            E (LET (rev dec', body'), expTypeOf body')
          end
      | typingLet l dec' env (Syntax.VAL (x, m) :: dec) body =
          let
            val m' = typingExp (l + 1) env m
            val t = Type.generalize l (expTypeOf m')
            val env' = Env.insert (env, x, t)
          in
            typingLet l (VAL ((x, t), m') :: dec') env' dec body
          end
      | typingLet l dec' env (Syntax.VALREC (f, x, m) :: dec) body =
          let
            val f' = (f, Type.genvar (l + 1))
            val x' = (x, Type.genvar (l + 1))
            val m' = typingExp (l + 1) (Env.insertList (env, [f', x'])) m
            val () = Type.unify (idTypeOf f', Type.FUN ([idTypeOf x'], expTypeOf m'))
            val f' = (f, Type.generalize l (idTypeOf f'))
            val env' = Env.insertList (env, [f'])
          in
            typingLet l (VALREC (f', x', m') :: dec') env' dec body
          end

    (* typing expression *)
    fun typing l env m = typingExp l env m
  end
end
