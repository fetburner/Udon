structure Infixing : INFIXING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string
  exception SyntaxError
  local
    open Assoc
    open Infixer
    open ConcreteSyntax
  in
    fun infixing env (CONST c) = Syntax.CONST c
      | infixing env (VAR x) =
          (case Env.findName (env, x) of
                SOME (x', _) => Syntax.VAR x'
              | NONE => raise (UnboundVar x))
      | infixing env (OP x) =
          (case Env.findName (env, x) of
                SOME (id, _) => Syntax.VAR id
              | NONE => raise (UnboundVar x))
      | infixing env (IF (m, n1, n2)) =
          let
            val m' = infixing env m
            val n1' = infixing env n1
            val n2' = infixing env n2
          in
            Syntax.IF (m', n1', n2')
          end
      | infixing env (ABS (x, m)) =
          let
            val x' = Id.gensym x
            val env' = Env.insert (env, x', NONE)
            val m' = infixing env' m
          in
            Syntax.ABS (x', m')
          end
      | infixing env (LET (dec, m)) = infixingLet [] env dec m
      | infixing env (SEQ ms) =
          let
            fun infixingSeq env ms =
              (case parseExp env ms of
                    SOME (e, _) => e
                  | NONE => raise SyntaxError)

            fun getToken (VAR x :: seq') =
                (case Env.findName (env, x) of
                      SOME (x', SOME (prio, assoc)) =>
                        SOME (BINOP_TOKEN (x', prio, assoc), seq')
                    | SOME (x', NONE) =>
                        SOME (EXP_TOKEN (Syntax.VAR x'), seq')
                    | NONE => raise (UnboundVar x))
              | getToken (m :: seq') = SOME (EXP_TOKEN (infixing env m), seq')
              | getToken [] = NONE

            fun reduceBinOp (e1, op1, e2) =
              Syntax.APP (Syntax.VAR op1, Syntax.TUPLE [e1, e2])
          in
            case parseExp
                   {getToken = getToken,
                    reduceApp = Syntax.APP,
                    reduceBinOp = reduceBinOp,
                    syntaxError = fn () => raise SyntaxError}
                   ms of
                 SOME (e, _) => e
               | NONE => raise SyntaxError
          end
      | infixing env (PAREN m) = infixing env m
      | infixing env (TUPLE ms) =
          Syntax.TUPLE (map (infixing env) ms)
      | infixing env (CASE (m, xs, n)) =
          let
            val m' = infixing env m
            val xs' = map Id.gensym xs
            val env' = Env.insertList (env, map (fn x => (x, NONE)) xs')
            val n' = infixing env' n
          in
            Syntax.CASE (m', xs', n')
          end

    and infixingLet dec' env [] body =
          Syntax.LET (rev dec', infixing env body) 
      | infixingLet dec' env (VAL (x, m) :: dec) body =
          let
            val x' = Id.gensym x
            val m' = infixing env m
            val env' = Env.insert (env, x', NONE)
          in
            infixingLet (Syntax.VAL (x', m') :: dec') env' dec body
          end
      | infixingLet dec' env (VALREC (f, x, m) :: dec) body =
          let 
            val f' = Id.gensym f
            val x' = Id.gensym x
            val env' = Env.insert (env, f', NONE)
            val m' = infixing (Env.insert (env', x', NONE)) m
          in
            infixingLet (Syntax.VALREC (f', x', m') :: dec') env' dec body
          end
      | infixingLet dec' env (INFIX (assoc, d, vids) :: dec) body =
          let
            val vids' =
              map (fn vid =>
                case Env.findName (env, vid) of
                     SOME (vid', _) => (vid', SOME (d, assoc))
                   | NONE => raise (UnboundVar vid)) vids
            val env' = Env.insertList (env, vids')
          in
            infixingLet dec' env' dec body
          end
      | infixingLet dec' env (NONFIX vids :: dec) body =
          let
            val vids' = 
              map (fn vid =>
                case Env.findName (env, vid) of
                     SOME (vid', _) => (vid', NONE)
                   | NONE => raise (UnboundVar vid)) vids
            val env' = Env.insertList (env, vids')
          in
            infixingLet dec' env' dec body
          end
  end
end
