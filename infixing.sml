structure Infixing : INFIXING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string
  local
    open Infixer
    open ConcreteSyntax
  in
    fun reduceBinOp op1 (e1, e2) =
      Syntax.APP (Syntax.VAR op1, Syntax.TUPLE [e1, e2])

    fun infixing env (CONST c) = Syntax.CONST c
      | infixing env (VAR x) = Syntax.VAR x
      | infixing env (OP x) = Syntax.VAR x
      | infixing env (IF (m, n1, n2)) =
          let
            val m' = infixing env m
            val n1' = infixing env n1
            val n2' = infixing env n2
          in
            Syntax.IF (m', n1', n2')
          end
      | infixing env (ABS (x, m)) =
          Syntax.ABS (x, infixing env m)
      | infixing env (LET (dec, m)) =
          infixingLet [] env dec m
      | infixing env (SEQ ms) =
          let
            fun lookahead (VAR x :: _) =
                  (case StringMap.find (env, x) of
                        SOME desc =>
                          Token.BINOP desc
                      | NONE =>
                          Token.EXP (Syntax.VAR x))
              | lookahead (m :: _) =
                  Token.EXP (infixing env m)
              | lookahead [] = Token.EOI
          in
            parseExp
              {getToken = tl,
               lookahead = lookahead,
               reduceApp = Syntax.APP} ms
          end
      | infixing env (PAREN m) = infixing env m
      | infixing env (TUPLE ms) =
          Syntax.TUPLE (map (infixing env) ms)
      | infixing env (CASE (m, xs, n)) =
          let
            val m' = infixing env m
            val n' = infixing env n
          in
            Syntax.CASE (m', xs, n')
          end

    and infixingLet dec' env [] body =
          Syntax.LET (rev dec', infixing env body) 
      | infixingLet dec' env (VAL (x, m) :: dec) body =
          infixingLet (Syntax.VAL (x, infixing env m) :: dec') env dec body
      | infixingLet dec' env (VALREC (f, m) :: dec) body =
          infixingLet (Syntax.VALREC (f, infixing env m) :: dec') env dec body
      | infixingLet dec' env (INFIX (assoc, d, vids) :: dec) body =
          let
            val env' =
              foldl StringMap.insert' env
                (map (fn vid =>
                  (vid, (reduceBinOp vid, d, assoc))) vids)
          in
            infixingLet dec' env' dec body
          end
      | infixingLet dec' env (NONFIX vids :: dec) body =
          let
            val env' =
              foldl (fn (vid, env) =>
              #1 (StringMap.remove (env, vid))) env vids
          in
            infixingLet dec' env' dec body
          end
  end
end
