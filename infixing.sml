structure Infixing : INFIXING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string
  exception SyntaxError
  local
    open Assoc
    open ConcreteSyntax
    datatype token = EXP_TOKEN of Syntax.exp | BINOP_TOKEN of Id.t * int * assoc
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
      | infixing env (ABS (xs, m)) =
          let
            val xs' = map Id.gensym xs
            val env' = Env.insertList (env, map (fn x => (x, NONE)) xs')
            val m' = infixing env' m
          in
            Syntax.ABS (xs', m')
          end
      | infixing env (APP (m, ns)) =
          let
            val m' = infixing env m
            val ns' = map (infixing env) ns
          in
            Syntax.APP (m', ns')
          end
      | infixing env (LET (dec, m)) = infixingLet [] env dec m
      | infixing env (SEQ ms) = infixingSeq env ms
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
      | infixingLet dec' env (VALREC (f, xs, m) :: dec) body =
          let 
            val f' = Id.gensym f
            val xs' = map Id.gensym xs
            val env' = Env.insert (env, f', NONE)
            val m' =
              infixing (Env.insertList (env', map (fn x => (x, NONE)) xs')) m
          in
            infixingLet (Syntax.VALREC (f', xs', m') :: dec') env' dec body
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

    and infixingSeq env ms =
          (case parseExp env ms of
                SOME (e, _) => e
              | NONE => raise SyntaxError)

    and getToken env (VAR x :: seq') =
          (case Env.findName (env, x) of
                SOME (x', SOME (prio, assoc)) => SOME (BINOP_TOKEN (x', prio, assoc), seq')
              | SOME (x', NONE) => SOME (EXP_TOKEN (Syntax.VAR x'), seq')
              | NONE => raise (UnboundVar x))
      | getToken env (m :: seq') = SOME (EXP_TOKEN (infixing env m), seq')
      | getToken env [] = NONE

    and parseFactor env seq =
          (case getToken env seq of
                SOME (EXP_TOKEN e, seq') => SOME (e, seq')
              | SOME (BINOP_TOKEN _, _) => NONE
              | NONE => NONE)

    and parseTerm env seq =
          (case parseFactor env seq of
                SOME (e, seq') => parseTerm' env e seq'
              | NONE => NONE)
    and parseTerm' env e seq =
          (case parseFactor env seq of
                SOME (e', seq') => parseTerm' env (Syntax.APP (e, [e'])) seq'
              | NONE => SOME (e, seq))

    and parseExp env seq =
          (case parseTerm env seq of
                SOME (e, seq') => 
                  (case getToken env seq' of
                        SOME (BINOP_TOKEN (op1, prio, assoc), seq'') =>
                          parseExp' env e (op1, prio, assoc) seq''
                      | SOME (EXP_TOKEN _, _) => NONE
                      | NONE => SOME (e, seq'))
              | NONE => NONE)
    and parseExp' env e1 (op1, prio1, assoc1) seq =
          (case parseTerm env seq of
                SOME (e2, seq') => 
                  (case getToken env seq' of
                        SOME (BINOP_TOKEN (op2, prio2, assoc2), seq'') => 
                          if prio1 = prio2 andalso assoc1 <> assoc2 then
                            raise SyntaxError
                          else if prio1 < prio2 orelse prio1 = prio2 andalso assoc1 = RIGHT_ASSOC then
                            Option.map (fn (e, seq0) => (Syntax.APP (Syntax.VAR op1, [Syntax.TUPLE [e1, e]]), seq0))
                              (parseExp' env e2 (op2, prio2, assoc2) seq'')
                          else
                            parseExp' env (Syntax.APP (Syntax.VAR op1, [Syntax.TUPLE [e1, e2]]))
                              (op2, prio2, assoc2) seq''
                      | SOME (EXP_TOKEN _, _) => NONE
                      | NONE => SOME (Syntax.APP (Syntax.VAR op1, [Syntax.TUPLE [e1, e2]]), seq'))
              | NONE => NONE)
  end
end
