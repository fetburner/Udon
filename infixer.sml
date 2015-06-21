structure Infixer = struct
  local
    open Assoc
  in
    datatype 'a token = EXP_TOKEN of 'a | BINOP_TOKEN of Id.t * int * assoc

    fun parseExp
      {getToken = getToken,
       reduceApp = reduceApp,
       reduceBinOp = reduceBinOp,
       syntaxError = syntaxError} =
      let
        fun parseFactor seq =
          case getToken seq of
               SOME (EXP_TOKEN e, seq') => SOME (e, seq')
             | SOME (BINOP_TOKEN _, _) => NONE
             | NONE => NONE

        fun parseTerm' e seq =
          case parseFactor seq of
               SOME (e', seq') =>
                 parseTerm' (reduceApp (e, e')) seq'
             | NONE => SOME (e, seq)
        fun parseTerm seq =
          case parseFactor seq of
               SOME (e, seq') => parseTerm' e seq'
             | NONE => NONE

        fun parseExp' e1 (op1, prio1, assoc1) seq =
          case parseTerm seq of
               SOME (e2, seq') => 
                 (case getToken seq' of
                       SOME (BINOP_TOKEN (op2, prio2, assoc2), seq'') => 
                         if prio1 = prio2 andalso assoc1 <> assoc2 then
                           syntaxError ()
                         else if prio1 < prio2 orelse prio1 = prio2 andalso assoc1 = RIGHT_ASSOC then
                           Option.map (fn (e, seq0) =>
                             (reduceBinOp (e1, op1, e), seq0))
                               (parseExp' e2 (op2, prio2, assoc2) seq'')
                         else
                           parseExp' (reduceBinOp (e1, op1, e2))
                             (op2, prio2, assoc2) seq''
                     | SOME (EXP_TOKEN _, _) => NONE
                     | NONE => SOME (reduceBinOp (e1, op1, e2), seq'))
             | NONE => NONE

        fun parseExp seq =
          case parseTerm seq of
               SOME (e, seq') => 
                 (case getToken seq' of
                       SOME (BINOP_TOKEN (op1, prio, assoc), seq'') =>
                         parseExp' e (op1, prio, assoc) seq''
                     | SOME (EXP_TOKEN _, _) => NONE
                     | NONE => SOME (e, seq'))
             | NONE => NONE
      in
        parseExp
      end
  end
end
