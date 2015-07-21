structure Infixer = struct
  open State

  exception InconsistentPriority
  exception BeginsInfixOp
  exception EndsInfixOp

  structure Assoc = struct
    datatype assoc = LEFT | RIGHT
  end

  structure Token = struct
    datatype 'a token =
        EXP of 'a
      | BINOP of ('a * 'a -> 'a) * int * Assoc.assoc
      | EOI
  end

  infix >>= >>

  fun parseExp
    {getToken = getToken',
     lookahead = lookahead',
     reduceApp = reduceApp} =
    let
      val getToken = modify getToken'

      val lookahead =
        get >>= (fn s => return (lookahead' s))

      fun parseTerm' e =
        lookahead >>= (fn
            Token.EXP e' => getToken >> parseTerm' (reduceApp (e, e'))
          | Token.BINOP _ => return e
          | Token.EOI => return e)
      fun parseTerm s =
        (lookahead >>= (fn
            Token.EXP e => getToken >> parseTerm' e
          | Token.BINOP _ => raise BeginsInfixOp
          | Token.EOI => raise EndsInfixOp)) s

      fun parseExp' e1 (op1 as (reduce1, prio1, assoc1)) =
        parseTerm >>= (fn e2 =>
          lookahead >>= (fn
              Token.BINOP (op2 as (_, prio2, assoc2)) =>
                getToken >>
                  (if prio1 = prio2 andalso assoc1 <> assoc2 then
                    raise InconsistentPriority
                  else if
                    prio1 < prio2
                    orelse prio1 = prio2
                      andalso assoc1 = Assoc.RIGHT
                  then
                    parseExp' e2 op2 >>= (fn e =>
                      return (reduce1 (e1, e)))
                  else
                    parseExp' (reduce1 (e1, e2)) op2)
             (* | Token.EXP _ => (* Don't worry, Be happy. *) *)
             | Token.EOI => return (reduce1 (e1, e2))))
      fun parseExp s =
        (parseTerm >>= (fn e =>
          lookahead >>= (fn
              Token.BINOP op1 => getToken >> parseExp' e op1
            (* | Token.EXP _ => (* Don't worry, Be happy. *) *)
            | Token.EOI => return e))) s
    in
      #1 o parseExp
    end
end
