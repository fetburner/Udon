structure Udon = struct


structure UdonLrVals = UdonLrValsFun(structure Token = LrParser.Token)
structure UdonLex = UdonLexFun(structure Tokens = UdonLrVals.Tokens)
structure UdonParser = Join(structure LrParser = LrParser
                           structure ParserData = UdonLrVals.ParserData
                           structure Lex = UdonLex)

fun foldn f 0 x = x
  | foldn f n x = foldn f (n - 1) (f x)

fun exec exp stat =
  ((print
    o Cps.expToString
    o foldn
        (Eta.etaReduction
         o ConstFold.constFold Env.empty
         o Hoisting.hoisting
         o Beta.betaReduction Env.empty) 10
    o (fn exp => TranslCps.transl exp (fn t =>
        let val x = Id.gensym "x" in
          Cps.LET ((x, t), Cps.APP_TAIL (Id.gensym "HALT", x))
        end))
    o (fn e => (print (TypedSyntax.expToString e ^ "\n\n"); e))
    o Typing.typing 0 Injection.typeInfo
    o Infixing.infixing Injection.infixInfo) exp
   handle
    Type.Unify (t1, t2) =>
       print
         ("Error : failed to unify "
           ^ Type.toString t1
           ^ " and "
           ^ Type.toString t2)
   | Infixing.UnboundVar x =>
       print ("Error : unbound variable " ^ x)
   | ListPair.UnequalLengths =>
       print "Error : inconsistent arity"
   | Infixer.BeginsInfixOp =>
       print "Error : expression or pattern begins with infix identifier"
   | Infixer.EndsInfixOp =>
       print "Error : expression or pattern ends with infix identifier";
   print "\n";
   stat)

fun print_error (s, _, _) = (print s; print "\n")
(* REPL *)
fun readEvalPrintLoop stat lexer =
    let val (result, lexer') = UdonParser.parse(0, lexer, print_error, ())
	val stat' = exec result stat
	val (next, lexer'') = UdonParser.Stream.get lexer'
    in readEvalPrintLoop stat' lexer''
    end
val lexer = UdonParser.makeLexer (fn _ => valOf (TextIO.inputLine TextIO.stdIn))
fun run () = readEvalPrintLoop () lexer

end
