structure Udon = struct


structure UdonLrVals = UdonLrValsFun(structure Token = LrParser.Token)
structure UdonLex = UdonLexFun(structure Tokens = UdonLrVals.Tokens)
structure UdonParser = Join(structure LrParser = LrParser
                           structure ParserData = UdonLrVals.ParserData
                           structure Lex = UdonLex)
structure Inlining = InliningFun (val threshold = 100)

fun foldn f 0 x = x
  | foldn f n x = foldn f (n - 1) (f x)

fun exec exp stat =
  ((print
    o Virtual.programToString
    o Virtual.codeGeneration
    o Closure.closureConv
    o foldn
        (DeadCodeElim.deadCodeElim
         o ConstFold.constFold
         o Inlining.inlining
         o Eta.etaReduction
         o Beta.betaReduction
         o Cse.cse
         o ControlFlow.controlFlowAnalysis) 10
    o (fn e => (print (Cps.expToString e ^ "\n\n"); e))
    o Sinking.sinking
    o ControlFlow.controlFlowAnalysis
    o Hoisting.hoisting
    o (fn exp => TranslCps.transl exp (fn t =>
        let val x = Id.gensym "x" in
          Cps.LET_REC ([(x, t)], Cps.APP (Id.gensym "HALT", [x]))
        end))
    o (fn e => (print (TypedSyntax.expToString e ^ "\n\n"); e))
    o Uncurrying.uncurrying
    o Typing.typing
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
  let
    val (result, lexer') = UdonParser.parse(0, lexer, print_error, ())
	val stat' = exec result stat
	val (next, lexer'') = UdonParser.Stream.get lexer'
  in
    readEvalPrintLoop stat' lexer''
  end
val lexer = UdonParser.makeLexer (fn _ => valOf (TextIO.inputLine TextIO.stdIn))
fun run (cmd, args) = (readEvalPrintLoop () lexer; OS.Process.success)

end
