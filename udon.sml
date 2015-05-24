structure Udon = struct


structure UdonLrVals = UdonLrValsFun(structure Token = LrParser.Token)
structure UdonLex = UdonLexFun(structure Tokens = UdonLrVals.Tokens)
structure UdonParser = Join(structure LrParser = LrParser
                           structure ParserData = UdonLrVals.ParserData
                           structure Lex = UdonLex)

fun exec exp stat =
  (print (Syntax.expToString exp);
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
