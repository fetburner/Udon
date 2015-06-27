structure Udon = struct


structure UdonLrVals = UdonLrValsFun(structure Token = LrParser.Token)
structure UdonLex = UdonLexFun(structure Tokens = UdonLrVals.Tokens)
structure UdonParser = Join(structure LrParser = LrParser
                           structure ParserData = UdonLrVals.ParserData
                           structure Lex = UdonLex)

fun exec exp out =
  (((fn str => TextIO.output (out, str))
    o Js.progToString
    o Js.transl
    o TranslCps.simpExp []
    o (fn exp => TranslCps.transl exp (Cps.CVAR (Id.gensym "HALT")))
    o Uncurrying.uncurrying
    o Typing.typing 0 (Env.fromList Prim.typeInfoBindings)
    o Infixing.infixing (Env.fromList Prim.infixInfoBindings)) exp
   handle
     Infixing.SyntaxError =>
       print "Syntax error"
   | Type.Unify (t1, t2) =>
       print
         ("Error : failed to unify "
           ^ Type.toString t1
           ^ " and "
           ^ Type.toString t2)
   | Infixing.UnboundVar x =>
       print ("Error : unbound variable " ^ x)
   | UnequalLengths =>
       print "Error : inconsistent arity")

fun print_error (s, _, _) = (print s; print "\n")

fun run (cmd, args) =
  if List.length args = 0 then OS.Process.failure else
  let
    val fileName = List.nth (args, 0)
    val outName = fileName ^ ".js"
    val out = TextIO.openOut outName
    val lexer = UdonParser.makeLexer (fn _ => TextIO.input (TextIO.openIn fileName))
    val (result, lexer') = UdonParser.parse(0, lexer, print_error, ())
  in
    TextIO.output (out, TextIO.input (TextIO.openIn "jsTemplete.js"));
    exec result out;
    TextIO.closeOut out;
    OS.Process.success
  end

end
