structure Udon = struct


structure UdonLrVals = UdonLrValsFun(structure Token = LrParser.Token)
structure UdonLex = UdonLexFun(structure Tokens = UdonLrVals.Tokens)
structure UdonParser = Join(structure LrParser = LrParser
                           structure ParserData = UdonLrVals.ParserData
                           structure Lex = UdonLex)
structure Inlining = InliningFn (struct val threshold = 10 end)

fun foldn f 0 x = x
  | foldn f n x = foldn f (n - 1) (f x)

fun exec exp stat =
  ((print
    (* o Js.progToString *)
    (* o Js.transl *)
    (* o (fn e => (print (Cps.expToString e); print "\n\n"; e)) *)
    o Cps.expToString
    o foldn
        (DeadCodeElim.deadCodeElim
          o Inlining.inlining
              (Env.fromList (map (fn (id, p) =>
                let
                  val tuple = Id.gensym "tuple"
                  val x = Id.gensym "x"
                  val y = Id.gensym "y"
                  val result = Id.gensym "result"
                  val k = Id.gensym "k"
                in
                  (id, Inlining.FUN_ABS (([tuple], k),
                    Cps.LET ((x, Cps.PRIM (Prim.TUPLE_GET 1, [Cps.VAR tuple])),
                      Cps.LET ((y, Cps.PRIM (Prim.TUPLE_GET 2, [Cps.VAR tuple])),
                        Cps.LET ((result, Cps.PRIM (p, [Cps.VAR x, Cps.VAR y])),
                          Cps.APP_TAIL (k, Cps.VAR result))))))
                end) Prim.primitives))
          o ConstFold.constFold Env.empty
          o Alpha.alphaConv Env.empty) 10
    o (fn exp => TranslCps.transl exp (Cps.CVAR (Id.gensym "HALT")))
    o (fn e => (print (TypedSyntax.expToString e ^ "\n\n"); e))
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
       print "Error : inconsistent arity";
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
