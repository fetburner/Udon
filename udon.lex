structure Tokens = Tokens
open Tokens

type pos = unit
type svalue = svalue
type ('a, 'b) token = ('a, 'b) token
type lexresult = (svalue, pos) token

fun eof () = SEMICOLON((),())
fun error (e, pos, _) = (print e; print "\n")



%%

%header (functor UdonLexFun(structure Tokens : Udon_TOKENS));

space=[\ \t\n\r];
digit=[0-9];
alpha=[A-Za-z];



%%

"else" => (ELSE((),()));
"end" => (END((),()));
"fn" => (FN((),()));
"if" => (IF((),()));
"in" => (IN((),()));
"let" => (LET((),()));
"op" => (OP((),()));
"rec" => (REC((),()));
"then" => (THEN((),()));
"val" => (VAL((),()));
"true" => (SCON(Const.BOOL(true),(),()));
"false" => (SCON(Const.BOOL(false),(),()));
"=" => (EQUAL((),()));
"," => (COMMA((),()));
"+" => (PLUS((),()));
"-" => (MINUS((),()));
"*" => (AST((),()));
"(" => (LPAREN((),()));
")" => (RPAREN((),()));
"<=" => (LE((),()));
"=>" => (FATARROW((),()));
";" => (SEMICOLON((),()));
"~"? {digit}+ => (SCON(Const.INT(valOf(Int.fromString yytext)),(),()));
{alpha} ({alpha} | {digit} | "_")* => (VID(yytext,(),()));
{space}+ => (lex()(* skip spaces and continue lexing *));

