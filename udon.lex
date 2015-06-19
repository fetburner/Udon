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
letter=[a-z];
alpha=[A-Za-z];



%%

"case" => (CASE((),()));
"else" => (ELSE((),()));
"end" => (END((),()));
"fn" => (FN((),()));
"if" => (IF((),()));
"in" => (IN((),()));
"infix" => (INFIX((),()));
"infixr" => (INFIXR((),()));
"let" => (LET((),()));
"nonfix" => (NONFIX((),()));
"of" => (OF((),()));
"op" => (OP((),()));
"rec" => (REC((),()));
"then" => (THEN((),()));
"val" => (VAL((),()));
"true" => (BOOL(true,(),()));
"false" => (BOOL(false,(),()));
"=" => (EQUAL((),()));
"," => (COMMA((),()));
"(" => (LPAREN((),()));
")" => (RPAREN((),()));
"=>" => (FATARROW((),()));
";" => (SEMICOLON((),()));
"~"? {digit}+ => (DIGIT(valOf(Int.fromString yytext),(),()));
{letter} ({alpha} | {digit} | "'" |  "_")* => (IDENT(yytext,(),()));
("!" | "%" | "&" | "$" | "#" | "+" | "-" | "/" | ":" | "<" | "=" | ">" | "?" | "@" | "\\" | "~" | "`" | "^" | "|" | "*")+ => (IDENT(yytext,(),()));
{space}+ => (lex()(* skip spaces and continue lexing *));

