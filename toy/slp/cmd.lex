(* ML Declarations *)

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token


exception LexerError of string



val lineNum = ref 1
val linePos = ref [1]

fun strToInt (str) =
    case Int.fromString (str) of
       SOME n => n 
    |  _      => raise LexerError "error in lexer: integer expected" 


fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%header (functor CmdLexFun(structure Tokens: Cmd_TOKENS));
letter =[a-zA-Z];
digit  =[0-9];
digits ={digit}+;
letterOrDigit = {letter}|{digit};

ws = " ";
id = {letter}{letterOrDigit}*;

%%

<INITIAL>"+"      => (Tokens.PLUS   (yypos, yypos + 1 )); 
<INITIAL>"-"      => (Tokens.MINUS  (yypos, yypos + 1 )); 
<INITIAL>"*"      => (Tokens.TIMES  (yypos, yypos + 1 )); 
<INITIAL>"/"      => (Tokens.DIV    (yypos, yypos + 1 )); 
<INITIAL>"("      => (Tokens.LPAREN (yypos, yypos + 1 )); 
<INITIAL>")"      => (Tokens.RPAREN (yypos, yypos + 1 )); 
<INITIAL>";"      => (Tokens.SEMICOLON (yypos, yypos + 1 )); 
<INITIAL>"if"     => (Tokens.IF     (yypos, yypos + size (yytext) )); 
<INITIAL>"then"   => (Tokens.THEN   (yypos, yypos + size (yytext) )); 
<INITIAL>"else"   => (Tokens.ELSE   (yypos, yypos + size (yytext) )); 
<INITIAL>":="     => (Tokens.ASSIGN (yypos, yypos + size (yytext) )); 


<INITIAL>{digits} => (Tokens.INT    (strToInt( yytext), yypos, yypos + size (yytext)));
<INITIAL>{id}     => (Tokens.ID     (yytext, yypos, yypos + size (yytext)));



\r                => (continue());
\n                => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
(" "|\t)+         => (continue());
.                 => (raise LexerError("illegal character " ^ yytext));

