type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentLevel = ref 0
val currentString = ref ""

fun err (p1,p2) = ErrorMsg.error p1

fun eof () =
    let
        val pos = hd (!linePos)
    in
        Tokens.EOF (pos,pos)
    end

fun s2i t pos =
    let
        val opti = (Int.fromString t) 
            handle Overflow => 
                   (ErrorMsg.error pos "Integer too large"; SOME 0)
        fun s2i_aux NONE = (ErrorMsg.error pos "Ill-formed integer"; 0)
          | s2i_aux (SOME n) = n
    in
        s2i_aux opti
    end

fun dopos token yypos yylen = token (yypos, yypos + yylen)
fun dopos3 token value yypos yylen = token (value, yypos, yypos + yylen)

%%
letter=[a-zA-Z];
digits=[0-9]+;
idchars=[a-zA-Z0-9_]*;
%%

"\n"	                   => (lineNum := !lineNum+1;
                               linePos := yypos :: !linePos;
                               continue());
","                        => (dopos Tokens.COMMA yypos 1);
"var"                      => (dopos Tokens.VAR yypos 3);

"type"					   => (dopos Tokens.TYPE yypos 4);
"function"				   => (dopos Tokens.FUNCTION yypos 8);
"break"					   => (dopos Tokens.BREAK yypos 5);
"of"					   => (dopos Tokens.OF yypos 2);
"end"						=> (dopos Tokens.END yypos 3);
"in"						=> (dopos Tokens.IN yypos 2);
"nil"						=> (dopos Tokens.NIL yypos 3);
"let"						=> (dopos Tokens.LET yypos 3);
"do"						=> (dopos Tokens.DO yypos 2);
"to"						=> (dopos Tokens.TO yypos 2);
"for"						=> (dopos Tokens.FOR yypos 3);
"while"						=> (dopos Tokens.WHILE yypos 5);
"else" 						=> (dopos Tokens.ELSE yypos 4);
"then"						=> (dopos Tokens.THEN yypos 4);
"if"						=> (dopos Tokens.IF yypos 2);
"array"						=> (dopos Tokens.ARRAY yypos 5);
"assign"					=> (dopos Tokens.ASSIGN yypos 6);
"or"						=> (dopos Tokens.OR yypos 2);
"and"						=> (dopos Tokens.AND yypos 3);
">="						=> (dopos Tokens.GE yypos 2);
">"							=> (dopos Tokens.GT yypos 1);
"<="						=> (dopos Tokens.LE yypos 2);
"<"							=> (dopos Tokens.LT yypos 1);
"/="						=> (dopos Tokens.NEQ yypos 2);
"="							=> (dopos Tokens.EQ yypos 1);
"/"							=> (dopos Tokens.DIVIDE yypos 1);
"*"							=> (dopos Tokens.TIMES yypos 1);
"+"							=> (dopos Tokens.PLUS yypos 1);
"-"							=> (dopos Tokens.MINUS yypos 1);
"."							=> (dopos Tokens.DOT yypos 1);
"}"							=> (dopos Tokens.RBRACE yypos 1);
"{"							=> (dopos Tokens.LBRACE yypos 1);
"]"							=> (dopos Tokens.RBRACK yypos 1);
"["							=> (dopos Tokens.LBRACK yypos 1);
")"							=> (dopos Tokens.RPAREN yypos 1);
"("							=> (dopos Tokens.LPAREN yypos 1);
";"							=> (dopos Tokens.SEMICOLON yypos 1);
":"							=> (dopos Tokens.COLON yypos 1);
"^"							=> (dopos Tokens.CARET yypos 1);


{letter}{idchars}          => (dopos3 Tokens.ID  yytext yypos
			       (size yytext));
{digits}                   => (dopos3 Tokens.INT (s2i yytext yypos) yypos
                                                 (size yytext));
.                          => (ErrorMsg.error yypos ("illegal char " ^ yytext);
                               continue());

