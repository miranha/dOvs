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
{letter}{idchars}          => (dopos3 Tokens.ID  yytext yypos
			       (size yytext));
{digits}                   => (dopos3 Tokens.INT (s2i yytext yypos) yypos
                                                 (size yytext));
.                          => (ErrorMsg.error yypos ("illegal char " ^ yytext);
                               continue());

