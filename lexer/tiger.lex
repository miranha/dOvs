type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentLevel = ref 0
val currentString = ref ""
val stringStart = ref 0

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

  %s COMMENT STRING;

letter=[a-zA-Z];
digits=[0-9]+;
idchars=[a-zA-Z0-9_]*;
%%

<INITIAL COMMENT>"\n"	                   => (lineNum := !lineNum+1;
                               linePos := yypos :: !linePos;
                               continue());
<INITIAL>","                        => (dopos Tokens.COMMA yypos 1);
"var"                      => (dopos Tokens.VAR yypos 3);

<INITIAL>"type"					    => (dopos Tokens.TYPE yypos 4);
<INITIAL>"function"				    => (dopos Tokens.FUNCTION yypos 8);
<INITIAL>"break"					    => (dopos Tokens.BREAK yypos 5);
<INITIAL>"of"					    => (dopos Tokens.OF yypos 2);
<INITIAL>"end"						=> (dopos Tokens.END yypos 3);
<INITIAL>"in"						=> (dopos Tokens.IN yypos 2);
<INITIAL>"nil"						=> (dopos Tokens.NIL yypos 3);
<INITIAL>"let"						=> (dopos Tokens.LET yypos 3);
<INITIAL>"do"						=> (dopos Tokens.DO yypos 2);
<INITIAL>"to"						=> (dopos Tokens.TO yypos 2);
<INITIAL>"for"						=> (dopos Tokens.FOR yypos 3);
<INITIAL>"while"						=> (dopos Tokens.WHILE yypos 5);
<INITIAL>"else" 						=> (dopos Tokens.ELSE yypos 4);
<INITIAL>"then"						=> (dopos Tokens.THEN yypos 4);
<INITIAL>"if"						=> (dopos Tokens.IF yypos 2);
<INITIAL>"array"						=> (dopos Tokens.ARRAY yypos 5);
<INITIAL>":="					    => (dopos Tokens.ASSIGN yypos 2);
<INITIAL>"|"							=> (dopos Tokens.OR yypos 1);
<INITIAL>"&"							=> (dopos Tokens.AND yypos 1);
<INITIAL>">="						=> (dopos Tokens.GE yypos 2);
<INITIAL>">"							=> (dopos Tokens.GT yypos 1);
<INITIAL>"<="						=> (dopos Tokens.LE yypos 2);
<INITIAL>"<"							=> (dopos Tokens.LT yypos 1);
<INITIAL>"<>"						=> (dopos Tokens.NEQ yypos 2);
<INITIAL>"="							=> (dopos Tokens.EQ yypos 1);
<INITIAL>"/"							=> (dopos Tokens.DIVIDE yypos 1);
<INITIAL>"*"							=> (dopos Tokens.TIMES yypos 1);
<INITIAL>"+"							=> (dopos Tokens.PLUS yypos 1);
<INITIAL>"-"							=> (dopos Tokens.MINUS yypos 1);
<INITIAL>"."							=> (dopos Tokens.DOT yypos 1);
<INITIAL>"}"							=> (dopos Tokens.RBRACE yypos 1);
<INITIAL>"{"							=> (dopos Tokens.LBRACE yypos 1);
<INITIAL>"]"							=> (dopos Tokens.RBRACK yypos 1);
<INITIAL>"["							=> (dopos Tokens.LBRACK yypos 1);
<INITIAL>")"							=> (dopos Tokens.RPAREN yypos 1);
<INITIAL>"("							=> (dopos Tokens.LPAREN yypos 1);
<INITIAL>";"							=> (dopos Tokens.SEMICOLON yypos 1);
<INITIAL>":"							=> (dopos Tokens.COLON yypos 1);
<INITIAL>"^"							=> (dopos Tokens.CARET yypos 1);

<INITIAL>"\"" => (YYBEGIN STRING; currentString := ""; stringStart := yypos; continue());


 				
<INITIAL>{letter}{idchars}          => (dopos3 Tokens.ID  yytext yypos
			       (size yytext));
<INITIAL>{digits}                   => (dopos3 Tokens.INT (s2i yytext yypos) yypos
                                                 (size yytext));
<INITIAL>.                          => (ErrorMsg.error yypos ("illegal char " ^ yytext);
                               continue());

"/*"						=> (commentLevel := !commentLevel+1; YYBEGIN COMMENT; continue());

<COMMENT>"*/"				=> (commentLevel := !commentLevel-1; if !commentLevel < 1 then YYBEGIN INITIAL else (); continue());
<COMMENT>.	=> (continue());

<STRING> {letter}|{digits}|" " => (currentString := !currentString ^ yytext; continue());

<STRING> "\"" => (YYBEGIN INITIAL; dopos3 Tokens.STRING (!currentString) (!stringStart) (String.size (!currentString)));

<STRING>. => (ErrorMsg.error yypos ("I do not yet know how to handle this part of a string " ^ yytext); continue());
