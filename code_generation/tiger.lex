(* -*- mode:sml -*- *)

type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val maxAsciiCode = 127

val commentLevel = ref 0
val currentString = ref ""
val inString = ref false
val stringStart = ref 0
val inMultiline = ref false

fun trim s = 
	case s of
		  "\\n"  => "\n"
		| "\\\\" => "\\"
		| "\\t"  => "\t"
		| "\\\"" => "\""
		| _ 	 => s

fun err (p1,p2) = ErrorMsg.error p1

fun eof () =
  (let
   val pos = hd (!linePos)
   val res1 = (if (!commentLevel > 0) then
					ErrorMsg.error pos "Reached EOF while parsing comment. Close all comment bloks" else ())
   val res3 = (if (!inMultiline) then ErrorMsg.error pos "Unclosed multiline block. Remember to close with \\" 
		 else if ((!inString)) then
					 ErrorMsg.error pos "Unclosed string. Rember to close your string with \"." else ())
   in
   Tokens.EOF (pos,pos) end)

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

  fun addToCurString s = let val _ = currentString := !currentString ^ s
  in ()
  end

  fun digitEsc pos s =
 	let 
 		val sCode = (valOf (Int.fromString (String.substring (s, 1, 3))))
 	in 
  		if sCode <= maxAsciiCode then
  				String.str(Char.chr sCode)
  		else 
  			let 
  				val _ = ErrorMsg.error pos (s ^ " of the form \\ddd must have that ddd <= " ^ (Int.toString maxAsciiCode)) 
  			in ""
  			end 
  	end

fun handleCtrl s pos =
	let 
		val char' = String.sub(s,2)
		val i' = Char.ord(char')
		fun checker i = 
			if i >= 64 andalso i <= 95 then
				String.str(Char.chr (i-64))
			else
				(ErrorMsg.error pos ("Error: CTRL char must be of the form:\n\\^{@ABCDEFGHIJKLMNOPQRSTUVWXYZ[?]^_} \nYou gave me: \\^" ^ String.str(char')); "")
	in
		if (s = "\\^?") then
		"\127"
		else checker i'
	end 


fun handleNewline ( pos ) = (lineNum := !lineNum+1;
    linePos := pos :: !linePos)

fun dopos token yypos yylen = token (yypos, yypos + yylen)
fun dopos3 token value yypos yylen = token (value, yypos, yypos + yylen)

%%
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));

  %s COMMENT STRING MULTILINE;

letter=[a-zA-Z];
digit=[0-9];
digits=[0-9]+;
idchars=[a-zA-Z0-9_]*;
ctrlchar=[@ABCDEFGHIJKLMNOPQRSTUVWXYZ"[""?"]"^""_""\\";
printable=[! "\"" # \$ % "'" "(" ")" "\*" "\+" , \- "\." "\/" ":" "\;"];
printable2=["\<" "\=" "\>" "\?" @ "\[" "\]" "\^" _ ` "\{" "\|" "\}" ~];
%%

<INITIAL>"\n"	                   => (handleNewline(yypos);continue());
<INITIAL> " "|"\t" => (continue());

<INITIAL>"*/" => (ErrorMsg.error yypos "*/ must have a matching */"; continue());

<INITIAL>","                        => (dopos Tokens.COMMA yypos 1);
<INITIAL>"var"                      => (dopos Tokens.VAR yypos 3);

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

<INITIAL>"\"" => (YYBEGIN STRING; currentString := ""; inString := true; stringStart := yypos; continue());


 				
<INITIAL>{letter}{idchars}          => (dopos3 Tokens.ID  yytext yypos
			       (size yytext));
<INITIAL>{digits}                   => (dopos3 Tokens.INT (s2i yytext yypos) yypos
                                                 (size yytext));
<INITIAL>.                          => (ErrorMsg.error yypos ("illegal char " ^ yytext);
                               continue());

<INITIAL, COMMENT>"/*"						=> (commentLevel := ((!commentLevel)+1); YYBEGIN COMMENT; continue());

<COMMENT>"*/"				=> (commentLevel := ((!commentLevel)-1); if !commentLevel < 1 then YYBEGIN INITIAL else (); continue());

<COMMENT>"\n" => (handleNewline yypos; continue());
<COMMENT>.	=> (continue());


<STRING> "\"" => (YYBEGIN INITIAL; inString := false; dopos3 Tokens.STRING (!currentString) (!stringStart) (String.size (!currentString)));

<STRING> "\\n"|"\\t"|"\\\\"|"\\\"" => (addToCurString (trim yytext);continue());

<STRING> "\\^"(.?) => (addToCurString (handleCtrl yytext yypos);continue());
			   
<STRING> "\\"{digit}{3} => (addToCurString (digitEsc yypos yytext);continue());

<STRING> "\\"{digit}{1,2} => (ErrorMsg.error yypos (yytext ^ " is an illformed ascii decimal escape code. ascii decimal escape code must be of the form \\ddd, with 000 <= ddd <=" ^ (Int.toString maxAsciiCode) ^ " , and d a digit between 0 and 9"); continue());

<STRING> {letter}|{digits}|{printable}|{printable2} => (addToCurString yytext; continue());

<STRING> "\n" => (ErrorMsg.error yypos "Only use enter when inside \\f....f\\";
 lineNum := !lineNum+1;
 linePos := yypos :: !linePos;
		  continue());

<STRING> "\\"(" "|"\t"|"\f") =>(inMultiline := true; YYBEGIN MULTILINE; continue());
<STRING> "\\\n" => (handleNewline( yypos ); inMultiline := true; YYBEGIN MULTILINE; continue());

<STRING>.=> (ErrorMsg.error yypos ("Illegal character in string: " ^ yytext); continue());

<MULTILINE>"\n"	                   => (handleNewline(yypos);continue());
<MULTILINE> " "|"\t"|"\f" => (continue());
<MULTILINE> "\\" => (inMultiline := false; YYBEGIN STRING; continue());
<MULTILINE> . => (ErrorMsg.error yypos "Please only use tab, newline and space inside the \\...\\ block of a multiline string."; continue());
