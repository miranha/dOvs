structure Frontend : sig val parse : string -> Ast.cmd end =
struct
    structure CmdLrVals = CmdLrValsFun(structure Token = LrParser.Token)
    structure Lex = CmdLexFun(structure Tokens = CmdLrVals.Tokens)
    structure CmdP = Join (structure ParserData = CmdLrVals.ParserData
                            structure Lex = Lex
                            structure LrParser = LrParser)
    fun parse filename =
        let val file   = TextIO.openIn filename
            fun get  _ = TextIO.input file
            fun parseerror (s, p1, p2) = 
                let 
                    exception LocalParseError of int * string
                in 
                    print (Int.toString p1);
                    print ":";
                    print s;
                    print "\n";
                    raise LocalParseError (p1, s)
                end
            val lexer      = LrParser.Stream.streamify (Lex.makeLexer get)
            val (absyn, _) = CmdP.parse (30, lexer, parseerror,())
         in TextIO.closeIn file;
            absyn
        end
end
