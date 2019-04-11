(* This file contains the driver code to run the parser on a given file containing tiger source code and return its AST *)

signature PARSE = sig 
                      val parse : string -> Absyn.exp 
                  end

structure Parse : PARSE = struct 
    structure TigerLrVals = TigerLrValsFun ( structure Token = LrParser.Token )
    structure TigerLexer  = TigerLexFun ( structure Tokens = TigerLrVals.Tokens )
    structure TigerParser = Join ( structure ParserData = TigerLrVals.ParserData
                                   structure Lex = TigerLexer
                                   structure LrParser = LrParser )
    
    fun parse filename =
        let 
            val _ = ( ErrorMsg.reset(); ErrorMsg.fileName := filename )
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            fun parseerror(s, p1, p2) = ErrorMsg.error p1 s
            val lexer = LrParser.Stream.streamify (TigerLexer.makeLexer get)
            val (absyn, _) = TigerParser.parse(30, lexer, parseerror, ())
        in ( TextIO.closeIn file; absyn ) end 
      
    handle LrParser.ParseError => raise ErrorMsg.Error
end