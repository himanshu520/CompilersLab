(* Driver to run the lexer on a given input file *)

structure Parse = struct 
    fun parse filename =
        let val file = TextIO.openIn filename
            val lexer = Mlex.makeLexer (fn _ => TextIO.input file)
            fun run () =
                let val ret = lexer()
                in ( TextIO.print (ret ^ "\n");
                     if ret = "EOF" then () else run () )
                end
        in ( run(); TextIO.closeIn file) end
end
