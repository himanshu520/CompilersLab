use "test_lexer.lex.sml";

val instream = TextIO.openIn "testfile.txt";
val lexer = Mlex.makeLexer (fn _ => TextIO.input instream);

fun printColoured (Mlex.UserDeclarations.Tokens.NUM (t, _, _))   = (TextIO.print ("\027[1;31m" ^ (Int.toString t)); true) |
    printColoured (Mlex.UserDeclarations.Tokens.REAL (t, _, _))  = (TextIO.print ("\027[1;32m" ^ (Real.toString t)); true) |
    printColoured (Mlex.UserDeclarations.Tokens.ID (t, _, _))    = (TextIO.print ("\027[1;33m" ^ t); true) |
    printColoured (Mlex.UserDeclarations.Tokens.IF (_, _))       = (TextIO.print ("\027[1;34m if"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.EOF (_, _))      = false;

fun printOutput () = let 
                        val retval : Mlex.UserDeclarations.Tokens.token = lexer()
                     in
                        if (printColoured retval) then printOutput() else () 
                     end;

val _ = printOutput ();
