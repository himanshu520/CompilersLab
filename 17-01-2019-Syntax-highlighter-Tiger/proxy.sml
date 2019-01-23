use "lexer.lex.sml";

val instream = TextIO.openIn "testfile.txt";
val lexer = Mlex.makeLexer (fn _ => TextIO.input instream);

fun printColoured (Mlex.UserDeclarations.Tokens.COMMENT (t, _, _))     = (TextIO.print ("\027[1;31m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.OPERATOR (t, _, _))    = (TextIO.print ("\027[1;32m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.KEYWORD (t, _, _))     = (TextIO.print ("\027[1;33m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.ID (t, _, _))          = (TextIO.print ("\027[1;34m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.INT (t, _, _))         = (TextIO.print ("\027[1;35m" ^ (Int.toString t) ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.STRING (t, _, _))      = (TextIO.print ("\027[1;36m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.ESCAPE (t, _, _))      = (TextIO.print ("\027[1;94m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.WHITESPACE (t, _, _))  = (TextIO.print ("\027[1;91m" ^ t ^ "\027[0m"); true) |
    printColoured (Mlex.UserDeclarations.Tokens.EOF (_, _))            = false;

fun printOutput () = let val retval : Mlex.UserDeclarations.Tokens.token = lexer()
                     in if (printColoured retval) then printOutput() else () end;

val _ = printOutput ();
