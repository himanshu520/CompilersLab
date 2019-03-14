(* This file defines the Tokens structure used inside tiger.lex file. The function definitions given here are useful for debugging the lexer *)

structure Tokens = struct 
    type lexresult = string;
    fun ARRAY (x, y) = TextIO.print ("array -> " ^ x);
    fun BREAK (x, y) = TextIO.print ("break -> " ^ x);
    fun IF (x, y) = TextIO.print ("if -> " ^ x);
    fun THEN (x, y) = TextIO.print ("then -> " ^ x);
    fun ELSE (x, y) = TextIO.print ("else -> " ^ x);
    fun DO (x, y) = TextIO.print ("do -> " ^ x);
    fun WHILE (x, y) = TextIO.print ("while -> " ^ x);
    fun FOR (x, y) = TextIO.print ("for -> " ^ x);
    fun FUNCTION (x, y) = TextIO.print ("function -> " ^ x);
    fun LET (x, y) = TextIO.print ("let -> " ^ x);
    fun IN (x, y) = TextIO.print ("in -> " ^ x);
    fun END (x, y) = TextIO.print ("end -> " ^ x);
    fun NIL (x, y) = TextIO.print ("nil -> " ^ x);
    fun OF (x, y) = TextIO.print ("of -> " ^ x);
    fun TO (x, y) = TextIO.print ("to -> " ^ x);
    fun TYPE (x, y) = TextIO.print ("type -> " ^ x);
    fun LPAREN (x, y) = TextIO.print ("( -> " ^ x);
    fun RPAREN (x, y) = TextIO.print (") -> " ^ x);
    fun LBRACES (x, y) = TextIO.print ("{ -> " ^ x);
    fun RBRACES (x, y) = TextIO.print ("} -> " ^ x);
    fun LBRACKETS (x, y) = TextIO.print ("[ -> " ^ x);
    fun RBRACKETS (x, y) = TextIO.print ("] -> " ^ x);
    fun PLUS (x, y) = TextIO.print ("+ -> " ^ x);
    fun MINUS (x, y) = TextIO.print ("- -> " ^ x);
    fun MULTIPLY (x, y) = TextIO.print ("* -> " ^ x);
    fun DIVIDE (x, y) = TextIO.print ("/ -> " ^ x);
    fun EQUALS (x, y) = TextIO.print ("= -> " ^ x);
    fun NOTEQUAL (x, y) = TextIO.print ("<> -> " ^ x);
    fun LESS (x, y) = TextIO.print ("< -> " ^ x);
    fun LESSEQUAL (x, y) = TextIO.print ("<= -> " ^ x);
    fun GREATER (x, y) = TextIO.print ("> -> " ^ x);
    fun GREATEREQUAL (x, y) = TextIO.print (">= -> " ^ x);
    fun ASSIGN (x, y) = TextIO.print (":= -> " ^ x);
    fun DOT (x, y) = TextIO.print (". -> " ^ x);
    fun COMMA (x, y) = TextIO.print (", -> " ^ x);
    fun COLON (x, y) = TextIO.print (": -> " ^ x);
    fun SEMICOLON (x, y) = TextIO.print ("; -> " ^ x);
    fun AND (x, y) = TextIO.print ("& -> " ^ x);
    fun OR (x, y) = TextIO.print ("| -> " ^ x);
    fun INT (x, y, z) = TextIO.print (Int.toString x ^ " -> " ^ y);
    fun ID (x, y, z) = TextIO.print (x ^ " -> " ^ y);
    fun STRING (x, y, z) = TextIO.print (x ^ " -> " ^ y);
end;

