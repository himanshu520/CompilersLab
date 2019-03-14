(* This file defines the Tokens structure used inside tiger.lex file. The function definitions given here are useful for debugging the lexer *)

structure Tokens = struct 
    type token = string;
    fun ARRAY (x, y)        = "array -> " ^ Int.toString x;
    fun BREAK (x, y)        = "break -> " ^ Int.toString x;
    fun IF (x, y)           = "if -> " ^ Int.toString x;
    fun THEN (x, y)         = "then -> " ^ Int.toString x;
    fun ELSE (x, y)         = "else -> " ^ Int.toString x;
    fun DO (x, y)           = "do -> " ^ Int.toString x;
    fun WHILE (x, y)        = "while -> " ^ Int.toString x;
    fun FOR (x, y)          = "for -> " ^ Int.toString x;
    fun FUNCTION (x, y)     = "function -> " ^ Int.toString x;
    fun LET (x, y)          = "let -> " ^ Int.toString x;
    fun IN (x, y)           = "in -> " ^ Int.toString x;
    fun END (x, y)          = "end -> " ^ Int.toString x;
    fun NIL (x, y)          = "nil -> " ^ Int.toString x;
    fun OF (x, y)           = "of -> " ^ Int.toString x;
    fun TO (x, y)           = "to -> " ^ Int.toString x;
    fun TYPE (x, y)         = "type -> " ^ Int.toString x;
    fun LPAREN (x, y)       = "( -> " ^ Int.toString x;
    fun RPAREN (x, y)       = ") -> " ^ Int.toString x;
    fun LBRACES (x, y)      = "{ -> " ^ Int.toString x;
    fun RBRACES (x, y)      = "} -> " ^ Int.toString x;
    fun LBRACKETS (x, y)    = "[ -> " ^ Int.toString x;
    fun RBRACKETS (x, y)    = "] -> " ^ Int.toString x;
    fun PLUS (x, y)         = "+ -> " ^ Int.toString x;
    fun MINUS (x, y)        = "- -> " ^ Int.toString x;
    fun MULTIPLY (x, y)     = "* -> " ^ Int.toString x;
    fun DIVIDE (x, y)       = "/ -> " ^ Int.toString x;
    fun EQUALS (x, y)       = "= -> " ^ Int.toString x;
    fun NOTEQUAL (x, y)     = "<> -> " ^ Int.toString x;
    fun LESS (x, y)         = "< -> " ^ Int.toString x;
    fun LESSEQUAL (x, y)    = "<= -> " ^ Int.toString x;
    fun GREATER (x, y)      = "> -> " ^ Int.toString x;
    fun GREATEREQUAL (x, y) = ">= -> " ^ Int.toString x;
    fun ASSIGN (x, y)       = ":= -> " ^ Int.toString x;
    fun DOT (x, y)          = ". -> " ^ Int.toString x;
    fun COMMA (x, y)        = ", -> " ^ Int.toString x;
    fun COLON (x, y)        = ": -> " ^ Int.toString x;
    fun SEMICOLON (x, y)    = "; -> " ^ Int.toString x;
    fun AND (x, y)          = "& -> " ^ Int.toString x;
    fun OR (x, y)           = "| -> " ^ Int.toString x;
    fun INT (x, y, z)       = Int.toString x ^ " -> " ^ Int.toString y;
    fun ID (x, y, z)        = x ^ " -> " ^ Int.toString y;
    fun STRING (x, y, z)    = x ^ " -> " ^ Int.toString y;
    fun EOF (x, y)          = "EOF";
end;

