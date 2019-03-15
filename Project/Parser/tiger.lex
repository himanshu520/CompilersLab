(* This file contains the ML-Lex specifications of lexer for the Tiger Language *)

val lineNum = ErrorMsg.lineNum;
val linePos = ErrorMsg.linePos;
val inString = ref false;
val commentCnt = ref 0;
val str = ref "";
val stringPos = ref 0;

type pos = int;
type lexresult = Tokens.token;

fun eof () = let val (errorPos :: _) = !linePos
             in
                ( if !inString then ErrorMsg.error errorPos "Unclosed string"
                  else if !commentCnt <> 0 then ErrorMsg.error errorPos "Unclosed Comment" else ();
                  Tokens.EOF (0, 0) )
             end;

%%

alpha = [a-zA-Z];
digits = [0-9];
ws = [\t\ ];
%s INITIAL COMMENT STRING MULTILINE_STRING ESCAPE;


%%

<INITIAL,COMMENT> \n                        => ( lineNum := !lineNum + 1; linePos := yypos :: (!linePos); continue () );
<INITIAL> {ws}+                             => ( continue () );
<INITIAL,COMMENT> "/*"                      => ( YYBEGIN COMMENT; commentCnt := !commentCnt + 1; continue () );
<COMMENT> "*/"                              => ( commentCnt := !commentCnt - 1; if (!commentCnt) = 0 then YYBEGIN INITIAL else (); continue () );
<COMMENT> .                                 => ( continue () );
<INITIAL> array                             => ( Tokens.ARRAY (yypos, yypos + 5) );
<INITIAL> break                             => ( Tokens.BREAK (yypos, yypos + 5) );
<INITIAL> if                                => ( Tokens.IF (yypos, yypos + 2) );
<INITIAL> then                              => ( Tokens.THEN (yypos, yypos + 4) );
<INITIAL> else                              => ( Tokens.ELSE (yypos, yypos + 4) );
<INITIAL> do                                => ( Tokens.DO (yypos, yypos + 2) );
<INITIAL> while                             => ( Tokens.WHILE (yypos, yypos + 5) );
<INITIAL> for                               => ( Tokens.FOR (yypos, yypos + 3) );
<INITIAL> function                          => ( Tokens.FUNCTION (yypos, yypos + 8) );
<INITIAL> let                               => ( Tokens.LET (yypos, yypos + 3) );
<INITIAL> in                                => ( Tokens.IN (yypos, yypos + 2) );
<INITIAL> end                               => ( Tokens.END (yypos, yypos + 3) );
<INITIAL> nil                               => ( Tokens.NIL (yypos, yypos + 3) );
<INITIAL> of                                => ( Tokens.OF (yypos, yypos + 2) );
<INITIAL> to                                => ( Tokens.TO (yypos, yypos + 2) );
<INITIAL> type                              => ( Tokens.TYPE (yypos, yypos + 4) );
<INITIAL> "("                               => ( Tokens.LPAREN (yypos, yypos + 1) );
<INITIAL> ")"                               => ( Tokens.RPAREN (yypos, yypos + 1) );
<INITIAL> "{"                               => ( Tokens.LBRACES (yypos, yypos + 1) );
<INITIAL> "}"                               => ( Tokens.RBRACES (yypos, yypos + 1) );
<INITIAL> "["                               => ( Tokens.LBRACKETS (yypos, yypos + 1) );
<INITIAL> "]"                               => ( Tokens.RBRACKETS (yypos, yypos + 1) );
<INITIAL> "+"                               => ( Tokens.PLUS (yypos, yypos + 1) );
<INITIAL> "-"                               => ( Tokens.MINUS (yypos, yypos + 1) );
<INITIAL> "*"                               => ( Tokens.MULTIPLY (yypos, yypos + 1) );
<INITIAL> "/"                               => ( Tokens.DIVIDE (yypos, yypos + 1) );
<INITIAL> "="                               => ( Tokens.EQUALS (yypos, yypos + 1) );
<INITIAL> "<>"                              => ( Tokens.NOTEQUAL (yypos, yypos + 2) );
<INITIAL> "<"                               => ( Tokens.LESS (yypos, yypos + 1) );
<INITIAL> "<="                              => ( Tokens.LESSEQUAL (yypos, yypos + 2) );
<INITIAL> ">"                               => ( Tokens.GREATER (yypos, yypos + 1) );
<INITIAL> ">="                              => ( Tokens.GREATEREQUAL (yypos, yypos + 2) );
<INITIAL> ":="                              => ( Tokens.ASSIGN (yypos, yypos + 2) );
<INITIAL> "."                               => ( Tokens.DOT (yypos, yypos + 1) );
<INITIAL> ","                               => ( Tokens.COMMA (yypos, yypos + 1) );
<INITIAL> ":"                               => ( Tokens.COLON (yypos, yypos + 1) );
<INITIAL> ";"                               => ( Tokens.SEMICOLON (yypos, yypos + 1) );
<INITIAL> "&"                               => ( Tokens.AND (yypos, yypos + 1) );
<INITIAL> "|"                               => ( Tokens.OR (yypos, yypos + 1) );
<INITIAL> {digits}+                         => ( Tokens.INT (valOf (Int.fromString yytext), yypos, yypos + size yytext) );
<INITIAL> {alpha}[a-zA-Z0-9_]*              => ( Tokens.ID (yytext, yypos, yypos + size yytext) );
<INITIAL> \"                                => ( YYBEGIN STRING; inString := true; stringPos := yypos; str := ""; continue () );
<STRING> \"                                 => ( YYBEGIN INITIAL; inString := false; Tokens.STRING (!str, !stringPos, yypos + 1) );
<STRING> \\{ws}*\n                          => ( YYBEGIN MULTILINE_STRING; linePos := yypos :: (!linePos); lineNum := !lineNum + 1; continue () );
<MULTILINE_STRING> {ws}*\n                  => ( linePos := yypos :: (!linePos); lineNum := !lineNum + 1; continue () );
<MULTILINE_STRING> {ws}*\\                  => ( YYBEGIN STRING; continue () );
<MULTILINE_STRING> .                        => ( YYBEGIN STRING; ErrorMsg.error yypos ("Illegal character '" ^ yytext ^ "' found in multiline string"); continue () );
<STRING> \\                                 => ( YYBEGIN ESCAPE; continue () );
<ESCAPE> [nrtf\\\"]                         => ( YYBEGIN STRING; str := (!str) ^ "\\" ^ yytext; continue () );
<ESCAPE> {digits}{digits}{digits}           => ( YYBEGIN STRING; str := (!str) ^ String.str (Char.chr (valOf (Int.fromString yytext))); continue () );
<ESCAPE> .                                  => ( YYBEGIN STRING; ErrorMsg.error yypos ("Illegal character '" ^ yytext ^ "' found inside escape sequence"); continue () );
<STRING> .                                  => ( str := (!str) ^ yytext; continue () );
.                                           => ( ErrorMsg.error yypos ("Illegal character '" ^ yytext ^ "' found"); continue () );