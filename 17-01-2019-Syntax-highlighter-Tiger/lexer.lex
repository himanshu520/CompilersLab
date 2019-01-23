structure Tokens = struct
    datatype token = COMMENT of string * int * int |
                     EOF of int * int |
                     OPERATOR of string * int * int |
                     KEYWORD of string * int * int |
                     ID of string * int * int |
                     INT of int * int * int |
                     STRING of string * int * int |
                     ESCAPE of string * int * int |
                     WHITESPACE of string * int * int;
end;

val inString = ref 0;
val prevLineNum = ref 0;
val lineNum = ref 1;
val prevCharNum = ref 0;
val charNum = ref 1;
val commentCnt = ref 0;

type lexresult = Tokens.token;
fun eof () = if !commentCnt > 0 then (print ("improper comments\n"); Tokens.EOF (0, 0)) else 
             if !inString > 0 then (print ("improper string\n"); Tokens.EOF(0, 0)) else
             (print ("\n"); Tokens.EOF(0, 0));
%%

alpha = [a-zA-z];
digits = [0-9];
ws = [\t\ ];
%s COMMENT;
%s STRING;
%s MULTILINE_STRING;
%%

<INITIAL> \n                                =>    ( prevLineNum := !lineNum; 
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.WHITESPACE (yytext, !prevLineNum, !prevCharNum) );
<INITIAL> {ws}+                             =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.WHITESPACE (yytext, !lineNum, !prevCharNum) );
"/*"                                        =>    ( YYBEGIN COMMENT;
                                                    commentCnt := !commentCnt + 1;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> "*/"                              =>    ( commentCnt := !commentCnt - 1;
                                                    if !commentCnt = 0 then (YYBEGIN INITIAL) else (YYBEGIN COMMENT);
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> \n                                =>    ( prevLineNum := !lineNum; 
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.COMMENT (yytext, !prevLineNum, !prevCharNum) );
<COMMENT> [*/]                              =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> [^*\n/]*                          =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<INITIAL> \"                                =>    ( YYBEGIN STRING;
                                                    inString := 1;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.STRING (yytext, !lineNum, !prevCharNum) );
<STRING> [^\\"\n]*                          =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.STRING (yytext, !lineNum, !prevCharNum) );
<STRING> \\ (n | t | \^{alpha} | {digits}{digits}{digits} | \" | \\)
                                            =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.ESCAPE (yytext, !lineNum, !prevCharNum) );
<STRING> \\{ws}*\n                          =>    ( YYBEGIN MULTILINE_STRING;
                                                    prevLineNum := !lineNum;
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.ESCAPE (yytext, !prevLineNum, !prevCharNum) );
<MULTILINE_STRING> {ws}*\n                  =>    ( prevLineNum := !lineNum;
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.ESCAPE (yytext, !prevLineNum, !prevCharNum) );
<MULTILINE_STRING> {ws}*\\                  =>    ( YYBEGIN STRING;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.ESCAPE (yytext, !lineNum, !prevCharNum) );
<STRING> \"                                 =>    ( YYBEGIN INITIAL;
                                                    inString := 0;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.STRING (yytext, !lineNum, !prevCharNum) );
<INITIAL> array | break | do | else | end | for | function | if | in | let | nil | of | then | to | type | var | while
                                            =>    ( YYBEGIN INITIAL;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.KEYWORD (yytext, !lineNum, !prevCharNum) );
<INITIAL> "<>" | "<=" | ">=" | ":=" | "(" | ")" | "{" | "}" | "[" | "]" | "-"
                                            =>    ( YYBEGIN INITIAL;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.OPERATOR (yytext, !lineNum, !prevCharNum) );  
<INITIAL> [:.,;*/+=<>&|]                    =>    ( YYBEGIN INITIAL;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.OPERATOR (yytext, !lineNum, !prevCharNum) );
<INITIAL> {digits}+                         =>    ( YYBEGIN INITIAL;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.INT (valOf (Int.fromString yytext), !lineNum, !prevCharNum) );
<INITIAL> {alpha}[a-zA-Z0-9_]*              =>    ( YYBEGIN INITIAL;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.ID (yytext, !lineNum, !prevCharNum) );
.                                           =>    ( print ("illegal character"); continue() );
