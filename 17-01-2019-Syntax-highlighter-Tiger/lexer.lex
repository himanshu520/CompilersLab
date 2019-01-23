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

val prevLineNum = ref 0;
val lineNum = ref 1;
val prevCharNum = ref 0;
val charNum = ref 1;
val commentCnt = ref 0;

type lexresult = Tokens.token;
fun eof () = if !commentCnt > 0 then (print ("improper comments"); Tokens.EOF (0, 0)) else Tokens.EOF(0, 0);
%%

alpha = [a-zA-z];
digits = [0-9];
ws = [\t\ ];
%s COMMENT;
%%

<INITIAL> \n                                =>    ( prevLineNum := !lineNum; 
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.WHITESPACE (yytext, !prevLineNum, !prevCharNum) );
<INITIAL> {ws}+                             =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.WHITESPACE (yytext, !lineNum, !prevCharNum) );
"(*"                                        =>    ( YYBEGIN COMMENT;
                                                    commentCnt := !commentCnt + 1;
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> "*)"                              =>    ( commentCnt := !commentCnt - 1;
                                                    if !commentCnt = 0 then (YYBEGIN INITIAL) else (YYBEGIN COMMENT);
                                                    prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> \n                                =>    ( prevLineNum := !lineNum; 
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    Tokens.COMMENT (yytext, !prevLineNum, !prevCharNum) );
<COMMENT> [*(]                              =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
<COMMENT> [^*\n(]*                           =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.COMMENT (yytext, !lineNum, !prevCharNum) );
.                                           =>    ( print ("illegal character"); continue() );  
