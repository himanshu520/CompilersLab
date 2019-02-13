(* User declarations *)

val prevLineNum = ref 0;
val lineNum = ref 1;
val prevCharNum = ref 0;
val charNum = ref 1;

type pos = int;
type svalue = Tokens.svalue;
type ('a, 'b) token = ('a, 'b) Tokens.token;
type lexresult = (svalue, pos) token;

fun eof fileName = Tokens.EOF (!lineNum, !charNum);

fun error_msg str = TextIO.print ("Illegal string " ^ str ^ " found at line number " ^ 
                                  (Int.toString (!prevLineNum)) ^ " at position " ^ Int.toString (!prevCharNum) ^
                                  "\n");


%%

%header (functor PrettyPrinterLexFun(structure Tokens : PrettyPrinter_TOKENS));
%arg (fileName : string);

alpha = [a-zA-Z];
digits = [0-9];
ws = [\t\ ];
%s COMMENT;


%%

<INITIAL> \n                                =>    ( prevLineNum := !lineNum; 
                                                    prevCharNum := !charNum;
                                                    lineNum := !lineNum + 1;
                                                    charNum := 1;
                                                    continue() );
<INITIAL> {ws}+                             =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    continue() );
<INITIAL> {alpha}+                          =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.ID (yytext, !lineNum, !charNum) );
<INITIAL> {digits}+                         =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.CONST (valOf (Int.fromString yytext), !lineNum, !charNum) );
<INITIAL> "*"                               =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.MUL (!lineNum, !charNum) );
<INITIAL> "+"                               =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.PLUS (!lineNum, !charNum) );
<INITIAL> "="                               =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.EQUALS (!lineNum, !charNum) );
.                                           =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    error_msg yytext;
                                                    continue () );
