(* User declarations *)

val prevLineNum = ref 0;
val lineNum = ref 1;
val prevCharNum = ref 0;
val charNum = ref 1;

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue, pos) token

fun eof filename = Tokens.EOF (!lineNum, !charNum);
type pos = int * int;

fun error_msg str = TextIO.print ("Illegal string " ^ str ^ " found at line number " ^ 
                                  (Int.toString (!prevLineNum)) ^ " at position " ^ Int.toString (!prevCharNum) ^
                                  "\n");

%%
(* ML-Lex definitions *)

(* This header line is appended before the usual output of ml-lex on running this file.
   Thus, this line converts the usual structure returned by ml-lex on processing this file into a 
   functor that accepts a structure Tokens of signature specified below.
   This Token structure is the same that is returned by ml-yacc on processing the grammar file
   specified in program.grm *)
%header (functor ExprLexFun(structure Tokens : PrettyPrinter_TOKENS));

alpha = [a-zA-Z];
digits = [0-9];
ws = [\t\ ];
%s COMMENT;
%arg (filename : string)


%%
(* Rules *)

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
                                                    Tokens.CONST (yytext, !lineNum, !charNum) );
<INITIAL> *                                 =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.MUL (!lineNum, !charNum) );
<INITIAL> +                                 =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.PLUS (!lineNum, !charNum) );
<INITIAL> =                                 =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    Tokens.EQUALS (!lineNum, !charNum) );
.                                           =>    ( prevCharNum := !charNum;
                                                    charNum := !charNum + size yytext;
                                                    error_msg yytext;
                                                    continue () );
