structure Tokens = struct
    datatype token =    EOF of int * int |
                        IF of int * int |
                        ID of string * int * int |
                        NUM of int * int * int |
                        REAL of real * int * int;
end

type lexresult = Tokens.token
fun eof() = Tokens.EOF(0, 0)
%%

digits = [0-9]+;
%%

if              =>  (Tokens.IF(yypos, yypos + 2));
[a-z][a-z0-9]   =>  (Tokens.ID(yytext, yypos, yypos + size yytext));
{digits}        =>  (Tokens.NUM(valOf(Int.fromString yytext), yypos, yypos + size yytext));
({digits}"."[0-9]*) | ([0-9]*"."{digits}) =>  (Tokens.REAL(valOf(Real.fromString yytext), yypos, yypos + size yytext));
("--"[a-z]*"\n") | (" "|"\n"|"\t")+        =>  (continue());
.               =>  (print ("illegal character"); continue());
