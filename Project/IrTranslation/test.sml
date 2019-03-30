CM.make "sources.cm";
val absyntree = Parse.parse "a.tig";
val t = Semant.transProg absyntree;
val ty = #ty t;
(* PrintAbsyn.print (TextIO.stdOut, absyntree); *)