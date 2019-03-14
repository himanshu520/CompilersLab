(* This file contains ErrorMsg structure used for printing messages for any error that occurs during the compilation of a Tiger source file *)


signature ERRORMSG = sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    exception Error
    val impossible : string -> 'a
    val reset : unit -> unit
end

structure ErrorMsg : ERRORMSG = struct

    val anyErrors = ref false
    val fileName = ref ""
    val lineNum = ref 1
    val linePos = ref [1]
    val sourceStream = ref TextIO.stdIn

    fun reset () = ( anyErrors := false;
		             fileName := "";
		             lineNum := 1;
		             linePos := [1];
		             sourceStream := TextIO.stdIn )

    exception Error

    fun error pos (msg : string) =
        let fun look (a :: rest, n) = if a < pos then app TextIO.print [":", Int.toString n, ".", Int.toString (pos-a)]
                                      else look (rest, n - 1)
	        |   look _ = print "0.0"
        in ( anyErrors := true;
	         TextIO.print (!fileName);
	         look (!linePos, !lineNum);
	         app TextIO.print [":", msg, "\n"] )
        end

    fun impossible msg = ( app print ["Error: Compiler bug: ", msg, "\n"];
                           TextIO.flushOut TextIO.stdOut;
                           raise Error )

end
  