(* This file defines the signature of a general frame interface *)

signature FRAME = sig
    type frame
    type access
    datatype frag = PROC of { body : Tree.stm, frame : frame }
                  | STRING of Temp.label * string
    type register = string
    val registers : register list

    val newFrame : { name : Temp.label, formals : bool list } -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val string : Temp.label * string -> string 

    val FP : Temp.temp
    val SP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val callerSaves : Temp.temp list
    val calleeSaves : Temp.temp list
    val argRegs : Temp.temp list
    val specialRegs : Temp.temp list
    val wordSize : int
    val tempMap : register Temp.Table.table
    val myTempMap : register Temp.Table.table ref
    val getTempName : Temp.temp -> string
    val setTempMap: register Temp.Table.table -> unit
    val tempString : Temp.temp -> string

    val exp : access -> Tree.exp -> Tree.exp
    
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list -> { prolog : string, body : Assem.instr list, epilog : string }
    val externalCall : string * Tree.exp list -> Tree.exp
end