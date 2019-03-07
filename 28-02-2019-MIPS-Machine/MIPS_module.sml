signature TEMP = sig
    type temp;
    type label;
    val newTemp : unit -> temp;
    val newLabel : unit -> label;
    val newLabelName : string -> unit;
    val valTemp : temp -> int;
    val valLabel : label -> int;
    val assignTemp : temp -> int -> unit;
    val assignLabel : label -> int -> unit;
end;


structure Temp : TEMP = struct 
    type temp = Atom.atom;
    type label = Atom.atom;
    
    val tempCnt = ref 0;
    val labelCnt = ref 0;
    
    val temps : int ref AtomMap.map ref = ref AtomMap.empty;
    val labels : int ref AtomMap.map ref = ref AtomMap.empty;

    fun newTemp () = let val nTemp = Atom.atom (tempCnt := !tempCnt + 1; "temp_" ^ (Int.toString (!tempCnt)))
                     in (temps := AtomMap.insert (!temps, nTemp, ref 0); nTemp) end;
    
    fun newLabel () = let val nLabel = Atom.atom (labelCnt := !labelCnt + 1; "label_" ^ (Int.toString (!labelCnt)))
                      in (labels := AtomMap.insert (!labels, nLabel, ref 0); nLabel) end;
    
    fun newLabelName x = labels := AtomMap.insert (!labels, Atom.atom x, ref 0);
    
    fun valTemp x = !(AtomMap.lookup (!temps, x));
    fun valLabel x = !(AtomMap.lookup (!labels, x));
    
    fun assignTemp x y = (AtomMap.lookup (!temps, x) := y);
    fun assignLabel x y = (AtomMap.lookup (!labels, x) := y);
end;

structure MIPS_module = struct
    type T = Temp.temp;
    type TTT = T * T * T;
    type TTI = T * T * IntInf.int;
    type TT = T * T;
    type L = Temp.label;
    datatype exp = ADD of TTT
                 | ADDI of TTI
                 | ADDU of TTT
                 | ADDIU of TTI
                 | SUB of TTT
                 | SUBU of TTT
                 | MUL of TTT
                 | MULT of TT
                 | DIV of TT
                 | AND of TTT
                 | ANDI of TTI
                 | OR of TTT
                 | ORI of TTI
                 | SLL of TTI
                 | SRL of TTI
                 | LW of T * IntInf.int * T
                 | LI of T * IntInf.int
                 | LA of T * Temp.label
                 | LUI of T * IntInf.int
                 | SW of T * IntInf.int * T
                 | MFHI of T
                 | MFLO of T
                 | MOVE of TT

end;