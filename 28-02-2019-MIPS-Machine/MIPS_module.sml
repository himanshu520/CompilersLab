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
    type L = Temp.label;
end;