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
    datatype exp = ADD of Temp.temp * Temp.temp * Temp.temp
                 | ADDI of Temp.temp * Temp.temp * int
                 | ADDU of Temp.temp * Temp.temp * Temp.temp
                 | ADDIU of Temp.temp * Temp.temp * int
                 | SUB of Temp.temp * Temp.temp * Temp.temp
                 | SUBU of Temp.temp * Temp.temp * Temp.temp
                 | MUL of Temp.temp * Temp.temp * Temp.temp
                 | MULT of Temp.temp * Temp.temp
                 | DIV of Temp.temp * Temp.temp
                 | AND of Temp.temp * Temp.temp * Temp.temp
                 | ANDI of Temp.temp * Temp.temp * int
                 | OR of Temp.temp * Temp.temp * Temp.temp
                 | ORI of Temp.temp * Temp.temp * int
                 | SLL of Temp.temp * Temp.temp * int
                 | SRL of Temp.temp * Temp.temp * int
                 | LW of Temp.temp * int * Temp.temp 
                 | LI of Temp.temp * int
                 | LA of Temp.temp * Temp.label
                 | LUI of Temp.temp * int
                 | SW of Temp.temp * int * Temp.temp 
                 | MFHI of Temp.temp 
                 | MFLO of Temp.temp 
                 | MOVE of Temp.temp * Temp.temp
                 | BEQ of Temp.temp * Temp.temp * Temp.label
                 | BNE of Temp.temp * Temp.temp * Temp.label
                 | BGT of Temp.temp * Temp.temp * Temp.label
                 | BGE of Temp.temp * Temp.temp * Temp.label
                 | BLT of Temp.temp * Temp.temp * Temp.label
                 | BLE of Temp.temp * Temp.temp * Temp.label
                 | SLT of Temp.temp * Temp.temp * Temp.temp
                 | SLTI of Temp.temp * Temp.temp * int
                 | J of Temp.label
                 | JR of Temp.temp
                 | JAL of Temp.label
                 | SYSCALL;
    
    fun prettyPrintExp (ADD (x, y, z)) = TextIO.print ("add " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (ADDI (x, y, z)) = TextIO.print ("addi " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (ADDU (x, y, z)) = TextIO.print ("addu " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (ADDIU (x, y, z)) = TextIO.print ("addiu " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SUB (x, y, z)) = TextIO.print ("sub " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (SUBU (x, y, z)) = TextIO.print ("subu " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (MUL (x, y, z)) = TextIO.print ("mul " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (MULT (x, y)) = TextIO.print ("mult " ^ Atom.toString x ^ " " ^ Atom.toString y ^ "\n")
    |   prettyPrintExp (DIV (x, y)) = TextIO.print ("div " ^ Atom.toString x ^ " " ^ Atom.toString y ^ "\n")
    |   prettyPrintExp (AND (x, y, z)) = TextIO.print ("and " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (ANDI (x, y, z)) = TextIO.print ("andi " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (OR (x, y, z)) = TextIO.print ("or " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (ORI (x, y, z)) = TextIO.print ("ori " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^"\n")
    |   prettyPrintExp (SLL (x, y, z)) = TextIO.print ("sll " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (SRL (x, y, z)) = TextIO.print ("srl " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (LW (x, y, z)) = TextIO.print ("lw " ^ Atom.toString x ^ " " ^ Int.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (LI (x, y)) = TextIO.print ("li " ^ Atom.toString x ^ " " ^ Int.toString y ^ "\n")
    |   prettyPrintExp (LA (x, y)) = TextIO.print ("la " ^ Atom.toString x ^ " " ^ Atom.toString y ^ "\n")
    |   prettyPrintExp (LUI (x, y)) = TextIO.print ("lui " ^ Atom.toString x ^ " " ^ Int.toString y ^ "\n")
    |   prettyPrintExp (SW (x, y, z)) = TextIO.print ("sw " ^ Atom.toString x ^ " " ^ Int.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (MFHI x) = TextIO.print ("mhfi " ^ Atom.toString x ^ "\n")
    |   prettyPrintExp (MFLO x) = TextIO.print ("mflo " ^ Atom.toString x ^ "\n")
    |   prettyPrintExp (MOVE (x, y)) = TextIO.print ("move " ^ Atom.toString x ^ " " ^ Atom.toString y ^ "\n")
    |   prettyPrintExp (BEQ (x, y, z)) = TextIO.print ("beq " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (BNE (x, y, z)) = TextIO.print ("bnq " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (BGT (x, y, z)) = TextIO.print ("bgt " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (BGE (x, y, z)) = TextIO.print ("bge " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (BLT (x, y, z)) = TextIO.print ("blt " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (BLE (x, y, z)) = TextIO.print ("ble " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (SLT (x, y, z)) = TextIO.print ("slt " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Atom.toString z ^ "\n")
    |   prettyPrintExp (SLTI (x, y, z)) = TextIO.print ("slti " ^ Atom.toString x ^ " " ^ Atom.toString y ^ " " ^ Int.toString z ^ "\n")
    |   prettyPrintExp (J x) = TextIO.print ("j " ^ Atom.toString x ^ "\n")
    |   prettyPrintExp (JR x) = TextIO.print ("jr " ^ Atom.toString x ^ "\n")
    |   prettyPrintExp (JAL x) = TextIO.print ("jal " ^ Atom.toString x ^ "\n")
    |   prettyPrintExp _ = TextIO.print "syscall\n";

end;