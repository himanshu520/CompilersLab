(* This file defines the frame interface corresponding to the MIPS architecture *)

structure MipsFrame : FRAME = struct
    structure Te = Temp
    structure Tr = Tree
    structure A = Assem

    datatype access = InFrame of int | InReg of Te.temp
    type frame = { name : Te.label, formals : access list, locals : int ref, shift : Tr.stm list }
    datatype frag = PROC of { body : Tr.stm, frame : frame } | STRING of Te.label * string
    type register = string
    exception ArgumentOverflow
    exception Error = ErrorMsg.Error

    val wordSize = 4

    (* registers for function return values *)
    val V0 = Te.newtemp ()
    val V1 = Te.newtemp ()

    (* registers for passing arguments to functions *)
    val A0 = Te.newtemp ()
    val A1 = Te.newtemp ()
    val A2 = Te.newtemp ()
    val A3 = Te.newtemp ()
    val argRegs = [A0, A1, A2, A3];

    (* caller save registers *)
    val T0 = Te.newtemp ()
    val T1 = Te.newtemp ()
    val T2 = Te.newtemp ()
    val T3 = Te.newtemp ()
    val T4 = Te.newtemp ()
    val T5 = Te.newtemp ()
    val T6 = Te.newtemp ()
    val T7 = Te.newtemp ()
    val T8 = Te.newtemp ()
    val T9 = Te.newtemp ()
    val callerSaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9];

    (* callee save registers *)
    val S0 = Te.newtemp ()
    val S1 = Te.newtemp ()
    val S2 = Te.newtemp ()
    val S3 = Te.newtemp ()
    val S4 = Te.newtemp ()
    val S5 = Te.newtemp ()
    val S6 = Te.newtemp ()
    val S7 = Te.newtemp ()
    val calleeSaves = [S0, S1, S2, S3, S4, S5, S6, S7];

    (* Other special registers *)
    val ZERO = Te.newtemp ()
    val SP = Te.newtemp ()
    val FP = Te.newtemp ()
    val RA = Te.newtemp ()
    val RV = V0
    val specialRegs = [ ZERO, SP, FP, RA, RV ];

    val regList = [ (SP, "$sp"), (FP, "$fp"), (RA, "$ra"), (RV, "$v0"), 
                    (S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7"),
                    (T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"), (T8, "$t8"), (T9, "$t9"),
                    (A0, "$a0"), (A1, "$a1"), (A2, "$a2"), (A3, "$a3") ];

    
    val registers = let val regs = [ (S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7"),
                                     (T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"),
                                     (T8, "$t8"), (T9, "$t9") ]
                    in map (fn (x, y) => y) regs end
    val tempMap = foldl (fn ((x, y), tbl) => Te.Table.enter (tbl, x, y)) Te.Table.empty regList
    fun tempString t = ( case Te.Table.look (tempMap, t) of SOME t => t
                                                          | _ => Temp.makestring t )

    val myTempMap = ref tempMap
    
    (* used by Translate to turn Frame.access to Tree.exp, the second argument is the address of the stack frame where the access (first argument) lives in *)
    fun exp (InFrame t) = (fn tmp => Tr.MEM (Tr.BINOP (Tr.PLUS, tmp, Tr.CONST t)))
    |   exp (InReg t) = (fn _ => Tr.TEMP t)

    fun newFrame { name, formals } = let fun addFormal (x :: xs, offset) = if x then (InFrame offset) :: (addFormal (xs, offset + wordSize))
                                                                           else (InReg (Te.newtemp ())) :: (addFormal (xs, offset))
                                         |   addFormal _ = []
                                         val accesses = addFormal (formals, wordSize)
                                         fun viewShift (access, reg) = Tr.MOVE (exp access (Tr.TEMP FP), Tr.TEMP reg)
                                         val shift = ListPair.map viewShift (accesses, argRegs) 
                                     in if (List.length formals) <= (List.length argRegs) then { name = name, formals = accesses, locals = ref 0, shift = shift }
                                                                                          else raise ArgumentOverflow
                                     end

    fun name { name, formals, locals, shift } = name
    fun formals { name, formals, locals, shift } = formals
    fun allocLocal { name, formals, locals, shift } escape = if escape then ( locals := !locals + 1; InFrame (!locals * wordSize) )
                                                             else InReg (Te.newtemp ())

    fun string (label, str) = Symbol.name label ^ ": .asciiz \"" ^ str ^ "\"\n"
    fun externalCall (s, args) = Tr.CALL (Tr.NAME (Te.namedlabel s), args)

    fun procEntryExit1 (frame, body) = body
        (* let val { name, formals, locals, shift } = frame
            val calleeSaveLocals = map (fn cs => allocLocal frame true) calleeSaves
            fun moveCalleeSaves (temp::temps, loc::locals, genMove) = (genMove (temp, loc)) :: (moveCalleeSaves (temps, locals, genMove))
            |   moveCalleeSaves ([], [], _) = []
            |   moveCalleeSaves(_, _, _) = raise Error

            val localSaves = moveCalleeSaves (calleeSaves, calleeSaveLocals, (fn (cs, csl) => Tr.MOVE (exp csl (Tr.TEMP FP), Tr.TEMP cs)))
            val localRestores = moveCalleeSaves (calleeSaves, calleeSaveLocals, (fn (cs, csl) => Tr.MOVE (Tr.TEMP cs, exp csl (Tr.TEMP FP))))
            fun seq [x] = x
            |   seq (x::rest) = Tr.SEQ (x, seq(rest))
            |   seq [] = Tr.EXP (Tr.CONST 0)
        in seq (shift @ localSaves @ [body] @ localRestores) end *)

    fun procEntryExit2 (frame, instrs) = instrs @ [ Assem.OPER { assem = "", src = specialRegs @ calleeSaves,
                                                                 dst = [], jump = SOME [] } ]

    fun procEntryExit3 ({ name, formals, locals, shift }, instrs) =
        let val outSpace = (2 * List.length(argRegs) + (!locals)) * wordSize
            val prologue = String.concat ([ Symbol.name name, ":\n",
                                            "sw $fp, 0($sp)\n",
                                            "move $fp, $sp\n",
                                            "addiu $sp, $sp, ", Int.toString outSpace, "\n"])
            val epilogue = String.concat ([ "move $sp, $fp\n",
                                            "lw $fp, 0($sp)\n",
                                            "jr $ra\n" ])
      in { prolog = prologue, body = instrs, epilog = epilogue } end


    fun getTempName(temp) =
    let 
      val name = Te.Table.look(!myTempMap, temp)
    in
      case name of NONE => Te.makestring(temp)
                 | SOME regstr => regstr
    end
      fun setTempMap (newTempMap) =
    myTempMap := newTempMap
end