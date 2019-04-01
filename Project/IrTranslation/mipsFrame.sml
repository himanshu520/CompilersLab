(* This file defines the frame interface corresponding to the MIPS architecture *)

structure MipsFrame : FRAME = struct
    structure Te = Temp
    structure Tr = Tree

    datatype access = InFrame of int | InReg of Te.temp
    type frame = { name : Te.label, formals : access list, locals : int ref, shift : Tr.stm list }
    datatype frag = PROC of { body : Tr.stm, frame : frame } | STRING of Te.label * string
    exception ArgumentOverflow

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
    val callerSave = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9];

    (* callee save registers *)
    val S0 = Te.newtemp ()
    val S1 = Te.newtemp ()
    val S2 = Te.newtemp ()
    val S3 = Te.newtemp ()
    val S4 = Te.newtemp ()
    val S5 = Te.newtemp ()
    val S6 = Te.newtemp ()
    val S7 = Te.newtemp ()
    val calleeSave = [S0, S1, S2, S3, S4, S5, S6, S7];

    (* Other special registers *)
    val ZERO = Te.newtemp ()
    val SP = Te.newtemp ()
    val FP = Te.newtemp ()
    val RA = Te.newtemp ()
    val RV = V0

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

    fun procEntryExit1 (frame, body) = body
    fun string (label, str) = Symbol.name label ^ ": .asciiz \"" ^ str ^ "\"\n"
    fun externalCall (s, args) = Tr.CALL (Tr.NAME (Te.namedlabel s), args)
end