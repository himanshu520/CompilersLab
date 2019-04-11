structure Main = struct

    structure Tr = Translate
    structure Frame = MipsFrame
    structure F = Frame
    structure R = RegAlloc
    
    exception Error = ErrorMsg.Error

    fun getsome (SOME x) = x
    |   getsome _ = raise Error

    fun emitproc out (F.PROC { body, frame }) =
        let val _ = print ("emit " ^ Symbol.name (Frame.name frame) ^ "\n")
            (* val _ = PrintTree.printTree (out,body) *)
            val stms = Canon.linearize body
            (* val _ = app (fn s => PrintTree.printTree (out, s)) stms *)
            val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
            val instrs = List.concat (map (Codegen.codegen frame) stms')
            val instrs' : Assem.instr list = F.procEntryExit2(frame, instrs)

            (* val (flowGraph, nodeList) = MakeGraph.instrs2graph(instrs)
            val (iGraph, liveFunc) = Liveness.interferenceGraph (flowGraph)
            val (regTable, spillList) = Color.color({interference=iGraph, initial=F.tempMap, 
            node2live = liveFunc, spillCost=(fn n=> 1 ), registers=F.registers}) *)

            val format0 = Assem.format F.getTempName
            val format1 = Assem.format Temp.makestring
            val (inst, allc) = RegAlloc.alloc(instrs, frame)
            val {prolog, body, epilog} = F.procEntryExit3 (frame, instrs')
            fun makeAssem st = Assem.OPER { assem = st, dst = [], src = [], jump = NONE}
            val inst' = [makeAssem prolog] @ body @ [makeAssem epilog]
            val _ = F.setTempMap allc

            val tmp = Temp.Table.listItemsi (allc)
            fun prn (x, y) = TextIO.print ((Temp.makestring x) ^ ": " ^ y ^ "\n")
            val _ = map prn tmp
        in  app (fn i => TextIO.output (out, (format0 i) ^ "\n")) inst';
              app (fn i => TextIO.print ( (format1 i) ^ "\n")) inst' end
    |   emitproc out (F.STRING (lab, s)) = TextIO.output (out, F.string (lab, s))

    fun withOpenFile fname f = 
        let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e) end 

    fun compile filename = 
        let val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in withOpenFile (filename ^ ".s") (fn out => app (emitproc out) frags) end

end