structure Main = struct

    structure Tr = Translate
    structure Frame = MipsFrame
    structure F = Frame
    structure R = RegAlloc
    
    exception Error = ErrorMsg.Error

    fun getsome (SOME x) = x
    |   getsome _ = raise Error

    fun emitproc out (F.PROC { body, frame }) =
        let val _ = print ("__________emit " ^ Symbol.name (Frame.name frame) ^ "__________\n")
            (* val _ = PrintTree.printTree (out,body) *)
            val stms = Canon.linearize body
            (* val _ = app (fn s => PrintTree.printTree (out, s)) stms *)
            val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
            val instrs = List.concat (map (Codegen.codegen frame) stms')
            val format1 = Assem.format Temp.makestring

            val instrs' : Assem.instr list = F.procEntryExit2(frame, instrs)
            val format0 = Assem.format F.getTempName
            val (inst, allc) = RegAlloc.alloc(instrs', frame)
            val {prolog, body, epilog} = F.procEntryExit3 (frame, inst)
            fun makeAssem st = Assem.OPER { assem = st, dst = [], src = [], jump = NONE}
            val inst' = [makeAssem prolog] @ body @ [makeAssem epilog]
            val _ = F.setTempMap allc

            val tmp = Temp.Table.listItemsi (allc)
            fun prn (x, y) = TextIO.print ((Temp.makestring x) ^ ": " ^ y ^ "\n")
            val _ = map prn tmp
        in  app (fn i => TextIO.output (out, (format0 i) ^ "\n")) inst;
            app (fn i => TextIO.print ( (format1 i) ^ "\n")) inst end
        (* in  app (fn i => TextIO.print ( (format1 i) ^ "\n")) instrs end
    |   emitproc out (F.STRING (lab, s)) = (TextIO.output (out, F.string (lab, s)); TextIO.print (F.string (lab, s))) *)
    |   emitproc out (F.STRING (lab, s)) = TextIO.output (out, F.string (lab, s))

    fun withOpenFile fname f = 
        let val out = TextIO.openOut fname
        in ((f out) handle e => (TextIO.closeOut out; raise e); TextIO.closeOut out) end 

    fun compile filename = 
        let val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
            val _ = TextIO.print ("SIZE OF FRAG IS " ^ Int.toString (List.length frags) ^ "\n")
        in withOpenFile (filename ^ ".s") (fn out => app (emitproc out) frags) end

end