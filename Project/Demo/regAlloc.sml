signature REGALLOC = sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REGALLOC = struct
    structure Frame = MipsFrame
    structure T = Tree
    structure G = Graph
    structure S = RedBlackSetFn ( struct
                                      type ord_key = Temp.temp
                                      fun compare (t1, t2) = String.compare (Temp.makestring t1, Temp.makestring t2)
                                  end )

    type allocation = Frame.register Temp.Table.table
    exception Error = ErrorMsg.Error

    fun alloc (instrLst, frame) =
        let val program = ref instrLst
            val allocation = Frame.tempMap
            fun rewriteProgram spills = 
                let val newtemps = ref S.empty
                    fun procSpill tmp =
                        let val acc = Frame.allocLocal frame true;
                            val addr = case Frame.exp acc (T.TEMP (MipsFrame.FP)) of T.MEM a => a
                                                                                    | _ => raise Error
                            fun contains (lst, tp) = isSome (List.find (fn tmp' => tmp' = tp) lst)
                            fun subTemp (lst, a, b) = foldl (fn (tp, acc) => if tp = a then b::acc else tp::acc) [] lst
                            fun foldfun (instr, acc) = 
                                let val newtemp = Temp.newtemp ()
                                    fun sandwich (src, dst, instr) =
                                        let val pre = if contains (src, tmp)
                                                      then ( newtemps := S.add (!newtemps, newtemp);
                                                             Codegen.codegen frame (T.MOVE (T.TEMP newtemp, T.MEM addr)) )
                                                      else []
                                            val post = if contains (dst, tmp)
                                                       then ( newtemps := S.add (!newtemps, newtemp);
                                                              Codegen.codegen frame (T.MOVE (T.MEM addr, T.TEMP newtemp)) )
                                                       else []
                                        in (pre @ [instr] @ post) end
                                    fun appnd lst = lst @ acc
                                in case instr of Assem.OPER { assem, dst, src , jump } => appnd (sandwich (src, dst, Assem.OPER { assem = assem, src = subTemp (src, tmp, newtemp), 
                                                                                                                                  dst = subTemp (dst, tmp, newtemp), jump = jump }))
                                               | Assem.MOVE { assem, dst, src } => if dst = tmp andalso src = tmp then acc
                                                                                   else if dst = tmp then appnd (instr :: (Codegen.codegen frame (T.MOVE (T.MEM addr, T.TEMP newtemp))))
                                                                                   else if src = tmp then appnd ((Codegen.codegen frame (T.MOVE (T.TEMP newtemp, T.MEM addr))) @ [instr])
                                                                                   else instr :: acc
                                               | _ => instr :: acc
                                end
                        in program := foldl foldfun [] (!program) end
                in ( app procSpill spills; (!program) ) end

            fun mainLoop () = 
                let val (igraph, liveFunc) = Liveness.interferenceGraph (#1 (MakeGraph.instrs2graph (!program)))
                    (* val _ = Liveness.show (TextIO.stdOut, igraph) *)
                    val (allocation, spills) = Color.color { interference = igraph, initial = allocation, spillCost = fn (node) => 1,
                                                             node2live = liveFunc, registers = Frame.registers }
                in if not (List.null spills)
                   then ( program := rewriteProgram spills; Color.cleanSpills (); mainLoop ())
                   else (!program, allocation)
                end
        in mainLoop () end
end