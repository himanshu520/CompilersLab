(* This file contains function to convert a MIPS assembly instruction list into a flow graph *)

signature MAKEGRAPH = sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph : MAKEGRAPH = struct
    structure A = Assem
    exception Error = ErrorMsg.Error

    fun instrs2graph instrLst = 
        let val control = ref (Graph.newGraph ())
            val def = ref Graph.Table.empty
            val use = ref Graph.Table.empty
            val ismove = ref Graph.Table.empty
            val labelNodeMap = ref AtomMap.empty
            fun str x = Atom.atom (Symbol.name x)

            fun addNode (d, u, ism) =
                let val node = Graph.newNode (!control)
                    val _ = def := Graph.Table.enter (!def, node, d);
                    val _ = use := Graph.Table.enter (!use, node, u);
                    val _ = ismove := Graph.Table.enter (!ismove, node, ism);
                in node end

            fun addLabel (A.LABEL { assem, lab }) = 
                let val node = addNode ([], [], false)
                in labelNodeMap := AtomMap.insert (!labelNodeMap, str lab, node) end
            |   addLabel _ = ()

            fun addMoOp (A.OPER { assem, dst, src, jump }) = 
                let val node = addNode (dst, src, false)
                    fun addEdge label = 
                        let val nd = AtomMap.lookup (!labelNodeMap, str label)
                        in Graph.mk_edge { from = node, to = nd } end
                in  ( case jump of SOME labels => map addEdge labels
                                 | NONE => [()] );
                    node
                 end
            |   addMoOp (A.MOVE { assem, dst, src }) = addNode ([dst], [src], true)
            |   addMoOp (A.LABEL { assem, lab }) = AtomMap.lookup (!labelNodeMap, str lab)

            fun addEdges (x::y::ys) = ( Graph.mk_edge { from = x, to = y }; addEdges (y::ys) )
            |   addEdges _ = ()

        in  map addLabel instrLst;
            addEdges (map addMoOp instrLst);
            (Flow.FGRAPH { control = !control, def = !def, use = !use, ismove = !ismove }, Graph.nodes (!control))
        end
end