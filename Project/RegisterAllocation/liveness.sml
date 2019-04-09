(* This file performs liveness analysis on the flow graph and computes interference graph *)

signature LIVENESS = sig
    datatype igraph = IGRAPH of { graph : Graph.graph,
                                  tnode : Temp.temp -> Graph.node,
                                  gtemp : Graph.node -> Temp.temp,
                                  moves : (Graph.node * Graph.node) list }
    val interferenceGraph : Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
    val show : TextIO.outstream * igraph -> unit
end

structure Liveness : LIVENESS = struct
    structure G = Graph
    structure T = Temp
    structure F = Flow
    structure S = IntListSet

    datatype igraph = IGRAPH of { graph : Graph.graph,
                                  tnode : Temp.temp -> Graph.node,
                                  gtemp : Graph.node -> Temp.temp,
                                  moves : (Graph.node * Graph.node) list }
    type liveSet = S.set
    type liveMap = liveSet Flow.Graph.Table.table

    exception Error = ErrorMsg.Error

    fun interferenceGraph (F.FGRAPH { control, def, use, ismove }) =
        let val graph = G.newGraph ()
            val nodes = G.nodes control

            fun list (t, e) = case G.Table.look (t, e) of SOME el => el | NONE => []
            fun set (t, e) = case G.Table.look (t, e) of SOME el => el | NONE => S.empty

            val temps = let fun addTempList (node, temps) = (list (def, node)) @ (list (use, node)) @ temps
                        in foldl addTempList [] nodes end

            val (tempNodeMap, nodeTempMap) = 
                let fun addTemp (temp, (tnode, ntemp)) = 
                        ( case T.Table.look (tnode, temp) of SOME node => (tnode, ntemp)
                                                           | _ => let val node = Graph.newNode graph
                                                                      val tnode' = T.Table.enter (tnode, temp, node)
                                                                      val ntemp' = F.Graph.Table.enter (ntemp, node, temp)
                                                                  in (tnode', ntemp') end )
                in foldl addTemp (T.Table.empty, F.Graph.Table.empty) temps end

            fun tnode t = ( case T.Table.look (tempNodeMap, t) of SOME n => n | NONE => raise Error )
            fun gtemp n = ( case F.Graph.Table.look (nodeTempMap, n) of SOME t => t | NONE => raise Error )
            val moves = let fun addMoves (node, moves) = 
                                ( case G.Table.look (ismove, node) of SOME true => let val hdd = tnode (hd (list (def, node)))
                                                                                       val hdu = tnode (hd (list (use, node)))
                                                                                   in (hdd, hdu) :: moves end
                                                                    | _ => moves )
                        in foldl addMoves [] nodes end

            fun initGraph () = foldl (fn (node, table) => G.Table.enter (table, node, S.empty)) Graph.Table.empty nodes
            fun compLiveness (ins, outs) =
                let val repeat = ref false
                    fun procNode (node, (ins, outs)) =
                        let val in_ = set (ins, node)
                            val out = set (outs, node)
                            val use' = S.fromList (list (use, node))
                            val def' = S.fromList (list (def, node))
                            val in_' = S.union (use', S.difference (out, def'))
                            val out' = foldl (fn (n, out) => S.union (set (ins, n), out)) S.empty (G.succ node)
                        in  if S.equal (in_, in_') andalso S.equal (out, out') then (ins, outs)
                            else ( repeat := true;
                                   (G.Table.enter (ins, node, in_'), G.Table.enter(outs, node, out')) )
                        end
                    val (ins', outs') = foldl procNode (ins, outs) (List.rev nodes)
                in if !repeat then compLiveness (ins', outs') else (ins', outs') end
            val (ins, outs) = compLiveness (initGraph (), initGraph ())

            fun nodeToOut node = ( case F.Graph.Table.look (outs, node) of SOME s => S.listItems s
                                                                         | _ => raise Error )

            fun plotEdges node =
                let val def' = ( case G.Table.look (def, node) of SOME d => d | _ => raise Error )
                    val out' = ( case G.Table.look (outs, node) of SOME t => S.listItems t | _ => raise Error )
                    fun plotDefToOuts out def = G.mk_edge { from = tnode def, to = tnode out }
                    fun plotDefsToOuts out = map (plotDefToOuts out) def'
                in map plotDefsToOuts out' end

        in ( map plotEdges nodes;
             (IGRAPH { graph = graph, tnode = tnode, gtemp = gtemp, moves = moves }, nodeToOut) )
        end

    fun show (outstream, IGRAPH { graph = igraph, tnode = tnode, gtemp = gtemp, moves = moves }) =
        let val nodes = Graph.nodes igraph
            fun str n = Temp.makestring (gtemp n) 
            fun nodeStr n =  (str n) ^ " --> " ^ (String.concatWith "," (map str (Graph.adj n)))
        in  TextIO.output (outstream, String.concatWith "\n" (map nodeStr nodes) ^ "\n") end
end