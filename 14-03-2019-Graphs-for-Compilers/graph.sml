signature GRAPH = sig
    type node 
    val addEdge : (node * node) -> unit
    val succ    : node -> node list
    val pred    : node -> node list 
end

structure graph : GRAPH = struct
    type node = int

    structure IntKey : ORD_KEY = struct
        type ord_key = int
        val compare = Int.compare
    end

    structure nodeIntMapStruct = RedBlackMapFn(IntKey)
    val successor : node list nodeIntMapStruct.map ref = ref nodeIntMapStruct.empty
    val predecessor : node list nodeIntMapStruct.map ref = ref nodeIntMapStruct.empty


    fun addEdge (x, y) = let val curSuccList = if nodeIntMapStruct.inDomain (!successor, x)
                                               then nodeIntMapStruct.lookup (!successor, x)
                                               else []
                             val curPredList = if nodeIntMapStruct.inDomain (!predecessor, y) 
                                               then nodeIntMapStruct.lookup (!predecessor, y)
                                               else []
                         in ( successor := nodeIntMapStruct.insert (!successor, x, y::curSuccList);
                              predecessor := nodeIntMapStruct.insert (!predecessor, y, x::curPredList) ) 
                         end
    
    fun succ x = if nodeIntMapStruct.inDomain (!successor, x)
                 then nodeIntMapStruct.lookup (!successor, x)
                 else []
    fun pred y = if nodeIntMapStruct.inDomain (!predecessor, y)
                 then nodeIntMapStruct.lookup (!predecessor, y)
                 else []
end