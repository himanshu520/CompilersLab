use "type.sml";
use "grammar.sml";

(* function to find the closure of a state *)
fun closure st = let 
                    val newSt = ref st;
                    val lrItems = State.listItems st;
                    fun addSym sym = let 
                                         val rls = AtomMap.lookup (#rules grammar, sym);
                                         val prods = RHSSet.listItems (rls);
                                         fun addProd prd = newSt := State.add (!newSt, { lhs = sym, before = [], after = prd})
                                     in
                                         List.app addProd prods
                                     end;
                    fun proc { lhs = z, before = ys, after = x::xs } = if AtomSet.member (#symbols grammar, x) then addSym x else ()
                    |   proc _ = ();
                 in 
                    (List.app proc lrItems;
                     if State.equal (st, !newSt) then st else closure (!newSt))
                 end;

(* function to calculate goto(st, x) where st is a state and x is a terminal or non-terminal *)
fun goto (st, x) = let 
                    val nst = ref State.empty;
                    val lrItems = State.listItems st;
                    fun proc {lhs = z, before = ys, after = x::xs } = nst := State.add (!nst, { lhs = z, before = x::ys, after = xs})
                    |   proc _ = ();
                in 
                    (List.app proc lrItems; 
                     closure (!nst))
                end;

(* function to add a new row (corresponding to a new state in the LR table *)
fun addRowLrTable x = if (!StateMap.cnt) > x then ()
                      else 
                        let fun addCol y = lrTable := LRTable.insert (!lrTable, (x, y), ref LRTableEl.empty)
                        in List.app addCol (AtomSet.listItems (AtomSet.union (#symbols grammar, #tokens grammar))) end;

fun computeLrTable stNum = if stNum >= !(StateMap.cnt) then ()
                           else let 
                                    val state = StateMap.getItem stNum
                                    fun addEdge x = let val nSt = StateMap.getProxy (goto (state, x));
                                                        val ent:actions = if AtomSet.member (#symbols grammar, x) then (Goto nSt) else (Shift nSt);
                                                        val old = LRTable.lookup (!lrTable, (stNum, x));
                                                        val new = LRTableEl.add (!old, ent);
                                                    in (LRTable.lookup (!lrTable, (stNum, x))) := new end;
                                in (List.app addEdge (AtomSet.listItems (AtomSet.union (#symbols grammar, #tokens grammar))); computeLrTable (stNum + 1)) end;
                                    

                            
