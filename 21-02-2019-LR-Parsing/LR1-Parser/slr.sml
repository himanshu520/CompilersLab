(* This file contains functions to calculate and print SLR table of a grammar in file grammar.sml *)

use "type.sml";
use "grammar.sml";
use "faf.sml";


(* function to find the closure of a state *)
fun closure st = let 
                    val newSt = ref st;
                    val lrItems = State.listItems st;
                    fun addSym sym = let 
                                         val rls = AtomMap.lookup (#rules grammar, sym);
                                         val prods = RHSSet.listItems (rls);
                                         fun addProd prd = newSt := State.add (!newSt, { lhs = sym, before = [], after = prd })
                                     in
                                         List.app addProd prods
                                     end;
                    fun proc { lhs = z, before = ys, after = x::xs } = if AtomSet.member (#symbols grammar, x) then addSym x else ()
                    |   proc _ = ();
                 in 
                    (List.app proc lrItems;
                     if State.equal (st, !newSt) then st else closure (!newSt))
                 end;


(* function to add accept action to a given state *)
fun addAccept stNum = let val old = LRTable.lookup (!lrTable, (stNum, Atom.atom "$"));
                          val new = LRTableEl.add (!old, Accept);
                      in (LRTable.lookup (!lrTable, (stNum, Atom.atom "$"))) := new end;


(* function to calculate goto(st, x) where st is a state and x is a terminal or non-terminal *)
fun goto (st, t) = if Atom.compare (t, Atom.atom "$") = EQUAL 
                   then let val stNum = StateMap.getProxy st;
                            val lrItems = State.listItems st;
                            fun proc { lhs = z, before = ys, after = [x] } = if Atom.compare (x, Atom.atom "$") = EQUAL then addAccept stNum else ()
                            |   proc _ = ();
                        in (List.app proc lrItems; State.empty) end
                   else let 
                            val nst = ref State.empty;
                            val lrItems = State.listItems st;
                            fun proc {lhs = z, before = ys, after = x::xs } = if Atom.compare (x, t) = EQUAL then nst := State.add (!nst, { lhs = z, before = x::ys, after = xs }) 
                                                                              else ()
                            |   proc _ = ();
                        in (List.app proc lrItems; closure (!nst)) end;


(* function to add a new row (corresponding to a new state in the LR table *)
fun addRowLrTable x = let fun addCol y = lrTable := LRTable.insert (!lrTable, (x, y), ref LRTableEl.empty);
                      in List.app addCol (AtomSet.listItems (AtomSet.union (#symbols grammar, #tokens grammar))) end;


(* Function to fill the LR0 table with shift, accept and goto actions *)
fun computeLrTable stNum = if stNum >= !(StateMap.cnt) then ()
                           else let 
                                    val state = StateMap.getItem stNum
                                    fun addEdge x = let val nSt = StateMap.getProxy (goto (state, x));
                                                        val ent:actions = if AtomSet.member (#symbols grammar, x) then (Goto nSt) else (Shift nSt);
                                                        val old = LRTable.lookup (!lrTable, (stNum, x));
                                                        val new = LRTableEl.add (!old, ent);
                                                    in (LRTable.lookup (!lrTable, (stNum, x))) := new end;
                                in (addRowLrTable stNum; List.app addEdge (AtomSet.listItems (AtomSet.union (#symbols grammar, #tokens grammar))); computeLrTable (stNum + 1)) end;


(* Function to fill the LR0 table with reduce actions *)
fun addReduceActions stNum = if stNum >= !(StateMap.cnt) then ()
                             else let 
                                    val itemList = State.listItems (StateMap.getItem stNum);
                                    fun procItem { lhs = z, before = x, after = nil } = let val rl = RuleMap.getProxy (z, List.rev x)
                                                                                            val followSet = !(AtomMap.lookup (!follow, z))
                                                                                            fun addRedTable t = if not (AtomSet.member (followSet, t)) then ()
                                                                                                                else let val old = LRTable.lookup (!lrTable, (stNum, t));
                                                                                                                         val new = LRTableEl.add (!old, Reduce rl);
                                                                                                                     in (LRTable.lookup (!lrTable, (stNum, t))) := new end;
                                                                                        in List.app addRedTable (AtomSet.listItems (#tokens grammar)) end
                                    |   procItem _ = ();
                                  in (List.app procItem itemList; addReduceActions (stNum + 1)) end;


(* Function to print the numerical encoding of the grammar rules *)
fun printGrammar () = let val grm = RuleMap.getList ();
                          fun printAtom x = TextIO.print ((Atom.toString x) ^ " ");
                          val printAtomList = List.app printAtom;
                          fun printRule ((x, y), z) = (TextIO.print ("(" ^ (Int.toString z) ^ ") "); printAtom x; TextIO.print ("--> "); printAtomList y; TextIO.print "\n");
                      in (TextIO.print "\n\n========= The grammar rules are =========\n"; List.app printRule grm; TextIO.print "=========================================\n\n") end;


(* Function to print a given state of the grammar *)
fun printState stNum = let val lrItems = State.listItems (StateMap.getItem stNum);
                           fun printAtom x = TextIO.print ((Atom.toString x) ^ " ");
                           val printAtomList = List.app printAtom;
                           fun printLrItem { lhs = x, before = y, after = z } = (printAtom x; TextIO.print "--> "; printAtomList (List.rev y); TextIO.print ". "; printAtomList z; TextIO.print "\n");
                           fun printAction (Reduce x) = if x = 0 then () else TextIO.print ("r" ^ (Int.toString x))
                           |   printAction (Shift x) = if x = 0 then () else TextIO.print ("s" ^ (Int.toString x)) 
                           |   printAction (Goto x) = if x = 0 then () else TextIO.print ("g" ^ (Int.toString x)) 
                           |   printAction _ = TextIO.print ("a"); 
                           fun printActions (x::y::xs) = (printAction x; TextIO.print ", "; printActions (y::xs))
                           |   printActions (x::xs) = printAction x
                           |   printActions _ = ();
                           fun printActionPair (x, y) = (printAtom x; TextIO.print ":  "; printActions y; TextIO.print "\n");
                           fun printActionForSym x = printActionPair (x, LRTableEl.listItems (!(LRTable.lookup (!lrTable, (stNum, x)))));
                           fun printActionState () = List.app printActionForSym (AtomSet.listItems (AtomSet.union (#symbols grammar, #tokens grammar)));
                       in ( TextIO.print ("\n\n=============== State " ^ (Int.toString stNum) ^ " =================\n");
                            List.app printLrItem lrItems;
                            TextIO.print ("-----------------------------------------\n");
                            printActionState ();
                            TextIO.print ("=========================================\n=========================================\n\n"))
                       end;


(* Function to print the LR0 table *)
fun printLrTable stNum = if stNum >= !(StateMap.cnt) then ()
                         else (printState stNum; printLrTable (stNum + 1));


StateMap.getProxy (closure (!initialStateLr0));
(computeLrTable 1; addReduceActions 1; printGrammar (); printLrTable 1);