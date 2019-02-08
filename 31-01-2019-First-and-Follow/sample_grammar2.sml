(* This file contains a sample grammar which can be copied to grammar.sml and run.
   The grammar is - 
                                Z -> d | XYZ
                                Y -> c | eps
                                X -> a | Y                                          *)



(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "X", Atom.atom "Y", Atom.atom "Z"]);


(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "a", Atom.atom "c", Atom.atom "d"]);


(* Rules corresponding to each symbol in the grammar - each of type Productions.
   First an empty set corresponding to each symbol in the grammar is created and then each production corresponding to the symbol is 
   added successively. *)
val Z_ = ref RHSSet.empty;
Z_ := RHSSet.add (!Z_, [Atom.atom "d"]);
Z_ := RHSSet.add (!Z_, [Atom.atom "X", Atom.atom "Y", Atom.atom "Z"]);

val Y_ = ref RHSSet.empty;
Y_ := RHSSet.add (!Y_, [Atom.atom "EPS"]);
Y_ := RHSSet.add (!Y_, [Atom.atom "c"]);

val X_ = ref RHSSet.empty;
X_ := RHSSet.add (!X_, [Atom.atom "Y"]);
X_ := RHSSet.add (!X_, [Atom.atom "a"]);


(* Rules corresponding to all the symbols - of type Rules.
   First an empty map is created and then the rules for all the symbols are added successively. *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "Z", !Z_);
rule := AtomMap.insert (!rule, Atom.atom "Y", !Y_);
rule := AtomMap.insert (!rule, Atom.atom "X", !X_);


(* Finally all the three components of the grammar - symbols, tokens and rules - defined above are combined in the record grammar
   (of type grammar). *)
val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };