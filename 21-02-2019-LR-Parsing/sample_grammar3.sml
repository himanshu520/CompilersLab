(* This file contains a sample grammar which can be copied to grammar.sml and run.
   The grammar is -
                                S -> E$
                                E -> XF
                                F -> eps | aE
                                X -> b | ( E )                                  *)



(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "E", Atom.atom "F", Atom.atom "X"]);


(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "a", Atom.atom "b", Atom.atom "(", Atom.atom ")", Atom.atom "$"]);


(* Rules corresponding to each symbol in the grammar - each of type Productions.
   First an empty set corresponding to each symbol in the grammar is created and then each production corresponding to the symbol is 
   added successively. *)
val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "E", Atom.atom "$"]);

val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "X", Atom.atom "F"]);

val F_ = ref RHSSet.empty;
F_ := RHSSet.add (!F_, [Atom.atom "EPS"]);
F_ := RHSSet.add (!F_, [Atom.atom "a", Atom.atom "E"]);

val X_ = ref RHSSet.empty;
X_ := RHSSet.add (!X_, [Atom.atom "(", Atom.atom "E", Atom.atom ")"]);
X_ := RHSSet.add (!X_, [Atom.atom "b"]);


(* Rules corresponding to all the symbols - of type Rules.
   First an empty map is created and then the rules for all the symbols are added successively. *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_);
rule := AtomMap.insert (!rule, Atom.atom "X", !X_);


(* Finally all the three components of the grammar - symbols, tokens and rules - defined above are combined in the record grammar
   (of type grammar). *)
val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };