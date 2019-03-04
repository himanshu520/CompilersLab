(* This file contains a sample grammar which can be copied to grammar.sml and run. This grammar is a LR(0) grammar.
   The grammar is - 
                                T -> S$
                                S -> (L) | x
                                L -> S | L, S                                   *)



(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "T", Atom.atom "S", Atom.atom "L"]);


(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "(", Atom.atom ")", Atom.atom ",", Atom.atom "x", Atom.atom "$"]);



(* Rules corresponding to each symbol in the grammar - each of type Productions.
   First an empty set corresponding to each symbol in the grammar is created and then each production corresponding to the symbol is 
   added successively. *)
val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "S", Atom.atom "$"]);
RuleMap.getProxy (Atom.atom "T", [Atom.atom "S", Atom.atom "$"]);

val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "(", Atom.atom "L", Atom.atom ")"]);
S_ := RHSSet.add (!S_, [Atom.atom "x"]);
RuleMap.getProxy (Atom.atom "S", [Atom.atom "(", Atom.atom "L", Atom.atom ")"]);
RuleMap.getProxy (Atom.atom "S", [Atom.atom "x"]);

val L_ = ref RHSSet.empty;
L_ := RHSSet.add (!L_, [Atom.atom "S"]);
L_ := RHSSet.add (!L_, [Atom.atom "L", Atom.atom ",", Atom.atom "S"]);
RuleMap.getProxy (Atom.atom "L", [Atom.atom "S"]);
RuleMap.getProxy (Atom.atom "L", [Atom.atom "L", Atom.atom ",", Atom.atom "S"]);


(* Rules corresponding to all the symbols - of type Rules.
   First an empty map is created and then the rules for all the symbols are added successively. *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "L", !L_);


(* Finally all the three components of the grammar - symbols, tokens and rules - defined above are combined in the record grammar
   (of type grammar). *)
val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };


(* Adding initial state to the map of states *)
val initialStateSlr = ref State.empty;
initialStateSlr := State.add (!initialStateSlr, { lhs = Atom.atom "T", before = [], after = [Atom.atom "S", Atom.atom "$"] });