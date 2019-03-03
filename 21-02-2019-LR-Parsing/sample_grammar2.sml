(* This file contains a sample grammar which can be copied to grammar.sml and run. This grammar is an SLR grammar.
   The grammar is -
                                S -> E$
                                E -> T + E | T
                                T -> x                                          *)



(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "E", Atom.atom "T"]);

(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "x", Atom.atom "+", Atom.atom "$"]);


(* Rules corresponding to each symbol in the grammar - each of type Productions.
   First an empty set corresponding to each symbol in the grammar is created and then each production corresponding to the symbol is 
   added successively. *)
val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "E", Atom.atom "$"]);
RuleMap.getProxy (Atom.atom "S", [Atom.atom "E", Atom.atom "$"]);

val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "T", Atom.atom "+", Atom.atom "E"]);
E_ := RHSSet.add (!E_, [Atom.atom "T"]);
RuleMap.getProxy (Atom.atom "E", [Atom.atom "T", Atom.atom "+", Atom.atom "E"]);
RuleMap.getProxy (Atom.atom "E", [Atom.atom "T"]);

val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "x"]);
RuleMap.getProxy (Atom.atom "T", [Atom.atom "x"]);


(* Rules corresponding to all the symbols - of type Rules.
   First an empty map is created and then the rules for all the symbols are added successively. *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);


(* Finally all the three components of the grammar - symbols, tokens and rules - defined above are combined in the record grammar
   (of type grammar). *)
val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };


(* Adding initial state to the map of states *)
val initialStateLr0 = ref State.empty;
initialStateLr0 := State.add (!initialStateLr0, { lhs = Atom.atom "S", before = [], after = [Atom.atom "E", Atom.atom "$"] });