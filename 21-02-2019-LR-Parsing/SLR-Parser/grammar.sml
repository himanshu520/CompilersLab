(* Set of atoms to represent the symbols in the grammar. *)
val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "T", Atom.atom "S", Atom.atom "E", Atom.atom "V"]);


(* Set of atoms to represent the tokens in the grammar. *)
val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "x", Atom.atom "*", Atom.atom "=", Atom.atom "$"]);


(* Rules corresponding to each symbol in the grammar - each of type Productions.
   First an empty set corresponding to each symbol in the grammar is created and then each production corresponding to the symbol is 
   added successively. *)
val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "S", Atom.atom "$"]);
RuleMap.getProxy (Atom.atom "T", [Atom.atom "S", Atom.atom "$"]);

val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "V", Atom.atom "=", Atom.atom "E"]);
S_ := RHSSet.add (!S_, [Atom.atom "E"]);
RuleMap.getProxy (Atom.atom "S", [Atom.atom "V", Atom.atom "=", Atom.atom "E"]);
RuleMap.getProxy (Atom.atom "S", [Atom.atom "E"]);

val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "V"]);
RuleMap.getProxy (Atom.atom "E", [Atom.atom "V"]);

val V_ = ref RHSSet.empty;
V_ := RHSSet.add (!V_, [Atom.atom "*", Atom.atom "E"]);
V_ := RHSSet.add (!V_, [Atom.atom "x"]);
RuleMap.getProxy (Atom.atom "V", [Atom.atom "*", Atom.atom "E"]);
RuleMap.getProxy (Atom.atom "V", [Atom.atom "x"]);


(* Rules corresponding to all the symbols - of type Rules.
   First an empty map is created and then the rules for all the symbols are added successively. *)
val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "V", !V_);


(* Finally all the three components of the grammar - symbols, tokens and rules - defined above are combined in the record grammar
   (of type grammar). *)
val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };


(* Adding initial state to the map of states *)
val initialStateSlr = ref State.empty;
initialStateSlr := State.add (!initialStateSlr, { lhs = Atom.atom "T", before = [], after = [Atom.atom "S", Atom.atom "$"] });