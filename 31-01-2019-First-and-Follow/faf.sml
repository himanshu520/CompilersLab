type RHS = Atom.atom list

structure RHS_KEY = struct
	type ord_key = RHS
	val compare = List.collate Atom.lexCompare
end;

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

type Rules = Productions AtomMap.map

type Grammar = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


val sym = ref AtomSet.empty;
sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "E", Atom.atom "T", Atom.atom "F"]);

val tok = ref AtomSet.empty;
tok := AtomSet.addList (!tok, [Atom.atom "num", Atom.atom "id", Atom.atom "(", Atom.atom ")", Atom.atom "*", Atom.atom "+", Atom.atom "$"]);

val S_ = ref RHSSet.empty;
S_ := RHSSet.add (!S_, [Atom.atom "E", Atom.atom "$"]);

val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "E", Atom.atom "+", Atom.atom "T"]);
E_ := RHSSet.add (!E_, [Atom.atom "T"]);

val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "T", Atom.atom "*", Atom.atom "F"]);
T_ := RHSSet.add (!T_, [Atom.atom "F"]);

val F_ = ref RHSSet.empty;
F_ := RHSSet.add (!F_, [Atom.atom "id"]);
F_ := RHSSet.add (!F_, [Atom.atom "num"]);	
F_ := RHSSet.add (!F_, [Atom.atom "(", Atom.atom "E", Atom.atom ")"]);