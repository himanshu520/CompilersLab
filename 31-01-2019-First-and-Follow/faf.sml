type RHS = Atom.atom list;

structure RHS_KEY = struct
	type ord_key = RHS
	val compare = List.collate Atom.lexCompare
end;

structure RHSSet = RedBlackSetFn (RHS_KEY);

type Productions = RHSSet.set;

type Rules = Productions AtomMap.map;

type Grammar = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules };


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

val rule : Rules ref = ref AtomMap.empty;
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_);

val grammar : Grammar = { symbols = !sym, tokens = !tok, rules = !rule }

val nullable : Atom.atom list ref = ref nil;

fun member_list lst x = let fun cmp y = Atom.compare (x, y) = EQUAL in
                            List.exists cmp lst
                        end

fun check_nullable_single x = if Atom.compare (x, Atom.atom "EPS") = EQUAL then true
							  else if AtomSet.member (#tokens grammar, x) then false
							       else member_list (!nullable) x;

fun check_nullable_prod (x::xs) = if not (check_nullable_single x) then false else check_nullable_prod xs
|	check_nullable_prod _ = true;

fun check_nullable_rule (x::xs) = if check_nullable_prod x then true else check_nullable_rule xs
| 	check_nullable_rule _ = false;

fun check_nullable_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup (#rules grammar, x)) in
									check_nullable_rule rl
								end;

val cont = ref false;
val cnt = ref 0;
fun check_nullable_symbols (x::xs) = (if member_list (!nullable) x then () 
                                      else if check_nullable_symbol x then (nullable := x :: !nullable;
																            cont := true)
									       else (); 
                                      check_nullable_symbols xs)
|	check_nullable_symbols _ = ();

fun find_nullable () = (cnt := !cnt + 1; TextIO.print (Int.toString (!cnt)); cont := false; check_nullable_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_nullable () else ());

find_nullable ();

fun print_list (x::xs) = (TextIO.print ((Atom.toString x) ^ "\n"); print_list xs)
|	print_list _ = (TextIO.print "Done\n");

print_list (!nullable);
