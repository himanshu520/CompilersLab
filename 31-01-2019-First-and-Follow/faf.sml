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

fun check_nullable_symbols (x::xs) = (if member_list (!nullable) x then () 
                                      else if check_nullable_symbol x then (nullable := x :: !nullable;
																            cont := true)
									       else (); 
                                      check_nullable_symbols xs)
|	check_nullable_symbols _ = ();

fun find_nullable () = (cont := false; check_nullable_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_nullable () else ());

find_nullable ();

fun print_list (x::xs) = (TextIO.print ((Atom.toString x) ^ "\n"); print_list xs)
|	print_list _ = (TextIO.print "Done\n");

print_list (!nullable);


val first : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;

fun init_first () = let fun temp (x::xs) = (first := AtomMap.insert (!first, x, ref AtomSet.empty); temp xs)
                            |   temp _ = () in
                        temp (AtomSet.listItems (#symbols grammar))
                    end;

init_first ();

fun add_first_symbol y x = let val fst_x = if AtomSet.member (#tokens grammar, x) then AtomSet.add (AtomSet.empty, x)
                                           else if Atom.same (x, Atom.atom "EPS") then AtomSet.empty
										   else !(AtomMap.lookup (!first, x))
							   		val fst_y = !(AtomMap.lookup (!first, y)) in
							   if AtomSet.isSubset (fst_x, fst_y) then () 
							   else (cont := true; AtomMap.lookup (!first, y) := AtomSet.union(fst_x, fst_y))
							end;

fun find_first_prod y (x::xs) = (add_first_symbol y x; if member_list (!nullable) x then find_first_prod y xs else ())
|	find_first_prod _ _ = ();

fun find_first_rule y (x::xs) = (find_first_prod y x; find_first_rule y xs)
| 	find_first_rule _ _ = ();

fun find_first_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup (#rules grammar, x)) in
							 	find_first_rule x rl
							end;

fun find_first_symbols (x::xs) = (find_first_symbol x; find_first_symbols xs)
|	find_first_symbols _ = ();

fun find_first () = (cont := false; find_first_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_first () else ());


fun print_set x = print_list (AtomSet.listItems x);
fun print_map y = let fun prn ((x::xs) : (Atom.atom * (AtomSet.set ref)) list) = (TextIO.print ("Printing for symbol " ^ (Atom.toString (#1 x)) ^ ":-\n");
											print_set (!(#2 x)); prn xs)
					  |	  prn _ = () in
					  prn (AtomMap.listItemsi y)
				   	end;

find_first ();
print_map (!first);	



val follow : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;

fun init_follow () = let fun temp (x::xs) = (follow := AtomMap.insert (!follow, x, ref AtomSet.empty); temp xs)
                            |   temp _ = () in
                        temp (AtomSet.listItems (#symbols grammar))
                    end;

init_follow ();

fun add_follow_symbol y x (xs::xss) = (let val fst_xs = if AtomSet.member (#tokens grammar, xs) then AtomSet.add (AtomSet.empty, xs)
									   					else !(AtomMap.lookup (!first, xs))
												val foll_x = !(AtomMap.lookup (!follow, x)) in
											if AtomSet.isSubset (fst_xs, foll_x) then ()
											else (cont := true; AtomMap.lookup (!follow, x) := AtomSet.union (fst_xs, foll_x))
										end;
										if member_list (!nullable) xs then add_follow_symbol y x xss else ())
| 	add_follow_symbol y x _ = let val foll_y = !(AtomMap.lookup (!follow, y)) 
										val foll_x = !(AtomMap.lookup (!follow, x)) in
									if AtomSet.isSubset (foll_y, foll_x) then ()
									else (cont := true; AtomMap.lookup (!follow, x) := AtomSet.union (foll_x, foll_y))
								end;

fun find_follow_prod y (x::xs) = (if AtomSet.member (#symbols grammar, x) then add_follow_symbol y x xs else ();
								  find_follow_prod y xs)
|	find_follow_prod _ _ = ();

fun find_follow_rule y (x::xs) = (find_follow_prod y x; find_follow_rule y xs)
| 	find_follow_rule _ _ = ();

fun find_follow_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup (#rules grammar, x)) in
							 	find_follow_rule x rl
							end;

fun find_follow_symbols (x::xs) = (find_follow_symbol x; find_follow_symbols xs)
|	find_follow_symbols _ = ();

val cnt = ref 0;

fun find_follow () = (cnt := !cnt + 1; TextIO.print (Int.toString (!cnt)); cont := false; find_follow_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_follow () else ());


find_follow ();
print_map (!follow);	