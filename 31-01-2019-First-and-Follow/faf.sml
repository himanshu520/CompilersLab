(* This file contains the code to find the NULLABLE symbols and the FIRST and FOLLOW sets of all the symbols in the grammar
   defined in the file grammar.sml. *)


(* Set of atoms to store all the nullable symbols. *)
val nullable : Atom.atom list ref = ref nil;

(* Map of sets of atoms to store the FIRST and FOLLOW sets corresponding to each symbol in the grammar. *)
val first : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;
val follow : (AtomSet.set ref) AtomMap.map ref = ref AtomMap.empty;

(* Function to initialise a map variable (map of sets of atoms described above) with empty set correspoding to each symbol 
   in the grammar. 
   Then using the function to initialise first and follow maps. *)
fun init mp = let fun insert_map_symbol (x::xs) = (mp := AtomMap.insert (!mp, x, ref AtomSet.empty); insert_map_symbol xs)
                            |   insert_map_symbol _ = () in
                        insert_map_symbol (AtomSet.listItems (#symbols grammar))
                    end;
init first;
init follow;



(* Defining some helper functions. *)

(* Function to check whether whether an atom is a member of an atom list. *)
fun member_atom_list lst x = let fun cmp y = Atom.compare (x, y) = EQUAL in
                            		List.exists cmp lst
                        		end

(* Function to print an atom list. *)
fun print_atom_list (x::xs::xss) = (TextIO.print ((Atom.toString x) ^ ",\t"); print_atom_list (xs::xss))
| 	print_atom_list (x::xs) = (TextIO.print ((Atom.toString x) ^ "."); print_atom_list xs)
|	print_atom_list _ = (TextIO.print "\n");

(* Function to print an atom set *)
fun print_atom_set x = print_atom_list (AtomSet.listItems x);

(* Function to print a map of type ((AtomSet.set ref) AtomMap.map) *)
fun print_atom_atomset_map y = let fun print_atom_atomset_list ((x::xs) : (Atom.atom * (AtomSet.set ref)) list) = (TextIO.print ((Atom.toString (#1 x)) ^ " :-\t");
																												   print_atom_set (!(#2 x)); 
																												   print_atom_atomset_list xs)
					  			   		|   print_atom_atomset_list _ = () in
					  				print_atom_atomset_list (AtomMap.listItemsi y)
				   				end;



(* A reference variable to keep track of whether the calculation of FIRST, FOLLOW and NULLABLE has reached its fixed point. *)
val cont = ref false;



(* Implementation of NULLABLE. 
   Use "find_nullable ()" to get the list of nullables. *)

fun check_nullable_single x = if Atom.compare (x, Atom.atom "EPS") = EQUAL then true
							  else if AtomSet.member (#tokens grammar, x) then false
							       else member_atom_list (!nullable) x;

fun check_nullable_prod (x::xs) = if not (check_nullable_single x) then false else check_nullable_prod xs
|	check_nullable_prod _ = true;

fun check_nullable_rule (x::xs) = if check_nullable_prod x then true else check_nullable_rule xs
| 	check_nullable_rule _ = false;

fun check_nullable_symbol x = let val rl = RHSSet.listItems (AtomMap.lookup (#rules grammar, x)) in
									check_nullable_rule rl
								end;

fun check_nullable_symbols (x::xs) = (if member_atom_list (!nullable) x then () 
                                      else if check_nullable_symbol x then (nullable := x :: !nullable;
																            cont := true)
									       else (); 
                                      check_nullable_symbols xs)
|	check_nullable_symbols _ = ();

fun find_nullable () = (cont := false; check_nullable_symbols (AtomSet.listItems (#symbols grammar));
						if (!cont) then find_nullable () else ());




(* Implementation of FIRST.
   Use "find_first ()" to get the FIRST sets for each symbol in the grammar. *)

fun add_first_symbol y x = let val fst_x = if AtomSet.member (#tokens grammar, x) then AtomSet.add (AtomSet.empty, x)
                                           else if Atom.same (x, Atom.atom "EPS") then AtomSet.empty
										   else !(AtomMap.lookup (!first, x))
							   		val fst_y = !(AtomMap.lookup (!first, y)) in
							   if AtomSet.isSubset (fst_x, fst_y) then () 
							   else (cont := true; AtomMap.lookup (!first, y) := AtomSet.union(fst_x, fst_y))
							end;

fun find_first_prod y (x::xs) = (add_first_symbol y x; if member_atom_list (!nullable) x then find_first_prod y xs else ())
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



(* Implementation of FOLLOW.
   Use "find_follow ()" to get the FOLLOW sets for each symbol in the grammar. *)

fun add_follow_symbol y x (xs::xss) = (let val fst_xs = if AtomSet.member (#tokens grammar, xs) then AtomSet.add (AtomSet.empty, xs)
									   					else !(AtomMap.lookup (!first, xs))
												val foll_x = !(AtomMap.lookup (!follow, x)) in
											if AtomSet.isSubset (fst_xs, foll_x) then ()
											else (cont := true; AtomMap.lookup (!follow, x) := AtomSet.union (fst_xs, foll_x))
										end;
										if member_atom_list (!nullable) xs then add_follow_symbol y x xss else ())
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

fun find_follow () = (cont := false; find_follow_symbols (AtomSet.listItems (#symbols grammar));
					  if (!cont) then find_follow () else ());





(* Finally computing NULLABLE, FIRST, FOLLOW from the functions defined above and printing the result. *)
(find_nullable (); find_first (); find_follow ());
(TextIO.print ("The list of NULLABLE symbols :-\t"); print_atom_list (!nullable));
(TextIO.print ("\nThe list of FIRST tokens corresponding to each symbols in the grammar :-\n"); print_atom_atomset_map (!first));
(TextIO.print ("\nThe list of FOLLOW tokens correspoding to each symbols in the grammar :- \n"); print_atom_atomset_map (!follow));