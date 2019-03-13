use "faf.sml";

(* Defining datatypes required for LL1 table. *)

(* Type for entries in the LL1 table *)
type ll_table_value = (Atom.atom * RHS) list ref;

(* Defining ll_table_key structure (of ORD_KEY signature).
   This structure defining ordering for two members of the map which is used in RedBlackMapFn functor to 
   create a structure that contains map datatype to represent the table. *)
structure ll_table_key = struct
    type ord_key = (Atom.atom * Atom.atom)
    fun compare (x : ord_key, y : ord_key) = let val t = Atom.compare (#1 x, #1 y) in
                                                if t = EQUAL then Atom.compare (#2 x, #2 y)
                                                else t 
                                             end
end;

structure LL_TABLE = RedBlackMapFn (ll_table_key);

(* Using the above structure to create the map *)
val ll_table : ll_table_value LL_TABLE.map ref = ref LL_TABLE.empty;



(* Defining some helper functions. *)

(* Functions to initialise the map corresponding to each symbol-token combination *)
fun init_ll_table_insert x = ll_table := LL_TABLE.insert (!ll_table, x, ref []);
fun init_ll_table_symbols y = let fun init_ll_table_symbol (x::xs) = (init_ll_table_insert (x, y); 
                                                                      init_ll_table_symbol xs) 
                                  |   init_ll_table_symbol _ = () in
                                    init_ll_table_symbol (AtomSet.listItems (#symbols grammar))
                                end;
fun init_ll_table_tokens () = app init_ll_table_symbols (AtomSet.listItems (#tokens grammar))
val init_ll_table = init_ll_table_tokens;

(* Functions to print the table contents. *)
fun print_atom x = TextIO.print ((Atom.toString x) ^ " ");
fun print_ll_table_production (x, y) = (TextIO.print ((Atom.toString x) ^ " -> ");
                                        app print_atom y;
                                        TextIO.print ",\t");
fun print_ll_table_entry ((x, y), z) = (TextIO.print ("(" ^ (Atom.toString x) ^ ", " ^ (Atom.toString y) ^ ") :-\t");
                                        app print_ll_table_production (!z);
                                        TextIO.print "\n");
fun print_ll_table () = app print_ll_table_entry (LL_TABLE.listItemsi (!ll_table))

(* Function to check if a production is already present in a table entry *)
fun member_table_entry ((_, x)::xs) (z, y) = if List.collate (Atom.compare) (x, y) = EQUAL then true 
                                             else member_table_entry xs (z, y)
|   member_table_entry _ _ = false;



(* Implementation for filling up the LL1 table. *)
init_ll_table ();

fun fill_ll_table_entry x z y = let val prev_ent = LL_TABLE.lookup (!ll_table, (x, y)) in
                                    if member_table_entry (!prev_ent) (x, z) then ()
                                    else prev_ent := (x, z) :: (!prev_ent)
                                end;

fun fill_ll_table_symbol_production y z (x::xs) = if AtomSet.member (#tokens grammar, x) then fill_ll_table_entry y z x
                                                  else if Atom.compare (x, Atom.atom "EPS") = EQUAL then fill_ll_table_symbol_production y z xs
                                                  else (app (fill_ll_table_entry y z) (AtomSet.listItems (!(AtomMap.lookup(!first, x))));
                                                      if member_atom_list (!nullable) x then fill_ll_table_symbol_production y z xs else ())
|   fill_ll_table_symbol_production y z _ = app (fill_ll_table_entry y z) (AtomSet.listItems (!(AtomMap.lookup(!follow, y))));

fun fill_ll_table_symbol (z, y) = let fun fill_ll_table_symbol_productions (x::xs) = (fill_ll_table_symbol_production z x x;
                                                                                      fill_ll_table_symbol_productions xs) 
                                      |   fill_ll_table_symbol_productions _ = () in
                                        fill_ll_table_symbol_productions (RHSSet.listItems y)
                                    end;

fun fill_ll_table () = app fill_ll_table_symbol (AtomMap.listItemsi (#rules grammar));



(* Using the created functions to fill the table and show the filled table as output *)
fill_ll_table ();
print_ll_table ();