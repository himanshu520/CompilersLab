use "faf.sml";

type ll_table_value = (Atom.atom * RHS) list ref;

structure ll_table_key = struct
    type ord_key = (Atom.atom * Atom.atom)
    fun compare (x : ord_key, y : ord_key) = let val t = Atom.compare (#1 x, #1 y) in
                                                if t = EQUAL then Atom.compare (#2 x, #2 y)
                                                else t 
                                             end
end;

structure LL_TABLE = RedBlackMapFn (ll_table_key); 

val ll_table : ll_table_value LL_TABLE.map ref = ref LL_TABLE.empty;

fun init_ll_table_insert x = ll_table := LL_TABLE.insert (!ll_table, x, ref []);

fun init_ll_table_symbols y = let fun init_ll_table_symbol (x::xs) = (init_ll_table_insert (x, y); 
                                                                      init_ll_table_symbol xs) 
                                  |   init_ll_table_symbol _ = () in
                                    init_ll_table_symbol (AtomSet.listItems (#symbols grammar))
                                end;

fun init_ll_table_tokens () = app init_ll_table_symbols (AtomSet.listItems (#tokens grammar))

val init_ll_table = init_ll_table_tokens;

init_ll_table ();

fun print_atom x = TextIO.print ((Atom.toString x) ^ " ");
fun print_ll_table_production (x, y) = (TextIO.print ((Atom.toString x) ^ " -> ");
                                        app print_atom y;
                                        TextIO.print ",\t");
fun print_ll_table_entry ((x, y), z) = (TextIO.print ("(" ^ (Atom.toString x) ^ ", " ^ (Atom.toString y) ^ " :-\t");
                                        app print_ll_table_production (!z);
                                        TextIO.print "\n");
fun print_ll_table () = app print_ll_table_entry (LL_TABLE.listItemsi (!ll_table))