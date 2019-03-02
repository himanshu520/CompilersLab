(* This file contains the definition of the datatypes and type aliases used in program *)


(* Type to represent a single production *)
type RHS = Atom.atom list;

(* A structure to define ordering relation on the RHS type.
   List.collate takes a function that compares the members of a list and return a function that takes two lists and determine their 
   according to the ordring function. *)
structure RHS_KEY = struct
	type ord_key = RHS
	val compare = List.collate Atom.lexCompare
end;

(* Using the ordering sturcture defined above to create a set datatype.
   RedBlackSetFn is a functor that takes a structure of signature ORD_KEY as argument and returns a structure which define type set 
   (and associated functions whose members can be of type ord_key (Key.ord_key) defined in the argument structure. This set is 
   internally represented as a red-black tree.
   We can use <structure_name>.empty to create a new set of the type. After this we can insert elements in the set. *)
structure RHSSet = RedBlackSetFn (RHS_KEY);

(* Type of the sets created by the RHSSet.
   This set type will represent all the productions corresponding to a symbol. *)
type Productions = RHSSet.set;

(* Rules is a map datatype whose keys are of Atom.atom type and the values are of Productions type.
   This type will be used to store the grammar rules corresponding to all the symbols in the grammar.
   The symbols in the grammar would be the key and corresponding productions would the corresponding values. *)
type Rules = Productions AtomMap.map;

(* This type represents a complete grammar rule.
   It is a record consisting of three fields symbols - a set of atoms representing all symbols in the grammar
                                                     - a set of atoms representing all tokens in the grammar
                                                     - a map of type Rules to represent all the grammar rules. *)
type Grammar = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules };




(* Datatype to represent an LR(0) item 
   lhs represent the left hand side non-terminal of a production
   before represents the symbols on the rhs before the dot stored in reverse order for easy insertion in the end 
   after represents the symbols on the rhs after the dot stored in the same order *)
type Item = { lhs : Atom.atom, before : Atom.atom list, after : Atom.atom list };

(* Datatype to represent represent a state 
   First the comparison function for the items of the state is defined and then the actual structure is created *)
structure ITEM_KEY : ORD_KEY = struct
    type ord_key = Item;
    fun compare (x : ord_key, y : ord_key) = let val lhsCmp = Atom.compare (#lhs x, #lhs y)
                                                in 
                                                    if  lhsCmp <> EQUAL then lhsCmp
                                                    else let val beforeCmp = List.collate Atom.lexCompare (#before x, #before y)
                                                        in
                                                            if beforeCmp <> EQUAL then beforeCmp
                                                            else List.collate Atom.lexCompare (#after x, #after y)
                                                        end
                                                end
end

structure State = RedBlackSetFn(ITEM_KEY);

(* Structure to maintain states and grammar rules *)
structure STATE_KEY : ORD_KEY = struct
    type ord_key = State.set;
    val compare = State.compare;
end;

structure RULE_KEY : ORD_KEY = struct
    type ord_key = Atom.atom * RHS;
    fun compare (x : ord_key, y : ord_key) = let val lhsCmp = Atom.compare (#1 x, #1 y)
                                                in
                                                    if lhsCmp <> EQUAL then lhsCmp
                                                    else List.collate Atom.lexCompare (#2 x, #2 y)
                                                end;
end;

signature STATE_MAP = sig
    type proxy;
    type item;
    val cnt : proxy ref;
    val getProxy : item -> proxy;
    val getItem : proxy -> item;
    val getList : unit -> (item * proxy) list;
end;

functor Proxy (ARG_KEY : ORD_KEY) : STATE_MAP = struct
    type proxy = int;
    val proxyCmp = Int.compare;
    type item = ARG_KEY.ord_key;
    val cnt = ref 0;
    structure PROXY_KEY = struct
        type ord_key = proxy
        val compare = proxyCmp;
    end;
    structure proxyMap = RedBlackMapFn (ARG_KEY);
    structure itemMap = RedBlackMapFn (PROXY_KEY);
    val fmap : proxy proxyMap.map ref = ref proxyMap.empty;
    val rmap : item itemMap.map ref = ref itemMap.empty;
    fun getProxy it = (if proxyMap.inDomain (!fmap, it) 
                       then ()
                       else (fmap := proxyMap.insert (!fmap, it, !cnt);
                             rmap := itemMap.insert (!rmap, !cnt, it);
                             cnt := !cnt + 1);
                       proxyMap.lookup(!fmap, it));
    fun getItem prx = itemMap.lookup (!rmap, prx);
    fun getList () = proxyMap.listItemsi (!fmap);
end;

structure StateMap = Proxy(STATE_KEY);
structure RuleMap = Proxy(RULE_KEY);


(* Datatype for transitions *)
datatype actions = Shift of int | Goto of int | Reduce of int | Accept


(* Datatype for LR0 table - it would be a map from (int * atom) to actions *)
structure LR_TABLE_KEY = struct
    type ord_key = int * Atom.atom
    fun compare ((xi, xa), (yi, ya)) = if Int.compare (xi, yi) = EQUAL then Atom.compare (xa, ya)
                                       else Int.compare (xi, yi);
end;

structure LR_TABLE_EL_KEY = struct
    type ord_key = actions;
    fun compare (Shift x, Shift y) = Int.compare (x, y)
    |   compare (Shift x, _) = GREATER
    |   compare (_, Shift x) = LESS
    |   compare (Goto x, Goto y) = Int.compare (x, y)
    |   compare (Goto x, _) = GREATER
    |   compare (_, Goto x) = LESS
    |   compare (Reduce x, Reduce y) = Int.compare (x, y)
    |   compare (Reduce x, _) = GREATER
    |   compare (_, Reduce x) = LESS
    |   compare (_, _) = EQUAL;
end;

structure LRTable = RedBlackMapFn(LR_TABLE_KEY);
structure LRTableEl = RedBlackSetFn(LR_TABLE_EL_KEY);
val lrTable : LRTableEl.set ref LRTable.map ref = ref LRTable.empty;