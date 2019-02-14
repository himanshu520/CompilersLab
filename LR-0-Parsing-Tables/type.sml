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