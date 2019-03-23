(* This file contains datatype for the types in Tiger *)

structure Types = struct

    type unique = unit ref  (* The only useful things that can be done with a unit ref is to test for equality
                               Different instances of unit ref are not equal *)

    datatype ty = INT                                               (* Fundamental type *)
                | STRING                                            (* Fundamental type *)
                | UNIT                                              (* For expressions that do not return values *)
                | NIL                                               (* NIL is special constructor for 'nil' which belongs to any record type *)
                | RECORD of (Symbol.symbol * ty) list * unique      (* Derived type *)
                | ARRAY of ty * unique                              (* Derived type *)
                | NAME of Symbol.symbol * ty option ref             (* Placeholder for types whose names we have seen but not yet its definition 
                                                                       Useful for analysing recursive type definitions *)

end
