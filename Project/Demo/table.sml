(* This file contains functor that can be used to create a structure for a lookup table with a given key type *)

signature TABLE = sig
    type key
    type 'a table
    val empty : 'a table
    val enter : 'a table * key * 'a -> 'a table
    val look  : 'a table * key -> 'a option
    val listItemsi : 'a table -> (int * 'a) list
end

functor IntMapTable (type key val getInt : key -> int) : TABLE = struct
    type key = key
    type 'a table = 'a IntBinaryMap.map
    val empty = IntBinaryMap.empty
    fun enter (t, k, a) = IntBinaryMap.insert (t, getInt k, a)
    fun look (t, k) = IntBinaryMap.find (t, getInt k)
    val listItemsi = IntBinaryMap.listItemsi
end