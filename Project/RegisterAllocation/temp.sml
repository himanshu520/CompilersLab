(* This file contains implementation to represent register and labels in the Tree IR *)

signature TEMP = sig
    eqtype temp
    val newtemp : unit -> temp
    structure Table : TABLE sharing type Table.key = temp
    val makestring: temp -> string

    type label = Symbol.symbol
    val newlabel : unit -> label
    val namedlabel : string -> label
end

structure Temp : TEMP = struct
    type temp = int
    val temps = ref 0
    fun incRef refVar = let val tmp = !refVar in ( refVar := tmp + 1; tmp ) end
    fun newtemp () = incRef temps
    structure Table = IntMapTable (type key = int fun getInt n = n)
    fun makestring t = "temp" ^ (Int.toString t)

    type label = Symbol.symbol
    val labels = ref 0
    fun newlabel () = Symbol.symbol ("L" ^ Int.toString (incRef labels) )
    val namedlabel = Symbol.symbol

end
