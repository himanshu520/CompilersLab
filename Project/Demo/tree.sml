(* This file contains abstract syntax tree for the Intermediate Representation *)

structure Tree = struct
    type size = int
    type label = Temp.label

    datatype exp = CONST of int
                 | NAME of Temp.label
                 | TEMP of Temp.temp
                 | BINOP of binop * exp * exp
                 | MEM of exp
                 | CALL of exp * exp list
                 | ESEQ of stm * exp

         and stm = MOVE of exp * exp
                 | EXP of exp
                 | JUMP of exp * Temp.label list
                 | CJUMP of relop * exp * exp * Temp.label * Temp.label
                 | SEQ of stm * stm
                 | LABEL of Temp.label

         and binop = PLUS | MINUS | MUL | DIV
                   | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

         and relop = EQ | NE | LT | GT | LE | GE
                   | ULT | ULE | UGT | UGE

    fun notRel oper = ( case oper of EQ => NE
                                   | NE => EQ
                                   | LT => GE
                                   | GT => LE
                                   | LE => GT
                                   | GE => LT
                                   | ULT => UGE
                                   | UGT => ULE
                                   | ULE => UGT
                                   | UGE => ULT )
end