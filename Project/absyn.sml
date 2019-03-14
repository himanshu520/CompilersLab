(* This file contains the abstract syntax tree for the Tiger Language *)

structure Absyn = struct

    datatype Exp = Nil
                 | Break
                 | Integer of int
                 | String of string
                 | Lvalue of lValue
                 | Negation of Exp
                 | ExpList of Exp list
                 | FunCall of string * (Exp list)
                 | BinOp of Exp * Operator * Exp
                 | Array of string * Exp * Exp
                 | Record of string * ((string * Exp) list)
                 | Assignment of lValue * Exp
                 | IfThenElse of Exp * Exp * Exp
                 | IfThen of Exp * Exp
                 | While of Exp * Exp
                 | For of string * Exp * Exp * Exp
                 | Let of (Dec list) * (Exp list)

    and   lValue = Id of string
                 | Subscript of lValue * Exp
                 | Field of lValue * string

    and Operator = Plus | Minus | Divide | Multiply |
                   Equals | NotEqual | Greater | Less |
                   GreaterEqual | LessEqual | And | Or
    
    and      Dec = TyDec of TypeDec
                 | VDec of VarDec
                 | FDec of FunDec

    and  TypeDec = TypeAssignment of string * string
                 | ArrayType of string * string
                 | RecordType of string * ((string * string) list)

    and   VarDec = Var of string * Exp
                 | VarType of string * string * Exp

    and   FunDec = Fun of string * ((string * string) list) * Exp
                 | FunType of string * ((string * string) list) * string * Exp;


    type Program = Exp;
end