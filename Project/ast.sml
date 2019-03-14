(* The abstract syntax tree for expression *)
structure AST = struct

    datatype Exp = Nil
                 | Integer of int
                 | String of string
                 | Comment of string
                 | Lval of Lvalue
                 | Negation of Exp
                 | Exps of Exp list
                 | FunCall of string * (Exp list)
                 | App of Exp * Operator * Exp
                 | Array of string * Exp * Exp
                 | Record of string * ((string * Exp) list)
                 | Assignment of Lvalue * Exp
                 | IfThenElse of Exp * Exp * Exp
                 | IfThen of Exp * Exp
                 | While of Exp * Exp
                 | For of string * Exp * Exp * Exp
                 | Let of (Dec list) * (Exp list)

    and   Lvalue = Id of string
                 | Subscript of Lvalue * Exp
                 | Field of Lvalue * string

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


    type Program = Exp list;
end
