(* The abstract syntax tree for expression *)
structure Ast = struct

datatype Factor = V of string | Const of int
datatype Term = F of Factor | P of Factor * Term
datatype Expr = T of Term | S of Term * Expr
type Stmt = string * Expr
type Program = Stmt list

end
