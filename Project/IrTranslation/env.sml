(* This file defines the Env structure that contains the base environment for the types and function, variable environements of Tiger *)

signature ENV = sig
    type access
    type ty
    datatype enventry = VarEntry of { ty : ty }
                      | FunEntry of { formals : ty list, result : ty }
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end

structure Env : ENV = struct 
    structure S = Symbol
    structure T = Types
    type access = unit
    type ty = T.ty
    datatype enventry = VarEntry of { ty : ty }
                      | FunEntry of { formals : ty list, result : ty }

    fun insertTable ((sym, ty), tbl) = S.enter (tbl, sym, ty)
    val baseTypes = [(S.symbol "int", T.INT), (S.symbol "string", T.STRING)]
    val preDefFuns = [ (S.symbol "print", FunEntry { formals = [T.STRING], result = T.UNIT }),
                       (S.symbol "flush", FunEntry { formals = [], result = T.UNIT }),
                       (S.symbol "getchar", FunEntry { formals = [], result = T.STRING }),
                       (S.symbol "ord", FunEntry { formals = [T.STRING], result = T.INT }),
                       (S.symbol "chr", FunEntry { formals = [T.INT], result = T.STRING }),
                       (S.symbol "size", FunEntry { formals = [T.STRING], result = T.INT }),
                       (S.symbol "substring", FunEntry { formals = [T.STRING, T.INT, T.INT], result = T.STRING }),
                       (S.symbol "concat", FunEntry { formals = [T.STRING, T.STRING], result = T.STRING }),
                       (S.symbol "not", FunEntry { formals = [T.INT], result = T.INT }),
                       (S.symbol "exit", FunEntry { formals = [T.INT], result = T.UNIT })]

    (* base_tenv is the symbol table containing base Tiger types *)
    val base_tenv : ty S.table = foldl insertTable S.empty baseTypes

    (* base_venv is the symbol table containing predefined tiger functions *)
    val base_venv : enventry S.table = foldl insertTable S.empty preDefFuns
end