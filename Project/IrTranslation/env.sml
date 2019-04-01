(* This file defines the Env structure that contains the base environment for the types and function, variable environements of Tiger *)

signature ENV = sig
    type access
    type ty
    datatype enventry = VarEntry of { access : Translate.access, ty : ty }
                      | FunEntry of { level : Translate.level, label : Temp.label, formals : ty list, result : ty }
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end

structure Env : ENV = struct 
    structure S = Symbol
    structure T = Types
    structure Tr = Translate
    type access = Tr.access
    type ty = T.ty
    datatype enventry = VarEntry of { access : access, ty : ty }
                      | FunEntry of { level : Tr.level, label : Temp.label, formals : ty list, result : ty }

    fun insertTable ((sym, ty), tbl) = S.enter (tbl, S.symbol sym, ty)
    val baseTypes = [("int", T.INT), ("string", T.STRING)]
    val preDefFuns = [ ("print", [T.STRING], T.UNIT ),
                       ("flush", [], T.UNIT ),
                       ("getchar", [], T.STRING ),
                       ("ord", [T.STRING], T.INT ),
                       ("chr", [T.INT], T.STRING ),
                       ("size", [T.STRING], T.INT ),
                       ("substring", [T.STRING, T.INT, T.INT], T.STRING ),
                       ("concat", [T.STRING, T.STRING], T.STRING ),
                       ("not", [T.INT], T.INT ),
                       ("exit", [T.INT], T.UNIT )]

    (* base_tenv is the symbol table containing base Tiger types *)
    val base_tenv : ty S.table = foldl insertTable S.empty baseTypes

    (* base_venv is the symbol table containing predefined tiger functions *)
    val base_venv : enventry S.table = let fun expandFun ((x, y, z), env) = S.enter (env, S.symbol x, FunEntry { level = Tr.newLevel { parent = Tr.outermost, name = S.symbol x, formals = map (fn _ => false) y }, 
                                                                                                                 label = Temp.namedlabel x, formals = y, result = z })
                                       in foldl expandFun S.empty preDefFuns end
end