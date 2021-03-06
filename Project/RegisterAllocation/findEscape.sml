(* This file contains function which recurse over the AST and find which variables can be escaped *)

signature FINDESCAPE = sig
    val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE = struct

    structure S = Symbol
    structure A = Absyn

    type depth = int
    type escEnv = (depth * bool ref) S.table

    fun traverseVar (env, d, s) = case s of A.SimpleVar (sym, pos) => ( case S.look (env, sym) of SOME (d', esc) => if d > d' then esc := true else () )
                                          | A.FieldVar (var, sym, pos) => traverseVar (env, d, var)
                                          | A.SubscriptVar (var, exp, pos) => traverseVar (env, d, var)

    and traverseExp (env, d, s) = case s of A.VarExp var => traverseVar (env, d, var)
                                          | A.NilExp => ()
                                          | A.IntExp _ => ()
                                          | A.StringExp (_, _) => ()
                                          | A.CallExp ({ args, ... }) => foldl (fn (arg, _) => traverseExp (env, d, arg)) () args
                                          | A.OpExp({ left, right, ... }) => (traverseExp (env, d, left); traverseExp (env, d, right); ())
                                          | A.RecordExp({ fields, ... }) => foldl (fn ((_, exp, _), _) => traverseExp (env, d, exp)) () fields
                                          | A.SeqExp exps => foldl (fn ((exp, _), _) => traverseExp (env, d, exp)) () exps
                                          | A.AssignExp ({ var, exp, ... }) => ( traverseVar (env, d, var); 
                                                                                 traverseExp (env, d, exp) )
                                          | A.IfExp ({ test, then', else', ... }) => ( traverseExp (env, d, test); 
                                                                                       traverseExp (env, d, then');
                                                                                       ( case else' of SOME (else'') => traverseExp (env, d, else'')
                                                                                                     | NONE => () ) )
                                          | A.WhileExp({ test, body, ... }) => ( traverseExp (env, d, test); traverseExp (env, d, body) )
                                          | A.ForExp({ var, escape, lo, hi, body, pos }) => let val new_env = S.enter (env, var, (d, escape)) 
                                                                                            in ( escape := false;
                                                                                                 traverseExp (env, d, lo);
                                                                                                 traverseExp (env, d, hi);
                                                                                                 traverseExp (new_env, d, body) )
                                                                                            end
                                          | A.BreakExp _ => ()
                                          | A.LetExp ({ decs, body, ... }) => let val new_env = traverseDecs (env, d, decs) 
                                                                              in traverseExp (new_env, d, body) end
                                          | A.ArrayExp ({ size, init, ...}) => ( traverseExp (env, d, size); 
                                                                                 traverseExp (env, d, init) )

    and traverseDecs (env, d, s) = let fun travParam ({ name, escape, ... } : A.field, env) = ( escape := false; S.enter (env, name, (d + 1, escape)) )
                                       fun travFun ({ params, body, ... } : A.fundec, env) = ( traverseExp (foldl travParam env params, d + 1, body); env )
                                       fun do_dec (dec, env) = case dec of A.FunctionDec fundecs => foldl travFun env fundecs
                                                                         | A.VarDec ({ name, escape, init, ... }) => ( escape := false;
                                                                                                                       traverseExp (env, d, init);
                                                                                                                       S.enter (env, name, (d, escape)) )
                                                                         | A.TypeDec _ => env
                                   in foldl do_dec env s end

    (* Local variable that do not escape can be allocated in a register, escaping variables must be allocated in the frame *)
    fun findEscape prog = let val tbl = S.empty
                          in traverseExp (tbl, 0, prog) end
end