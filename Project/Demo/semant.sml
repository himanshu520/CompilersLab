(* This file contains the structure implementing the semantic analyser for a tiger Program *)

signature SEMANT = sig
    val transProg : Absyn.exp -> Translate.frag list
end

structure Semant : SEMANT = struct 
    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Types
    structure Tr = Translate
    type venv = E.enventry
    type tenv = E.ty
    val nestOrd = ref 0;
    val error = ErrorMsg.error
    exception ErrMsg

    fun checkInt ({ exp, ty }, pos) = ( case ty of Types.INT => ()
                                               | _ => error pos "integer value expected";
                                        exp )

    fun checkString ({ exp, ty }, pos) = ( case ty of Types.STRING => ()
                                               | _ => error pos "string value expected";
                                           exp )

    fun actual_ty (T.NAME (sym, ty)) = ( case (!ty) of SOME t => actual_ty t
                                                     | NONE => raise ErrMsg )
    |   actual_ty (T.ARRAY (ty, unq)) = T.ARRAY (actual_ty ty, unq)
    |   actual_ty ty = ty

    datatype opType = ARITHMETIC of Tree.binop | RELATIONAL of Tree.relop
    
    fun treeOp A.PlusOp = ARITHMETIC Tree.PLUS
    |   treeOp A.MinusOp = ARITHMETIC Tree.MINUS
    |   treeOp A.TimesOp = ARITHMETIC Tree.MUL
    |   treeOp A.DivideOp = ARITHMETIC Tree.DIV
    |   treeOp A.EqOp = RELATIONAL Tree.EQ
    |   treeOp A.NeqOp = RELATIONAL Tree.NE
    |   treeOp A.LeOp = RELATIONAL Tree.LE
    |   treeOp A.GeOp = RELATIONAL Tree.GE
    |   treeOp A.LtOp = RELATIONAL Tree.LT
    |   treeOp A.GtOp = RELATIONAL Tree.GT

    fun transExp (venv, tenv, level, break) = 
        let fun trexp (A.VarExp v) = trvar v
            |   trexp A.NilExp = { exp = Tr.nilexp, ty = T.NIL }
            |   trexp (A.IntExp t) = { exp = Tr.intlit t, ty = T.INT }
            |   trexp (A.StringExp (u, v)) = { exp = Tr.strlit u, ty = T.STRING }

            |   trexp (A.CallExp { func, args, pos }) = ( case S.look (venv, func) of
                                                              SOME (E.FunEntry { level = funlevel, label, formals , result }) => let val args' = map (fn t => trexp t) args
                                                                                                                                     val argsty = map (fn { exp, ty } => ty) args'
                                                                                                                                     val argsexp = map (fn { exp, ty } => exp) args'
                                                                                                                                     fun checkArgs ((x::xs) : T.ty list, (y::ys) : T.ty list) = if x = y then checkArgs (xs, ys)
                                                                                                                                                                                     else (error pos "passed arguments do not match those in function defintion"; false)
                                                                                                                                     |   checkArgs (nil, nil) = true
                                                                                                                                     |   checkArgs _ = (error pos "passed arguments do not match those in function definition"; false)
                                                                                                                                     val isProc = result = T.UNIT
                                                                                                                                 in checkArgs (argsty, formals); { exp = Tr.callExp (level, funlevel, label, argsexp, isProc), ty = actual_ty result } end
                                                            | _ => (error pos ("function '" ^ S.name func ^ "' not defined"); { exp = Tr.nilexp, ty = T.UNIT } ) )

            |   trexp (A.OpExp { left, oper, right, pos }) = ( case treeOp oper of ARITHMETIC _ => let val lexp = checkInt (trexp left, pos)
                                                                                                       val rexp = checkInt (trexp right, pos);
                                                                                                    in { exp = Tr.binop (oper, lexp, rexp), ty = T.INT } end
                                                                                 | _ => let val { exp = lexp, ty = leftty } = trexp left
                                                                                            val rexp = ( if leftty = T.INT then checkInt (trexp right, pos)
                                                                                                         else if leftty = T.STRING then checkString (trexp right, pos)
                                                                                                         else (error pos "invalid operands"; lexp) )
                                                                                        in { exp = Tr.relop (oper, lexp, rexp), ty = T.INT } end )

            |   trexp (A.RecordExp { fields, typ, pos }) = let val fieldExpTy = map (fn (sym, exp, pos) => (sym, trexp exp)) fields
                                                               val fieldsExp = map (fn (x, {exp, ty}) => exp) fieldExpTy
                                                               val fieldsTy = map (fn (x, {exp, ty}) => (x, ty)) fieldExpTy
                                                               fun findList lst (x : (S.symbol * T.ty), y) = if y orelse (List.find (fn t => t = x)  lst) <> NONE then true else false
                                                               fun checkListEqual (x, y) = (foldl (findList x) false y) andalso (foldl (findList y) false x)
                                                           in ( case S.look (tenv, typ) of SOME (T.RECORD (ty, u)) => if checkListEqual (fieldsTy, ty) then { exp = Tr.recordExp fieldsExp, ty = T.RECORD (ty, u) }
                                                                                                                      else { exp = Tr.nilexp, ty = T.UNIT }
                                                                                         | _ => ( error pos ("'" ^ S.name typ ^ "' is not a valid record type"); { exp = Tr.nilexp, ty = T.UNIT } ) )
                                                           end
            |   trexp (A.SeqExp exps) = let val trExps = map (fn (exp, pos) => #exp (trexp exp)) exps
                                            val ty = if List.null exps then T.UNIT
                                                     else #ty (trexp (#1 (List.last exps)))
                                        in { exp = Tr.seqExp trExps, ty = ty } end
            |   trexp (A.AssignExp { var, exp, pos }) = let val { exp = lexp, ty = varty } = trvar var
                                                            val { exp = rexp, ty = expty } = trexp exp
                                                        in ( if varty = expty then ()
                                                             else error pos "type of lvalue and rvalue does not match";
                                                             { exp = Tr.assignExp (lexp, rexp), ty = T.UNIT } )
                                                        end
            |   trexp (A.IfExp { test, then', else', pos }) = ( case else' of SOME t => let val { exp = thenexp, ty = thenty } = trexp then'
                                                                                            val { exp = elseexp, ty = elsety } = trexp t
                                                                                            val { exp = testexp, ty = testty } = trexp test
                                                                                        in ( if testty <> T.INT then error pos "'if' test condition does not evaluate to an integer" else ();
                                                                                             if thenty <> elsety then error pos "types of 'then' and 'else' expressions of 'if' does not match" else ();
                                                                                             { exp = Tr.ifExp (testexp, thenexp, SOME elseexp), ty = thenty } )
                                                                                        end
                                                                            | _ => let val { exp = thenexp, ty = thenty } = trexp then'
                                                                                       val { exp = testexp, ty = testty } = trexp test
                                                                                   in ( if testty <> T.INT then error pos "'if' test condition does not evaluate to an integer" else ();
                                                                                        if thenty <> T.UNIT then error pos "'then' can not have a value" else ();
                                                                                        { exp = Tr.ifExp (testexp, thenexp, NONE), ty = thenty } )
                                                                                   end )
            |   trexp (A.WhileExp { test, body, pos }) = let val finishlabel = Temp.newlabel ()
                                                             val { exp = testexp, ty = testty } = trexp test
                                                             val { exp = bodyexp, ty = bodyty } = ( nestOrd := (!nestOrd + 1); trexp body )
                                                         in ( if testty <> T.INT then error pos "test condition of 'while' does not evaluate to an integer" else ();
                                                              if bodyty <> T.UNIT then error pos "body of 'while' can not have a type" else ();
                                                              nestOrd := (!nestOrd) - 1;
                                                              { exp = Tr.whileExp (testexp, bodyexp, finishlabel), ty = T.UNIT } )
                                                         end
            |   trexp (A.ForExp { var, escape, lo, hi, body, pos }) = let val finishlabel = Temp.newlabel ()
                                                                          val var' = Tr.allocLocal level (!escape)
                                                                          val venv' = S.enter (venv, var, E.VarEntry { access = var', ty = T.INT })
                                                                          val { exp = loexp, ty = loty } = trexp lo
                                                                          val { exp = hiexp, ty = hity } = trexp hi
                                                                          val { exp = bodyexp, ty = bodyty } = ( nestOrd := (!nestOrd) + 1; transExp (venv', tenv, level, finishlabel) body )
                                                                      in ( if loty <> T.INT then error pos "low value of 'for' is not an integer" else ();
                                                                           if hity <> T.INT then error pos "high value of 'for' is not an integer" else ();
                                                                           if bodyty <> T.UNIT then error pos "body of 'for' can not have a type" else ();
                                                                           nestOrd := (!nestOrd) - 1;
                                                                           { exp = Tr.forExp (var', loexp, hiexp, bodyexp, finishlabel, level), ty = T.UNIT } )
                                                                      end
            |   trexp (A.BreakExp pos) = ( if (!nestOrd) > 0 then () else error pos "'break' is not inside a 'for' or 'while' loop";
                                           { exp = Tr.breakExp break, ty = T.UNIT } )
            |   trexp (A.LetExp { decs, body, pos }) = let fun trdec (dec, { venv, tenv, exps }) = let val { venv = venv', tenv = tenv', exps = exps' } = transDec (venv, tenv, level, dec, break)
                                                                                                   in { venv = venv', tenv = tenv', exps = exps @ exps' } end
                                                           val { venv = venv', tenv = tenv', exps = exps' } = foldl trdec { venv = venv, tenv = tenv, exps = [] } decs
                                                           val { exp = bodyexp, ty = bodyty } = transExp (venv', tenv', level, break) body
                                                       in { exp = Tr.letExp (exps', bodyexp), ty = bodyty } end
            |   trexp (A.ArrayExp { typ, size, init, pos }) = ( case S.look (tenv, typ) of SOME (T.ARRAY (t, _)) => let val sizeexp = checkInt (trexp size, pos);
                                                                                                                        val { exp = initexp, ty = initty } = trexp init
                                                                                                                    in ( if t = initty then ()
                                                                                                                         else (error pos "the initialisation expression for the array does not matches the array type");
                                                                                                                         { exp = Tr.arrayExp (sizeexp, initexp), ty = t } )
                                                                                                                    end
                                                                                         | _ => ( error pos ("'" ^ S.name typ ^ "' is not an array type"); { exp = Tr.nilexp, ty = T.UNIT } ) )
            and trvar (A.SimpleVar (sym, pos)) = ( case S.look (venv, sym) of SOME (E.VarEntry { access, ty }) => { exp = Tr.simpleVar (access, level), ty = actual_ty ty }
                                                                            | _ => (error pos ("undefined variable '" ^ S.name sym ^ "'"); { exp = Tr.nilexp, ty = Types.UNIT } ) )
            |   trvar (A.FieldVar (var, sym, pos)) = let val { exp = varexp, ty = varty } = trvar var
                                                         val symty = ref T.INT
                                                         fun findSymTy ((x, y) :: xs) = if x = sym then (symty := y; true)
                                                                                        else findSymTy xs
                                                         |   findSymTy _ = false
                                                     in ( case varty of T.RECORD (symtylst, _) => if findSymTy symtylst then { exp = Tr.fieldVar (varexp, sym, map #1 symtylst), ty = actual_ty (!symty) }
                                                                                                  else ( error pos ("'" ^ S.name sym ^ "' is not a valid field"); { exp = Tr.nilexp, ty = T.UNIT } )
                                                                      | _ => ( error pos ("invalid record variable"); { exp = Tr.nilexp, ty = T.UNIT } ) )
                                                     end
            |   trvar (A.SubscriptVar (var, exp, pos)) = let val exp' = checkInt (trexp exp, pos)
                                                             val { exp = varexp, ty = varty } = trvar var
                                                         in ( case varty of T.ARRAY (arrty, _) => { exp = Tr.subscriptVar (varexp, exp'), ty = actual_ty arrty }
                                                                          | _ => ( error pos "invalid array variable"; { exp = Tr.nilexp, ty = T.UNIT } ) )
                                                         end

        in (trexp) end


    and transDec (venv, tenv, level, A.VarDec { name, escape, typ, init, pos }, break) = ( case typ of SOME (sym, pos) => let val typty = S.look (tenv, sym)
                                                                                                                              val init' = transExp (venv, tenv, level, break) init
                                                                                                                              val initty = #ty init'
                                                                                                                              val initexp = #exp init'
                                                                                                                              val acc = Tr.allocLocal level (!escape)
                                                                                                                              val varexp = Tr.simpleVar (acc, level)
                                                                                                                          in ( case typty of SOME t => if (actual_ty t) = initty then ()
                                                                                                                                                       else error pos ("type '" ^ S.name sym ^ "' not matches the initialisation expression for variable '" ^ S.name name ^ "'")
                                                                                                                                           | _ => error pos ("type '" ^ S.name sym ^ "' is not defined");
                                                                                                                               { venv = S.enter (venv, sym, E.VarEntry { access = acc, ty = initty } ), tenv = tenv, exps = [Tr.assignExp (varexp, initexp)] } )
                                                                                                                          end
                                                                                                     | _ => let val init' = transExp (venv, tenv, level, break) init
                                                                                                                val acc = Tr.allocLocal level (!escape)
                                                                                                                val varexp = Tr.simpleVar (acc, level)
                                                                                                            in { venv = S.enter (venv, name, E.VarEntry { access = acc, ty = #ty init' }), tenv = tenv, exps = [Tr.assignExp (varexp, #exp init')] } end )

    |   transDec (venv, tenv, level, A.FunctionDec lst, break) = let fun checkFunRep ((x::xs) : A.fundec list, lst) = if List.find (fn t => (#name x) = t) lst = NONE then checkFunRep (xs, (#name x) :: lst) else (true, #pos x)
                                                                     |   checkFunRep (_, _) = (false, 0)
                                                                     val checkFunRepVal = checkFunRep (lst, [])
                                                                 in if (#1 checkFunRepVal) then ( error (#2 checkFunRepVal) "function already declared"; { venv = venv, tenv = tenv, exps = [] } )
                                                                    else let fun addFunName ({ name, params, result, body, pos }, env) = let val retty = ( case result of SOME (sym, pos) => ( case S.look (tenv, sym) of SOME t => t
                                                                                                                                                                                                                        | _ => ( error pos ("invalid return type '" ^ S.name sym ^ "' for function '" ^ S.name name ^ "'"); T.UNIT ) )
                                                                                                                                                                        | _ => T.UNIT )
                                                                                                                                             fun fieldTy { name, escape, typ, pos } = case S.look (tenv, typ) of SOME t => t
                                                                                                                                                                                                               | _ => ( error pos ("invalid type '" ^ S.name typ ^ "' for function argument '" ^ S.name name ^ "'"); T.UNIT)
                                                                                                                                             val paramTy = map fieldTy params
                                                                                                                                             val escapes = map (fn { name, escape, typ, pos } => !escape) params
                                                                                                                                         in S.enter (env, name, E.FunEntry { level = Translate.newLevel { parent = level, name = name, formals = escapes }, label = name, formals = paramTy, result = retty }) end
                                                                             val venv' = foldl addFunName venv lst
                                                                             fun completeFunName ({ name, params, result, body, pos }, env) = let val SOME (E.FunEntry { result, level = newlevel, ... }) = S.look (venv', name)
                                                                                                                                                  val retty = result
                                                                                                                                                  fun enterParam ({ name, escape, typ, pos }, access) = ( case S.look (tenv, typ) of SOME t => { access = access, name = name, ty = t }
                                                                                                                                                                                                                                   | _ => ( error pos ("invalid parameter type '" ^ S.name typ ^ "' for parameter '" ^ S.name name ^ "'"); 
                                                                                                                                                                                                                                            { access = access, name = name, ty = T.UNIT } ) )
                                                                                                                                                  val params' = ListPair.map enterParam (params, Tr.formals newlevel)
                                                                                                                                                  val venv'' = foldl (fn ({ access, name, ty }, env) => S.enter (env, name, E.VarEntry { access = access, ty = ty })) venv' params'
                                                                                                                                                  val { exp = bodyexp, ty = funTy } = transExp (venv'', tenv, newlevel, break) body
                                                                                                                                                  fun fieldTy { name, escape, typ, pos } = case S.look (tenv, typ) of SOME t => t
                                                                                                                                                                                                                           | _ => ( error pos ("invalid type '" ^ S.name typ ^ "' for function argument '" ^ S.name name ^ "'"); T.UNIT)
                                                                                                                                                  val paramTy = map fieldTy params
                                                                                                                                              in ( if funTy = retty then () else error pos ("function value and return type does not match for '" ^ S.name name ^ "'");
                                                                                                                                                   Tr.procEntryExit (newlevel, bodyexp);
                                                                                                                                                   { venv = venv', tenv = tenv } )
                                                                                                                                              end
                                                                             val { venv, tenv } = foldl completeFunName { venv = venv, tenv = tenv } lst
                                                                         in { venv = venv, tenv = tenv, exps = [] } end
                                                                 end

    |   transDec (venv, tenv, level, A.TypeDec lst, break) = let fun checkTypRep ((x::xs) : { name : S.symbol, ty : A.ty, pos : A.pos } list, lst) = if List.find (fn t => t = #name x) lst <> NONE then (true, #pos x) else checkTypRep (xs, (#name x) :: lst)
                                                                 |   checkTypRep (_, _) = (false, 0)
                                                                 val checkTypRepVal = checkTypRep (lst, [])
                                                             in if #1 checkTypRepVal then ( error (#2 checkTypRepVal) "type already declared"; { venv = venv, tenv = tenv, exps = [] } )
                                                                else let fun addTypeName ({ name, ty, pos }, env) = S.enter (env, name, T.NAME (name, ref NONE))
                                                                         val tenv' = foldl addTypeName tenv lst
                                                                         fun completeTypeName ({ name, ty, pos }, env) = S.enter (env, name, transTy (tenv', ty))
                                                                         val tenv'' = foldl completeTypeName tenv lst
                                                                         fun validateTy ({ name, ty, pos }, cnt) = ( case S.look (tenv'', name) of NONE => ( error pos ("type '" ^ S.name name ^ "' not valid"); false )
                                                                                                                                                 | SOME (T.NAME (nm, tyref)) => if nm = name andalso cnt = 0 then false
                                                                                                                                                                                else ( case !tyref of SOME t => validateTy ({ name = nm, ty = ty, pos = pos }, 0)
                                                                                                                                                                                                    | _ => false )
                                                                                                                                                 | _ => true )
                                                                         fun checkCyclicTyDec (x :: xs) = if validateTy (x, 1) then checkCyclicTyDec xs else (true, #pos x)
                                                                         |   checkCyclicTyDec _ = (false, 0)
                                                                         val cycle = checkCyclicTyDec lst
                                                                     in if #1 cycle then ( error (#2 cycle) "recursive type declaration not valid"; { venv = venv, tenv = tenv, exps = [] } )
                                                                        else { venv = venv, tenv = tenv'', exps = [] }
                                                                     end
                                                             end

    and transTy (tenv, A.NameTy (sym, pos)) = ( case S.look (tenv, sym) of SOME t => t
                                                                         | _ => ( error pos ("type '" ^ S.name sym ^ "' not defined"); T.UNIT ) )
    |   transTy (tenv, A.RecordTy lst) = let fun fieldTy { name, escape, typ, pos } = ( case S.look (tenv, typ) of SOME t => (name, t)
                                                                                                                 | _ => ( error pos ("type '" ^ S.name typ ^ "' not defined from here"); (name, T.UNIT) ) )
                                         in T.RECORD (map fieldTy lst, ref ()) end
    |   transTy (tenv, A.ArrayTy (sym, pos)) = ( case S.look (tenv, sym) of SOME t => T.ARRAY (t, ref ())
                                                                          | _ => ( error pos ("type '" ^ S.name sym ^ "' not defined"); T.ARRAY (T.UNIT, ref ()) ) )


    fun transProg exp = let val _ = Tr.reset ()
                            val mainlevel = Tr.newLevel { parent = Tr.outermost, name = Temp.namedlabel "main", formals = [] }
                            val { exp, ty } = transExp (E.base_venv, E.base_tenv, mainlevel, Temp.newlabel ()) exp
                        in ( Tr.procEntryExit (mainlevel, exp); Tr.getResult() ) end
end