signature SEMANT = sig
    val transProg : Absyn.exp -> unit
end

structure Semant = struct 
    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Types
    type venv = E.enventry
    type tenv = E.ty
    val nestOrd = ref 0;
    val error = ErrorMsg.error
    exception ErrMsg

    fun checkInt ({exp, ty}, pos) = ( case ty of Types.INT => ()
                                               | _ => error pos "integer value expected";
                                      exp )

    fun checkString ({exp, ty}, pos) = ( case ty of Types.STRING => ()
                                               | _ => error pos "string value expected";
                                      exp )

    fun actual_ty (T.NAME (sym, ty)) = ( case (!ty) of SOME t => actual_ty t
                                                     | NONE => raise ErrMsg )
    |   actual_ty ty = ty

    fun transExp (venv, tenv) = 
        let fun trexp (A.VarExp v) = trvar v
            |   trexp A.NilExp = { exp = (), ty = T.NIL }
            |   trexp (A.IntExp t) = { exp = (), ty = T.INT }
            |   trexp (A.StringExp (u, v)) = { exp = (), ty = T.STRING }
            |   trexp (A.CallExp { func, args, pos }) = ( case S.look (venv, func) of
                                                              SOME (E.FunEntry { formals , result }) => let val args' = map (fn t => #ty (trexp t)) args
                                                                                                            fun checkArgs ((x::xs) : T.ty list, (y::ys) : T.ty list) = if x = y then checkArgs (xs, ys)
                                                                                                                                                                       else (error pos "passed arguments do not match those in function defintion"; false)
                                                                                                            |   checkArgs (nil, nil) = true
                                                                                                            |   checkArgs _ = (error pos "passed arguments do not match those in function definition"; false)
                                                                                                        in checkArgs (args', formals); { exp = (), ty = actual_ty result } end
                                                            | _ => (error pos ("function '" ^ S.name func ^ "' not defined"); { exp = (), ty = T.UNIT } ) )
            |   trexp (A.OpExp { left, oper, right, pos }) = if foldl (fn (t, u) => u orelse (t = oper)) false [A.PlusOp, A.MinusOp, A.TimesOp, A.DivideOp] then ( checkInt (trexp left, pos);
                                                                                                                                                                   checkInt (trexp right, pos);
                                                                                                                                                                   { exp = (), ty = T.INT } )
                                                             else let val { exp = _, ty = leftty } = trexp left
                                                                  in ( if leftty = T.INT then checkInt (trexp right, pos)
                                                                       else if leftty = T.STRING then checkString (trexp right, pos) else error pos "invalid operands"; 
                                                                       { exp = (), ty = T.INT } )
                                                                  end
            |   trexp (A.RecordExp { fields, typ, pos }) = let fun fieldTy (sym, exp, pos) = (sym, #ty (trexp exp))
                                                               val fieldsTy = map fieldTy fields
                                                               fun findList lst (x : (S.symbol * T.ty), y) = if y orelse (List.find (fn t => t = x)  lst) <> NONE then true else false
                                                               fun checkListEqual (x, y) = (foldl (findList x) false y) andalso (foldl (findList y) false x)
                                                           in ( case S.look (tenv, typ) of SOME (T.RECORD (ty, u)) => if checkListEqual (fieldsTy, ty) then { exp = (), ty = T.RECORD (ty, u) }
                                                                                                                      else { exp = (), ty = T.UNIT }
                                                                                         | _ => ( error pos ("'" ^ S.name typ ^ "' is not a valid record type"); { exp = (), ty = T.UNIT } ) )
                                                           end
            |   trexp (A.SeqExp [(t, u)]) = trexp t
            |   trexp (A.SeqExp ((x, y) :: xs)) = ( trexp x; trexp (A.SeqExp xs) ) 
            |   trexp (A.SeqExp nil) = { exp = (), ty = T.UNIT }
            |   trexp (A.AssignExp { var, exp, pos }) = let val { exp = _, ty = varty } = trvar var
                                                            val { exp = _, ty = expty } = trexp exp
                                                        in ( if varty = expty then ()
                                                             else error pos "type of lvalue and rvalue does not match";
                                                             { exp = (), ty = T.UNIT } )
                                                        end
            |   trexp (A.IfExp { test, then', else', pos }) = ( case else' of SOME t => let val { exp = _, ty = thenty } = trexp then'
                                                                                            val { exp = _, ty = elsety } = trexp t
                                                                                            val { exp = _, ty = testty } = trexp test
                                                                                        in ( if testty <> T.INT then error pos "'if' test condition does not evaluate to an integer" else ();
                                                                                             if thenty <> elsety then error pos "types of 'then' and 'else' expressions of 'if' does not match" else ();
                                                                                             { exp = (), ty = thenty } )
                                                                                        end
                                                                            | _ => trexp (A.IfExp { test = test, then' = then', else' = SOME (A.SeqExp []), pos = pos }) )
            |   trexp (A.WhileExp { test, body, pos }) = let val { exp = _, ty = testty } = trexp test
                                                             val { exp = _, ty = bodyty } = ( nestOrd := (!nestOrd + 1); trexp body )
                                                         in ( if testty <> T.INT then error pos "test condition of 'while' does not evaluate to an integer" else ();
                                                              if bodyty <> T.UNIT then error pos "body of 'while' can not have a type" else ();
                                                              nestOrd := (!nestOrd) - 1;
                                                              { exp = (), ty = T.UNIT } )
                                                         end
            |   trexp (A.ForExp { var, escape, lo, hi, body, pos }) = let val venv' = S.enter (venv, var, E.VarEntry { ty = T.INT })
                                                                          val { exp = _, ty = loty } = trexp lo
                                                                          val { exp = _, ty = hity } = trexp hi
                                                                          val { exp = _, ty = bodyty } = ( nestOrd := (!nestOrd) + 1; transExp (venv', tenv) body)
                                                                      in ( if loty <> T.INT then error pos "low value of 'for' is not an integer" else ();
                                                                           if hity <> T.INT then error pos "high value of 'for' is not an integer" else ();
                                                                           if bodyty <> T.UNIT then error pos "body of 'for' can not have a type" else ();
                                                                           nestOrd := (!nestOrd) - 1;
                                                                           { exp = (), ty = T.UNIT } )
                                                                       end
            |   trexp (A.BreakExp pos) = ( if (!nestOrd) > 0 then () else error pos "'break' is not inside a 'for' or 'while' loop";
                                           { exp = (), ty = T.UNIT } )
            |   trexp (A.LetExp { decs, body, pos }) = let fun trdec (dec, { venv, tenv }) = transDec (venv, tenv, dec)
                                                           val { venv = venv', tenv = tenv' } = foldl trdec { venv = venv, tenv = tenv } decs
                                                       in transExp (venv', tenv') body end
            |   trexp (A.ArrayExp { typ, size, init, pos }) = ( checkInt (trexp size, pos);
                                                                case S.look (tenv, typ) of SOME (T.ARRAY (t, _)) => let val { exp = _, ty = initty } = trexp init
                                                                                                                    in ( if t = initty then ()
                                                                                                                         else (error pos "the initialisation expression for the array does not matches the array type");
                                                                                                                         { exp = (), ty = t } )
                                                                                                                    end
                                                                                         | _ => ( error pos ("'" ^ S.name typ ^ "' is not an array type"); { exp = (), ty = T.UNIT } ) )
            and trvar (A.SimpleVar (sym, pos)) = ( case S.look (venv, sym) of SOME (E.VarEntry { ty }) => { exp = (), ty = actual_ty ty }
                                                                            | _ => (error pos ("undefined variable '" ^ S.name sym ^ "'"); { exp = (), ty = Types.UNIT } ) )
            |   trvar (A.FieldVar (var, sym, pos)) = let val { exp = _, ty = varty } = trvar var
                                                         val symty = ref T.INT
                                                         fun findSymTy ((x, y) :: xs) = if x = sym then (symty := y; true)
                                                                                        else findSymTy xs
                                                         |   findSymTy _ = false
                                                     in ( case varty of T.RECORD (symtylst, _) => if findSymTy symtylst then { exp = (), ty = actual_ty (!symty) } 
                                                                                                  else ( error pos ("'" ^ S.name sym ^ "' is not a valid field"); { exp = (), ty = T.UNIT } )
                                                                      | _ => ( error pos ("invalid record variable"); { exp = (), ty = T.UNIT } ) )
                                                     end
            |   trvar (A.SubscriptVar (var, exp, pos)) = ( checkInt (trexp exp, pos);
                                                           let val { exp = _, ty = varty } = trvar var
                                                           in ( case varty of T.ARRAY (arrty, _) => { exp = (), ty = actual_ty arrty }
                                                                            | _ => ( error pos "invalid array variable"; { exp = (), ty = T.UNIT } ) )
                                                           end )

        in (trexp) end


    and transDec (venv, tenv, A.FunctionDec lst) = let fun checkFunRep ((x::xs) : A.fundec list, lst) = if List.find (fn t => (#name x) = t) lst = NONE then checkFunRep (xs, (#name x) :: lst) else (true, #pos x)
                                                       |   checkFunRep (_, _) = (false, 0)
                                                       val checkFunRepVal = checkFunRep (lst, [])
                                                   in if (#1 checkFunRepVal) then ( error (#2 checkFunRepVal) "function already declared"; { venv = venv, tenv = tenv} )
                                                      else let fun addFunName ({ name, params, result, body, pos }, env) = let val retty = ( case result of SOME (sym, pos) => ( case S.look (tenv, sym) of SOME t => t
                                                                                                                                                                                                          | _ => ( error pos ("invalid return type '" ^ S.name sym ^ "' for function '" ^ S.name name ^ "'"); T.UNIT ) )
                                                                                                                                                          | _ => T.UNIT )
                                                                                                                               fun fieldTy { name, escape, typ, pos } = case S.look (tenv, typ) of SOME t => t
                                                                                                                                                                                                 | _ => ( error pos ("invalid type '" ^ S.name typ ^ "' for function argument '" ^ S.name name ^ "'"); T.UNIT)
                                                                                                                               val paramTy = map fieldTy params
                                                                                                                           in S.enter (env, name, E.FunEntry { formals = paramTy, result = retty }) end
                                                               val venv' = foldl addFunName venv lst
                                                               fun completeFunName ({ name, params, result, body, pos }, env) = let val retty = ( case result of SOME (sym, pos) => ( case S.look (tenv, sym) of SOME t => t
                                                                                                                                                                                                               | _ => ( error pos ("invalid return type '" ^ S.name sym ^ "' for function '" ^ S.name name ^ "'"); T.UNIT ) )
                                                                                                                                                               | _ => T.UNIT )
                                                                                                                                    fun enterParam ({ name, escape, typ, pos }, env) = let val ty = ( case S.look (tenv, typ) of SOME t => t
                                                                                                                                                                                                                               | _ => ( error pos ("invalid return type '" ^ S.name typ ^ "' for function '" ^ S.name name ^ "'"); T.UNIT ) )
                                                                                                                                                                                       in S.enter (env, name, E.VarEntry { ty = ty }) end
                                                                                                                                    val venv'' = foldl enterParam venv' params
                                                                                                                                    val { exp = _, ty = funTy } = transExp (venv'', tenv) body
                                                                                                                                    fun fieldTy { name, escape, typ, pos } = case S.look (tenv, typ) of SOME t => t
                                                                                                                                                                                                      | _ => ( error pos ("invalid type '" ^ S.name typ ^ "' for function argument '" ^ S.name name ^ "'"); T.UNIT)
                                                                                                                                    val paramTy = map fieldTy params
                                                                                                                                in ( if funTy = retty then () else error pos ("function value and return type does not match for '" ^ S.name name ^ "'");
                                                                                                                                     S.enter (env, name, E.FunEntry { formals = paramTy, result = funTy }) )
                                                                                                                                end
                                                               val venv'' = foldl completeFunName venv lst
                                                           in { venv = venv'', tenv = tenv } end
                                                   end
    |   transDec (venv, tenv, A.VarDec { name, escape, typ, init, pos }) = ( case typ of SOME (sym, pos) => let val typty = S.look (tenv, sym)
                                                                                                                val initty = #ty (transExp (venv, tenv) init)
                                                                                                            in ( case typty of SOME t => if (actual_ty t) = initty then ()
                                                                                                                                         else error pos ("type '" ^ S.name sym ^ "' not matches the initialisation expression for variable '" ^ S.name name ^ "'")
                                                                                                                             | _ => error pos ("type '" ^ S.name sym ^ "' is not defined");
                                                                                                                 { tenv = tenv, venv = S.enter (venv, name, E.VarEntry { ty = initty }) } )
                                                                                                            end
                                                                                       | _ => let val initty = #ty (transExp (venv, tenv) init)
                                                                                              in { tenv = tenv, venv = S.enter (venv, name, E.VarEntry { ty = initty }) } end )
    |   transDec (venv, tenv, A.TypeDec lst) = let fun checkTypRep ((x::xs) : { name : S.symbol, ty : A.ty, pos : A.pos } list, lst) = if List.find (fn t => t = #name x) lst <> NONE then (true, #pos x) else checkTypRep (xs, (#name x) :: lst)
                                                   |   checkTypRep (_, _) = (false, 0)
                                                   val checkTypRepVal = checkTypRep (lst, [])
                                               in if #1 checkTypRepVal then ( error (#2 checkTypRepVal) "type already declared"; { venv = venv, tenv = tenv} )
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
                                                       in if #1 cycle then ( error (#2 cycle) "recursive type declaration not valid"; { venv = venv, tenv = tenv } )
                                                          else { venv = venv, tenv = tenv'' }
                                                       end
                                               end

    and transTy (tenv, A.NameTy (sym, pos)) = ( case S.look (tenv, sym) of SOME t => t
                                                                         | _ => ( error pos ("type '" ^ S.name sym ^ "' not defined"); T.UNIT ) )
    |   transTy (tenv, A.RecordTy lst) = let fun fieldTy { name, escape, typ, pos } = ( case S.look (tenv, typ) of SOME t => (name, t)
                                                                                                                 | _ => ( error pos ("type '" ^ S.name typ ^ "' not defined from here"); (name, T.UNIT) ) )
                                         in T.RECORD (map fieldTy lst, ref ()) end
    |   transTy (tenv, A.ArrayTy (sym, pos)) = ( case S.look (tenv, sym) of SOME t => T.ARRAY (t, ref ())
                                                                          | _ => ( error pos ("type '" ^ S.name sym ^ "' not defined"); T.ARRAY (T.UNIT, ref ()) ) )


    fun transProg exp = transExp (E.base_venv, E.base_tenv) exp
end