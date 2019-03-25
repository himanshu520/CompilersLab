signature SEMANT = sig
    type venv
    type tenv
    type expty
    val transVar : venv * tenv * Absyn.var -> expty
    val transExp : venv * tenv * Absyn.exp -> expty
    val transDec : venv * tenv * Absyn.dec -> { venv : venv, tenv : tenv }
    val transTy : tenv * Absyn.ty -> Types.ty
end

structure Semant = struct 
    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Types
    type venv = E.enventry
    type tenv = E.ty
    type expty = { exp : Translate.exp, ty : Types.ty }
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
                                                     | NONE => raise ErrMsg)
    |   actual_ty ty = ty

    fun transExp (venv, tenv) = 
        let fun trexp (A.VarExp v) = trvar v
            |   trexp A.NilExp = { exp = (), ty = T.NIL }
            |   trexp (A.IntExp t) = { exp = (), ty = T.INT }
            |   trexp (A.StringExp (u, v)) = { exp = (), ty = T.STRING }
            |   trexp (A.CallExp { func, args, pos }) = ( case S.look (venv, func) of
                                                              SOME (E.FunEntry { formals , result }) => let val args' = map (fn t => #ty (trexp t)) args
                                                                                                            fun checkArgs (x::xs, y::ys) = if x = y then checkArgs (xs, ys)
                                                                                                                                         else (error pos "passed arguments do not match those in function defintion"; false)
                                                                                                            |   checkArgs (nil, nil) = true
                                                                                                            |   checkArgs _ = (error pos "passed arguments do not match those in function definition"; false)
                                                                                                        in checkArgs (args', formals); { exp = (), ty = actual_ty result } end
                                                            | NONE => (error pos ("function '" ^ S.name func ^ "' not defined"); { exp = (), ty = T.UNIT } ) )
            |   trexp (A.OpExp { left, oper, right, pos }) = if foldl (fn (t, u) => u orelse (t = oper)) false [A.PlusOp, A.MinusOp, A.TimesOp, A.DivideOp] then ( checkInt (trexp left, pos);
                                                                                                                                                                     checkInt (trexp right, pos);
                                                                                                                                                                     { exp = (), ty = T.INT } )
                                                             else let val { exp = _, ty = leftty } = trexp left
                                                                  in ( if leftty = T.INT then checkInt (trexp right, pos)
                                                                       else checkString (trexp right, pos); 
                                                                       { exp = (), ty = T.INT } )
                                                                  end
            |   trexp (A.RecordExp { fields, typ, pos }) = ( { exp = (), ty = T.INT })
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
                                                                                        in ( if testty <> T.INT then error pos "test condition does not evaluate to an integer" else ();
                                                                                             if thenty <> elsety then error pos "types of 'then' and 'else' conditions does not match" else ();
                                                                                             { exp = (), ty = thenty } )
                                                                                        end
                                                                            | _ => trexp (A.IfExp { test = test, then' = then', else' = SOME (A.SeqExp []), pos = pos }) )
            |   trexp (A.WhileExp { test, body, pos }) = let val { exp = _, ty = testty } = trexp test
                                                             val { exp = _, ty = bodyty } = ( nestOrd := (!nestOrd + 1); trexp body )
                                                         in ( if testty <> T.INT then error pos "test condition does not evaluate to an integer" else ();
                                                              if bodyty <> T.UNIT then error pos "body of 'while' can not have a type" else ();
                                                              nestOrd := (!nestOrd) - 1;
                                                              { exp = (), ty = T.UNIT } )
                                                         end
            |   trexp (A.ForExp { var, escape, lo, hi, body, pos }) = let val venv' = S.enter (venv, var, E.VarEntry { ty = T.INT })
                                                                          val { exp = _, ty = loty } = trexp lo
                                                                          val { exp = _, ty = hity } = trexp hi
                                                                          val { exp = _, ty = bodyty } = ( nestOrd := (!nestOrd) + 1; transExp (venv', tenv) body)
                                                                      in ( if loty <> T.INT then error pos "lo value of 'for' is not an integer" else ();
                                                                           if hity <> T.INT then error pos "high value of 'for' is not an integer" else ();
                                                                           if bodyty <> T.UNIT then error pos "body of 'while' can not have a type" else ();
                                                                           nestOrd := (!nestOrd) - 1;
                                                                           { exp = (), ty = T.UNIT } )
                                                                       end
            |   trexp (A.BreakExp pos) = ( if (!nestOrd) > 0 then () else error pos "'break' is not inside a 'for' or 'while' loop";
                                           { exp = (), ty = T.UNIT } )
            |   trexp (A.LetExp { decs, body, pos }) = let val { venv = venv', tenv = tenv' } = transDec (venv, tenv, decs)
                                                       in transExp (venv', tenv') body end
            |   trexp (A.ArrayExp { typ, size, init, pos }) = ( checkInt (trexp size, pos);
                                                                case S.look (tenv, typ) of SOME (T.ARRAY (t, _)) => let val { exp = _, ty = initty } = trexp init
                                                                                                                    in ( if t = initty then ()
                                                                                                                         else (error pos "the initialisation expression for the array does not matches the array type");
                                                                                                                         { exp = (), ty = t } )
                                                                                                                    end
                                                                                         | _ => ( error pos ("'" ^ S.name typ ^ "' is not an array type"); { exp = (), ty = T.UNIT } ) )
            and trvar (A.SimpleVar (sym, pos)) = ( case S.look (venv, sym) of SOME (E.VarEntry { ty }) => { exp = (), ty = actual_ty ty }
                                                                            | NONE => (error pos ("undefined variable '" ^ S.name sym ^ "'"); { exp = (), ty = Types.UNIT } ) )
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

        in trexp end


    (* and fun TransDec (venv, tenv, dec) = { venv = venv, tenv = tenv } *)

end