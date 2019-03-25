signature SEMANT = sig
    type venv
    type tenv
    type expty
    val transVar : venv * tenv * Absyn.var -> expty
    val transExp : venv * tenv * Absyn.exp -> expty
    val transDec : venv * tenv * Absyn.dec -> { venv : venv, tenv : tevn }
    val transTy : tenv * Absyn.ty -> Types.ty
end

structure Semant = struct 
    structure A = Absyn
    structure S = Symbol
    structure T = Types
    type venv = Env.enventry
    type tenv = Env.ty
    type expty = { exp : Translate.exp, ty : Types.ty }
    val nestOrd = ref 0;
    exception ErrMsg

    fun checkInt ({exp, ty}, pos) = ( case ty of Types.INT => ()
                                               | _ => error pos "integer value expected";
                                      exp )

    fun checkInt ({exp, ty}, pos) = ( case ty of Types.STRING => ()
                                               | _ => error pos "string value expected";
                                      exp )

    fun actual_ty (Types.Name (sym, ty)) = ( case (!ty) of SOME t => actual_ty t
                                                         | NONE => raise ErrMsg)
    |   actual_ty ty = ty

    fun transExp (venv, tenv) = 
        let fun trexp (A.VarExp v) = transVar v
            |   trexp A.NilExp = { exp = (), ty = T.NIL }
            |   trexp (A.IntExp t) = { exp = (), ty = T.INT }
            |   trexp (A.StringExp (u, v)) = { exp = (), ty = T.STRING }
            |   trexp (A.CallExp { func, args, pos }) = ( case S.look (venv, func) of
                                                              SOME (E.FunEntry { formals , results }) => let val args' = map (fn t => trexp t) args
                                                                                                             fun checkArgs (x::xs, y::ys) = if x = y then checkArgs (xs, ys)
                                                                                                                                          else (error pos "passed arguments do not match those in function defintion"; false)
                                                                                                             |   checkArgs (nil, nil) = true
                                                                                                             |   checkArgs _ = (error pos "passed arguments do not match those in function definition"; false)
                                                                                                         in checkArgs (args, formals); { exp = (), ty = results } end
                                                            | NONE => (error pos ("function \'" ^ S.name func ^ "\' not defined"); { exp = (), ty = T.UNIT } ) )
            |   trexp (A.OpExp { left, oper, right, pos }) = if foldl (fn (t, u) => u orelse (t = A.oper)) false [A.PlusOp, A.MinusOp, A.TimesOp, A.DivideOp] then ( checkInt (trexp left, pos);
                                                                                                                                                                     checkInt (trexp right, pos);
                                                                                                                                                                     { exp = (), ty = T.INT } )
                                                             else let val { exp = _, ty = leftty } = trexp left
                                                                  in ( if leftty = T.INT then checkInt (trexp right, pos)
                                                                       else checkString (trexp right, pos); 
                                                                       { exp = (), ty = T.INT } )
                                                                  end
            |   trexp (A.RecordExp { fields, typ, pos }) = ()
            |   trexp (A.SeqExp [t]) = trexp t
            |   trexp (A.SeqExp (x::xs)) = (trexp x; trexp xs)
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
                                                                                        in ( if testty <> T.INT then error pos "test condition does not evaluate to an integer" else ()
                                                                                             if thenty <> elsety then error pos "types of \'then\' and \'else\' conditions does not match" else ();
                                                                                             { exp = (), ty = thenty } )
                                                                                        end
                                                                            | _ => trexp (A.IfExp { test = test, then' = then', else' = SOME (A.SeqExp []), pos = pos }) )
            |   trexp (A.WhileExp { test, body, pos }) = let val { exp = _, ty = testty } = trexp test
                                                             val { exp = _, ty = bodyty } = ( nestOrd := (!nestOrd + 1); trexp body )
                                                         in ( if testty <> T.INT then error pos "test condition does not evaluate to an integer";
                                                              if bodyty <> T.UNIT then error pos "body of \'while\' can not have a type";
                                                              nestOrd := (!nestOrd) - 1;
                                                              { exp = (), ty = T.UNIT } )
                                                         end
            |   trexp (A.ForExp { var, escape, lo, high, body, pos }) = let val venv' = S.enter (venv, var, T.INT)
                                                                            val { exp = _, ty = loty } = trexp lo
                                                                            val { exp = _, ty = highty } = trexp high
                                                                            val { body = _, ty = bodyty } = ( nestOrd := (!nestOrd) + 1; transExp (venv', tenv) body)
                                                                        in ( if loty <> T.INT then "lo value of \'for\' is not an integer" else ();
                                                                             if highty <> T.INT then "high value of \'for\' is not an integer" else ();
                                                                             if bodyty <> T.UNIT then "body of \'while\' can not have a type" else ();
                                                                             nestOrd := (!nestOrd) - 1;
                                                                             { exp = (), ty = T.UNIT } )
                                                                        end
            |   trexp (A.BreakExp pos) = ( if (!nestOrd) > 0 then () else error pos "\'break\' is not inside a \'for\' or \'while\' loop";
                                           { exp = (), ty = T.UNIT } )
            |   trexp (A.LetExp { decs, body, pos }) = let val { venv = venv', tenv = tenv' } = transDecs (venv, tenv, decs)
                                                       in transExp (venv', tenv') body end
            |   trexp (A.ArrayExp { typ, size, init, pos }) = ( checkInt (trexp size, pos);
                                                                case S.look (tenv, typ) of T.Array (t, _) => let val { exp = _, ty = initty } = trexp init
                                                                                                             in ( if t = initty then ()
                                                                                                                  else (error pos "the initialisation expression for the array does not matches the array type");
                                                                                                                  { exp = (), ty = t } )
                                                                                                             end
                                                                                         | _ => ( error pos ("\'" ^ typ ^ "\' is not an array type"); { exp = (), ty = T.UNIT } ) )

    

end