(* This file contains functions to convert IR into MIPS assembly code, however register allocation is yet to be done *)

signature CODEGEN = sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Codegen :> CODEGEN = struct

    structure Frame = MipsFrame
    structure A = Assem
    structure T = Tree

    exception ArgumentOverflow

    fun codegen frame stm = let val ilist = ref (nil : A.instr list)
                                fun emit x = ilist := x :: !ilist
                                fun result gen = let val t = Temp.newtemp () in ( gen t; t ) end
                                
                                val calldefs = Frame.RV :: Frame.RA :: Frame.argRegs
                                fun int x = if x < 0 then "-" ^ Int.toString (~x) else Int.toString x

                                fun munchStm (T.SEQ (a, b)) = ( munchStm a; munchStm b )
                                |   munchStm (T.LABEL lab) = emit (A.LABEL { assem = Symbol.name lab ^ " :\n", lab = lab })

                                |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)) = emit (A.OPER { assem = "sw `s0, " ^ int i ^ "(`s1)",
                                                                                                                     src = [ munchExp e2, munchExp e1 ],
                                                                                                                     dst = [],
                                                                                                                     jump=NONE })
                                |   munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2)) = emit (A.OPER { assem = "sw `s0, " ^ int i ^ "(`s1)",
                                                                                                                     src = [ munchExp e2, munchExp e1 ],
                                                                                                                     dst = [],
                                                                                                                     jump=NONE })
                                |   munchStm (T.MOVE (T.MEM (T.BINOP (T.MINUS, e1, T.CONST i)), e2)) = munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~i))), e2))
                                |   munchStm (T.MOVE (T.MEM (T.BINOP (T.MINUS, T.CONST i, e1)), e2)) = munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~i))), e2))

                                |   munchStm (T.MOVE (T.MEM (T.CONST i), e1)) = emit (A.OPER { assem = "sw `s0, " ^ int i ^ "($zero)",
                                                                                               src = [ munchExp e1 ],
                                                                                               dst = [],
                                                                                               jump=NONE })
                                |   munchStm (T.MOVE (T.MEM e1, e2)) = emit (A.OPER { assem = "sw `s0, 0(`s1)",
                                                                                      src = [ munchExp e2, munchExp e1 ],
                                                                                      dst = [],
                                                                                      jump=NONE })

                                |   munchStm (T.MOVE ((T.TEMP i, T.CONST n))) = emit (A.OPER { assem = "li `d0, " ^ int n,
                                                                                               src = [],
                                                                                               dst = [i], 
                                                                                               jump = NONE })
                                |   munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.PLUS, e1, T.CONST n)))) = emit ( A.OPER { assem = "lw `d0, " ^ int n ^ "(`s0)",
                                                                                                                            src = [ munchExp e1],
                                                                                                                            dst = [i],
                                                                                                                            jump = NONE })
                                |   munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.PLUS, T.CONST n, e2)))) = emit ( A.OPER { assem = "lw `d0, " ^ int n ^ "(`s0)",
                                                                                                                            src = [ munchExp e2 ],
                                                                                                                            dst = [i],
                                                                                                                            jump = NONE })
                                |   munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.MINUS, e1, T.CONST n)))) = munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~n)))))
                                |   munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.MINUS, T.CONST n, e1)))) = munchStm (T.MOVE (T.TEMP i, T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~n)))))

                                |   munchStm (T.MOVE (T.TEMP i, e2)) = emit (A.MOVE { assem = "move `d0, `s0", 
                                                                                      src = munchExp e2,
                                                                                      dst = i })
                                |   munchStm (T.MOVE _) = raise ErrorMsg.Error

                                |   munchStm (T.JUMP (T.NAME lab, _)) = emit(A.OPER { assem = "b `j0",
                                                                                      src = [],
                                                                                      dst = [],
                                                                                      jump = SOME [ lab ] })
                                |   munchStm (T.JUMP (e, labels)) = emit (A.OPER { assem = "jr `s0",
                                                                                   src = [ munchExp e ],
                                                                                   dst = [],
                                                                                   jump = SOME labels })

                                |   munchStm (T.CJUMP (T.EQ, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "beqz `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.NE, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "bnez `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.LT, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "bltz `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.GT, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "bgtz `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.LE, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "blez `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.GE, e1, T.CONST 0, l1, l2)) = emit (A.OPER { assem = "bgez `s0, `j0\nb `j1",
                                                                                                      dst = [],
                                                                                                      src = [ munchExp e1 ],
                                                                                                      jump = SOME [ l1, l2 ] })

                                |   munchStm (T.CJUMP (T.EQ, e1, e2, l1, l2)) = emit (A.OPER { assem = "beq `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.NE, e1, e2, l1, l2)) = emit (A.OPER { assem = "bne `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.LT, e1, e2, l1, l2)) = emit (A.OPER { assem = "blt `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.GT, e1, e2, l1, l2)) = emit (A.OPER { assem = "bgt `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.LE, e1, e2, l1, l2)) = emit (A.OPER { assem = "ble `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.GE, e1, e2, l1, l2)) = emit (A.OPER { assem = "bge `s0, `s1, `j0\nb `j1",
                                                                                               dst = [],
                                                                                               src = [ munchExp e1, munchExp e2 ],
                                                                                               jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.ULT, e1, e2, l1, l2)) = emit (A.OPER { assem = "bltu `s0, `s1, `j0\nb `j1",
                                                                                                dst = [],
                                                                                                src = [ munchExp e1, munchExp e2 ],
                                                                                                jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.UGT, e1, e2, l1, l2)) = emit (A.OPER { assem = "bgtu `s0, `s1, `j0\nb `j1",
                                                                                                dst = [],
                                                                                                src = [ munchExp e1, munchExp e2 ],
                                                                                                jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.ULE, e1, e2, l1, l2)) = emit (A.OPER { assem = "bleu `s0, `s1, `j0\nb `j1",
                                                                                                dst = [],
                                                                                                src = [ munchExp e1, munchExp e2 ],
                                                                                                jump = SOME [ l1, l2 ] })
                                |   munchStm (T.CJUMP (T.UGE, e1, e2, l1, l2)) = emit (A.OPER { assem = "bgeu `s0, `s1, `j0\nb `j1",
                                                                                                dst = [],
                                                                                                src = [ munchExp e1, munchExp e2 ],
                                                                                                jump = SOME [ l1, l2 ] })

                                |   munchStm (T.EXP (T.CALL (e, args))) = let val pairs = map (fn r => (Temp.newtemp (), r)) Frame.callerSaves
                                                                              val srcs = map #1 pairs
                                                                              fun fetch a r = T.MOVE (T.TEMP r, T.TEMP a)
                                                                              fun store a r = T.MOVE (T.TEMP a, T.TEMP r)
                                                                          in ( map (fn (a, r) => munchStm (store a r)) pairs;
                                                                               emit (A.OPER { assem = "jalr `s0",
                                                                                              src = munchExp e :: munchArgs (0, args),
                                                                                              dst = calldefs,
                                                                                              jump = NONE });
                                                                               map (fn (a, r) => munchStm (fetch a r)) (List.rev pairs);
                                                                               () )
                                                                          end
                                |   munchStm (T.EXP e) = ( munchExp e; () )


                                and munchArgs (_, nil) = nil
                                |   munchArgs (i, exp :: rest) = if i < List.length Frame.argRegs
                                                                 then let val dst = List.nth (Frame.argRegs, i)
                                                                      in ( munchStm (T.MOVE (T.TEMP dst, T.TEMP ( munchExp exp )));
                                                                           dst :: munchArgs (i + 1, rest) ) end
                                                                 else raise ArgumentOverflow


                                and munchExp (T.MEM (T.CONST i)) = result (fn r => emit (A.OPER { assem = "lw `d0, " ^ int i ^ "($zero)",
                                                                                                  src = [],
                                                                                                  dst = [r],
                                                                                                  jump = NONE }))
                                |   munchExp (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i))) = result (fn r => emit (A.OPER { assem="lw `d0, " ^ int i ^ "(`s0)",
                                                                                                                        src = [ munchExp e1 ],
                                                                                                                        dst = [r],
                                                                                                                        jump=NONE }))
                                |   munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST i, e2))) = result (fn r => emit (A.OPER { assem="lw `d0, " ^ int i ^ "(`s0)",
                                                                                                                        src = [ munchExp e2 ],
                                                                                                                        dst = [r],
                                                                                                                        jump=NONE }))
                                |   munchExp (T.MEM (T.BINOP (T.MINUS, e1, T.CONST i))) = munchExp (T.MEM (T.BINOP (T.PLUS, e1, T.CONST (~i))))
                                |   munchExp (T.MEM (T.BINOP (T.MINUS, T.CONST i, e2))) = munchExp (T.MEM (T.BINOP (T.PLUS, e2, T.CONST (~i))))
                                |   munchExp (T.MEM e1) = result (fn r => emit (A.OPER { assem = "lw `d0, 0(`s0)",
                                                                                         src = [ munchExp e1 ],
                                                                                         dst = [r],
                                                                                         jump = NONE }))

                                |   munchExp (T.BINOP (T.PLUS, e1, T.CONST i)) = result (fn r => emit (A.OPER { assem = "addiu `d0, `s0, " ^ int i,
                                                                                                                src = [ munchExp e1 ],
                                                                                                                dst = [r],
                                                                                                                jump=NONE }))
                                |   munchExp (T.BINOP (T.PLUS, T.CONST i, e2)) = result (fn r => emit (A.OPER { assem = "addiu `d0, `s0, " ^ int i,
                                                                                                                src = [ munchExp e2 ],
                                                                                                                dst = [r],
                                                                                                                jump=NONE }))
                                |   munchExp (T.BINOP (T.PLUS, e1, e2)) = result (fn r => emit (A.OPER { assem = "add `d0, `s0, `s1",
                                                                                                         src = [ munchExp e1, munchExp e2 ],
                                                                                                         dst = [r],
                                                                                                         jump = NONE }))
                                |   munchExp (T.BINOP (T.MINUS, e1, T.CONST i)) = munchExp (T.BINOP (T.PLUS, e1, T.CONST (~i)))
                                |   munchExp (T.BINOP (T.MINUS, e1, e2)) = result (fn r => emit (A.OPER { assem = "sub `d0, `s0, `s1",
                                                                                                         src = [ munchExp e1, munchExp e2 ],
                                                                                                         dst = [r],
                                                                                                         jump = NONE }))
                                |   munchExp (T.BINOP (T.DIV, e1, e2)) = result (fn r => emit (A.OPER { assem = "div `d0, `s0, `s1",
                                                                                                        src = [ munchExp e1, munchExp e2 ],
                                                                                                        dst = [r],
                                                                                                        jump = NONE }))
                                |   munchExp (T.BINOP (T.MUL, e1, e2)) = result (fn r => emit (A.OPER { assem = "mul `d0, `s0, `s1",
                                                                                                        src = [ munchExp e1, munchExp e2 ],
                                                                                                        dst = [r],
                                                                                                        jump = NONE }))

                                |   munchExp (T.BINOP (T.AND, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "andi `d0, `s0, " ^ int n,
                                                                                                             src = [ munchExp e1 ],
                                                                                                             dst = [r],
                                                                                                             jump = NONE }))
                                |   munchExp (T.BINOP (T.AND, T.CONST n, e2)) = munchExp (T.BINOP (T.AND, e2, T.CONST n))
                                |   munchExp (T.BINOP (T.AND, e1, e2)) = result (fn r => emit (A.OPER { assem = "and `d0, `s0, `s1",
                                                                                                        src = [ munchExp e1, munchExp e2 ],
                                                                                                        dst = [r],
                                                                                                        jump = NONE }))

                                |   munchExp (T.BINOP (T.OR, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "ori `d0, `s0, " ^ int n,
                                                                                                              src = [ munchExp e1 ],
                                                                                                              dst = [r],
                                                                                                              jump = NONE }))
                                |   munchExp (T.BINOP (T.OR, T.CONST n, e2)) = munchExp (T.BINOP (T.OR, e2, T.CONST n))
                                |   munchExp (T.BINOP (T.OR, e1, e2)) = result (fn r => emit (A.OPER { assem = "or `d0, `s0, `s1",
                                                                                                       src = [ munchExp e1, munchExp e2 ],
                                                                                                       dst = [r],
                                                                                                       jump = NONE }))
                                |   munchExp (T.BINOP (T.XOR, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "xori `d0, `s0, " ^ int n,
                                                                                                               src = [ munchExp e1 ],
                                                                                                               dst = [r],
                                                                                                               jump = NONE }))
                                |   munchExp (T.BINOP (T.XOR, T.CONST n, e2)) = munchExp (T.BINOP (T.XOR, e2, T.CONST n))
                                |   munchExp (T.BINOP (T.XOR, e1, e2)) = result (fn r => emit (A.OPER { assem = "xor `d0, `s0, `s1",
                                                                                                        src = [ munchExp e1, munchExp e2 ],
                                                                                                        dst = [r],
                                                                                                        jump = NONE }))

                                |   munchExp (T.BINOP (T.LSHIFT, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "sll `d0, `s0, " ^ int n,
                                                                                                                  src = [ munchExp e1 ],
                                                                                                                  dst = [r],
                                                                                                                  jump = NONE }))
                                |   munchExp (T.BINOP (T.LSHIFT, e1, e2)) = result (fn r => emit (A.OPER { assem = "sllv `d0, `s0, `s1",
                                                                                                          src = [ munchExp e1, munchExp e2 ],
                                                                                                          dst = [r],
                                                                                                          jump = NONE }))
                                
                                |   munchExp (T.BINOP (T.RSHIFT, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "srl `d0, `s0, " ^ int n,
                                                                                                                  src = [ munchExp e1 ],
                                                                                                                  dst = [r],
                                                                                                                  jump = NONE }))
                                |   munchExp (T.BINOP (T.RSHIFT, e1, e2)) = result (fn r => emit (A.OPER { assem = "srlv `d0, `s0, `s1",
                                                                                                           src = [ munchExp e1, munchExp e2 ],
                                                                                                           dst = [r],
                                                                                                           jump = NONE }))

                                |   munchExp (T.BINOP (T.ARSHIFT, e1, T.CONST n)) = result (fn r => emit (A.OPER { assem = "sra `d0, `s0, " ^ int n,
                                                                                                                   src = [ munchExp e1 ],
                                                                                                                   dst = [r],
                                                                                                                   jump = NONE }))
                                |   munchExp (T.BINOP (T.ARSHIFT, e1, e2)) = result (fn r => emit (A.OPER { assem = "srav `d0, `s0, `s1",
                                                                                                            src = [ munchExp e1, munchExp e2 ],
                                                                                                            dst = [r],
                                                                                                            jump = NONE }))

                                |   munchExp (T.CONST i) = result (fn r => emit (A.OPER { assem = "li `d0, " ^ int i,
                                                                                          src = [],
                                                                                          dst = [r],
                                                                                          jump = NONE }))

                                |   munchExp (T.TEMP t) = t
                                |   munchExp (T.NAME label) = result (fn r => emit (A.OPER { assem = "la `d0, " ^ Symbol.name label,
                                                                                             src = [],
                                                                                             dst = [r],
                                                                                             jump = NONE }))

                                |   munchExp (T.CALL (e,args)) = let val pairs = map (fn r => (Temp.newtemp (), r)) Frame.callerSaves
                                                                     val srcs = map #1 pairs
                                                                     fun fetch a r = T.MOVE (T.TEMP r, T.TEMP a)
                                                                     fun store a r = T.MOVE (T.TEMP a, T.TEMP r)
                                                                 in ( map (fn (a, r) => munchStm (store a r)) pairs;
                                                                      emit (A.OPER { assem = "jalr `s0",
                                                                                     src = munchExp e :: munchArgs (0, args),
                                                                                     dst = calldefs,
                                                                                     jump = NONE });
                                                                      map (fn (a, r) => munchStm (fetch a r)) (List.rev pairs);
                                                                      Frame.RV )
                                                                 end
                                |   munchExp (T.ESEQ _) = raise ErrorMsg.Error

                            in ( munchStm stm; rev (!ilist) ) end
end