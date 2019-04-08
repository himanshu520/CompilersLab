(*  *)

signature TRANSLATE = sig
    type level
    type access
    type exp
    type frag

    val outermost : level
    val newLevel : { parent : level, name : Temp.label, formals : bool list } -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    val nilexp : exp
    val intlit : int -> exp
    val strlit : string -> exp
    val simpleVar : access * level -> exp
    val subscriptVar : exp * exp -> exp
    val fieldVar : exp * Symbol.symbol * Symbol.symbol list -> exp
    val binop : Absyn.oper * exp * exp -> exp
    val relop : Absyn.oper * exp * exp -> exp
    val ifExp : exp * exp * exp option -> exp
    val recordExp : exp list -> exp
    val arrayExp : exp * exp -> exp
    val assignExp : exp * exp -> exp
    val whileExp : exp * exp * Temp.label -> exp
    val forExp : access * exp * exp * exp * Temp.label * level -> exp
    val breakExp : Temp.label -> exp
    val callExp : level * level * Temp.label * exp list * bool -> exp
    val seqExp : exp list -> exp
    val letExp : exp list * exp -> exp

    val procEntryExit : level * exp -> unit
    structure Frame : FRAME
    val getResult : unit -> Frame.frag list
    val reset : unit -> unit
end

structure Translate : TRANSLATE = struct
    structure Frame = MipsFrame
    structure A = Absyn
    structure F = Frame
    structure T = Tree

    datatype level = Root | Child of { parent : level, frame : F.frame, unique : unit ref }
    type access = level * F.access
    type frag = F.frag
    datatype exp = Ex of T.exp
                 | Nx of T.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm
    exception Error = ErrorMsg.Error

    val outermost = Root
    val fragments : frag list ref = ref nil

    fun seq [x] = x
    |   seq [x, y] = T.SEQ (x, y)
    |   seq (x :: xs) = T.SEQ (x, seq xs)
    |   seq _ = raise Error

    (* Extra true is for static link which is also passed explicitly into the argument *)
    fun newLevel { parent, name, formals } = Child { parent = parent, frame = F.newFrame { name = name, formals = true :: formals }, unique = ref () }
    fun formals lev = case lev of Root => []
                                | Child { parent, frame, unique } => map (fn x => (lev, x)) (tl (F.formals frame))
    fun allocLocal lev esc = case lev of Root => raise Error
                                       | Child { parent, frame, unique } => (lev, F.allocLocal frame esc)

    fun reset () = fragments := nil
    fun getResult () = !fragments
    val errexp = Ex (T.CONST 0)

    fun unEx (Ex exp) = exp
    |   unEx (Nx stm) = T.ESEQ (stm, T.CONST 0)
    |   unEx (Cx genstm) = let val r = Temp.newtemp ()
                               val t = Temp.newlabel ()
                               val f = Temp.newlabel ()
                           in T.ESEQ( seq [ T.MOVE (T.TEMP r, T.CONST 1), genstm (t, f), T.LABEL f,
                                            T.MOVE(T.TEMP r, T.CONST 0), T.LABEL t ], T.TEMP r ) end

    fun unNx (Ex exp) = T.EXP exp
    |   unNx (Nx stm) = stm
    |   unNx (Cx genstm) = unNx (Ex (unEx (Cx genstm)))

    fun unCx (Cx genstm) = genstm
    |   unCx (Ex (T.CONST 0)) = (fn (t, f) => T.JUMP (T.NAME f, [f]))
    |   unCx (Ex (T.CONST 1)) = (fn (t, f) => T.JUMP (T.NAME t, [t]))
    |   unCx (Ex exp) = (fn (t, f) => T.CJUMP (T.EQ, exp, T.CONST 1, t, f))
    |   unCx (Nx _) = raise Error


    val nilexp = Ex (T.CONST 0)
    fun intlit n = Ex (T.CONST(n))
    fun strlit s = let val t = List.find (fn x => ( case x of F.PROC _ => false | F.STRING (_, s') => s = s' )) (!fragments)
                   in ( case t of NONE => let val l = Temp.newlabel () 
                                          in ( fragments := F.STRING (l, s) :: (!fragments); Ex (T.NAME l) ) end
                                | SOME (F.STRING (lab, _)) => Ex (T.NAME lab) 
                                | _ => raise Error )
                   end

    fun memSum (e1, e2) = T.MEM (T.BINOP (T.PLUS, e1, e2))

    fun simpleVar (access, level) = let val (Child { parent, frame, unique = varref }, varacc) = access
                                        fun fndFrame (curlevel, acc) = let val (Child { parent, frame , unique = curref }) = curlevel
                                                                       in if varref = curref then F.exp varacc acc
                                                                          else let val staticlink = hd (Frame.formals frame)  
                                                                               in fndFrame (parent, F.exp staticlink acc) end
                                                                       end
                                    in Ex (fndFrame (level, T.TEMP F.FP)) end
    fun subscriptVar (base, offset) = Ex (memSum (unEx base, T.BINOP (T.MUL, unEx offset, T.CONST F.wordSize)))
    fun fieldVar (base, fld, lst) = let fun getPos (pos, fld : Symbol.symbol, x :: xs) = if fld = x then pos else getPos (pos + 1, fld, xs)
                                        |   getPos _ = raise Error
                                    in Ex (memSum (unEx base, T.BINOP (T.MUL, T.CONST (getPos (0, fld, lst)), T.CONST F.wordSize))) end

    fun binop (oper, e1, e2) = let val left = unEx e1
                                   val right = unEx e2
                                   val oper' = ( case oper of A.PlusOp => T.PLUS
                                                            | A.MinusOp => T.MINUS
                                                            | A.TimesOp => T.MUL
                                                            | A.DivideOp => T.DIV 
                                                            | _ => T.PLUS )
                               in Ex (T.BINOP (oper', left, right)) end

    fun relop (oper, e1, e2) = let val left = unEx e1
                                   val right = unEx e2
                                   val treeop = ( case oper of A.EqOp => T.EQ
                                                             | A.NeqOp => T.NE
                                                             | A.LtOp => T.LT
                                                             | A.LeOp => T.LE
                                                             | A.GtOp => T.GT
                                                             | A.GeOp => T.GE 
                                                             | _ => T.EQ )
                               in Cx (fn (t, f) => T.CJUMP (treeop, left, right, t, f)) end
    (* fun relopString (oper, e1, e2) = let val left =  *)

    fun ifExp (test, then', else') = let val r = Temp.newtemp ()
                                         val t = Temp.newlabel ()
                                         val f = Temp.newlabel ()
                                         val join = Temp.newlabel ()
                                         val testfun  = unCx test
                                     in ( case then' of Ex e => ( case else' of SOME e' => Ex (T.ESEQ (seq [ testfun (t, f), T.LABEL t, 
                                                                                                             T.MOVE (T.TEMP r, e),
                                                                                                             T.JUMP (T.NAME join, [join]),
                                                                                                             T.LABEL f, 
                                                                                                             T.MOVE (T.TEMP r, unEx e'),
                                                                                                             T.JUMP (T.NAME join, [join]),
                                                                                                             T.LABEL join ], T.TEMP r))
                                                                               | _ => raise Error )
                                                      | Nx s => ( case else' of SOME s' =>  Nx (seq [ testfun (t, f), T.LABEL t, s,
                                                                                                      T.JUMP (T.NAME join, [join]),
                                                                                                      T.LABEL f, unNx s',
                                                                                                      T.JUMP (T.NAME join, [join]),
                                                                                                      T.LABEL join ])
                                                                              | NONE => Nx (seq [ testfun (t, f), T.LABEL t, s, T.LABEL f ]) )
                                                      | Cx c => ( case else' of SOME c' => Cx (fn (t', f') => seq [ testfun (t, f), T.LABEL t, c (t', f'),
                                                                                                                    T.JUMP (T.NAME join, [join]),
                                                                                                                    T.LABEL f, unCx c' (t', f'),
                                                                                                                    T.JUMP (T.NAME join, [join]),
                                                                                                                    T.LABEL join ])
                                                                              | NONE => Cx (fn (t', f') => seq [ testfun (t, f), T.LABEL t, c (t', f'),
                                                                                                                 T.JUMP (T.NAME join, [join]),
                                                                                                                 T.LABEL f,
                                                                                                                 T.JUMP (T.NAME join, [join]),
                                                                                                                 T.LABEL join ]) ) )
                                     end

    (* fun ifExp (test, then', else') = let val r = Temp.newtemp ()
                                         val t = Temp.newlabel ()
                                         val f = Temp.newlabel ()
                                         val join = Temp.newlabel ()
                                         val testfun  = unCx testexp
                                         val thenfun = unEx then'
                                         val elsefun = unEx else'
                                     in Ex (T.ESEQ (seq [ testfun (t, f), T.LABEL t, 
                                                          T.MOVE (T.TEMP r, thenfun),
                                                          T.JUMP (T.NAME join, [join]),
                                                          T.LABEL f, 
                                                          T.MOVE (T.TEMP r, elsefun),
                                                          T.JUMP (T.NAME join, [join]),
                                                          T.LABEL join ], T.TEMP r))
                                     end *)

    fun recordExp fields = let val r = Temp.newtemp()
                               val init = T.MOVE (T.TEMP r, F.externalCall ("allocRecord", [T.CONST ((length fields) * F.wordSize)]))
                               fun loop ([], pos) = []
                               |   loop (x :: xs, pos) = T.MOVE (memSum (T.TEMP r, T.CONST (pos * F.wordSize)), unEx x) :: loop (xs, pos + 1)
                           in Ex (T.ESEQ (seq (init :: loop (fields, 0)), T.TEMP r)) end
    fun arrayExp (size, init) = Ex (F.externalCall ("initArray", [unEx size, unEx init]))

    fun assignExp (left, right) = Nx (T.MOVE (unEx left, unEx right))

    fun whileExp (test, body, flab) = let val tlab = Temp.newlabel ()
                                          val blab = Temp.newlabel ()
                                      in Nx (seq [ T.LABEL tlab, T.CJUMP (T.EQ, unEx test, T.CONST 0, flab, blab),
                                                   T.LABEL blab, unNx body, T.JUMP (T.NAME tlab, [tlab]), T.LABEL flab ])
                                      end
    fun forExp (var, lo, hi, body, flab, level) = let val blab = Temp.newlabel ()
                                                      val inclab = Temp.newlabel ()
                                                  in Nx (seq [ unNx (assignExp (simpleVar (var, level), lo)),
                                                               T.LABEL blab, unNx body,
                                                               unCx (relop (A.EqOp, simpleVar (var, level), hi)) (flab, inclab),
                                                               T.LABEL inclab,
                                                               T.EXP (T.BINOP (T.PLUS, unEx (simpleVar (var, level)), T.CONST 1)),
                                                               T.LABEL flab ])
                                                  end
    fun breakExp label = Nx (T.JUMP (T.NAME label, [label]))

    fun callExp (_, Child { parent = Root, ... }, label, exps, isProc) = if isProc then Nx (T.EXP (F.externalCall (Symbol.name label, map unEx exps)))
                                                                         else Ex (F.externalCall (Symbol.name label, map unEx exps))

    |   callExp (uselevel, deflevel, label, exps, isProc) = let fun depth Root = 0
                                                                | depth (Child { parent, ... }) = 1 + depth parent
                                                                val nestingDepth = depth uselevel - depth deflevel + 1
                                                                fun findLev (d, curlev) = if d = 0 then T.TEMP Frame.FP
                                                                                          else let val Child { parent, frame, unique } = curlev
                                                                                               in F.exp (hd (F.formals frame)) (findLev (d - 1, parent)) end
                                                                val call = T.CALL (T.NAME label, findLev (nestingDepth, uselevel) :: (map unEx exps))
                                                            in if isProc then Nx (T.EXP call) else Ex call end

    fun seqExp (exps) = let val len = length exps
                        in if len = 0 then Nx (T.EXP (T.CONST 0))
                           else if len = 1 then hd exps
                           else let val first = seq (map unNx (List.take (exps, length exps - 1)))
                                    val last = List.last exps
                                in case last of Nx s => Nx (T.SEQ (first, s))
                                              | _ => Ex (T.ESEQ (first, unEx last))
                                end
                        end

    fun letExp (decs, body) = let val len = List.length decs
                              in if len = 0 then body
                                 else if len = 1 then Ex (T.ESEQ (unNx (hd decs), unEx body))
                                 else let val s = map unNx decs in Ex (T.ESEQ (seq s, unEx body)) end
                              end


    fun procEntryExit (Child { frame, ... }, body) = let val body' = F.procEntryExit1 (frame, T.MOVE (T.TEMP F.RV,unEx body))
                                                     in fragments := F.PROC { frame = frame, body = body' } :: (!fragments) end
    |   procEntryExit _ = raise Error
end