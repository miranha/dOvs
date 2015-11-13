signature CODEGEN =
sig
    structure Frame: FRAME
    val codegen: Frame.frame -> Tree.stm -> Assem.instr list
end

structure X86Gen: CODEGEN =
struct

structure Frame: FRAME = X86Frame
structure S = Symbol
structure T = Tree
structure Tm = Temp
structure A = Assem
structure F = Frame
structure PT = PrintTree(F)

exception TODO

fun int i =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~i)

fun codegen frame stm =
    let
        val ilist = ref (nil: A.instr list)

        fun emit x = (ilist := x :: (!ilist))

        fun result gen =
            let val t = Tm.newtemp ()
            in  gen t; t
            end

        fun operator2jump oper =
            case oper of
                T.EQ => "je"
              | T.NE => "jne"
              | T.LT => "jl"
              | T.GT => "jg"
              | T.LE => "jle"
              | T.GE => "jge"
              | _ => "bad branch operator!"

        fun moveInstr s d doc = A.MOVE { assem = "\tmovl `s0, `d0"
                                       , src = s
                                       , dst = d
                                       , doc = "x86gen:" ^ doc}

        fun adjustSP count = A.OPER { assem = "\taddl $" ^ int count ^ ", `d0"
                                    , src = [F.SP] (* old-SP used *)
                                    , dst = [F.SP]
                                    , jump = NONE
                                    , doc = "x86gen:52"}

        fun allocArgs count = adjustSP (~F.wordSize*count)
        fun freeArgs count = adjustSP (F.wordSize*count)

        fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)

          (* MOVE *)
          | munchStm (T.MOVE (T.TEMP t, T.CALL (T.NAME l, args))) =
            ( emit (A.OPER { assem = "\tcall " ^ S.name l
                           , src = munchArgs args
                           , dst = F.calldefs
                           , jump = NONE
                           , doc = "x86gen:68"})
            ; emit (freeArgs (length args))
            ; emit (moveInstr F.EAX t "70"))

          | munchStm (T.MOVE (T.MEM e1, T.CALL (T.NAME l, args))) =
            let 
                val t = Tm.newtemp ()
            in  
                emit (A.OPER { assem = "\tcall " ^ S.name l
                             , src = munchArgs args
                             , dst = F.calldefs
                             , jump = NONE
                             , doc = "x86gen:80"});
                raise TODO
            end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)) =
              let
                val s0 = munchExp e1
                val d0 = munchExp e2
              in
                emit( A.MOVE {    assem = "\tmovl " ^ int i ^ "(`s0), d0"
                                , src = s0
                                , dst = d0
                                , doc = "x86gen:92"
                  })
              end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2)) =
            raise TODO

          | munchStm (T.MOVE (T.MEM (T.CONST i), e2)) =
            raise TODO

          | munchStm (T.MOVE (T.MEM e1, e2)) =
            raise TODO

          | munchStm (T.MOVE (T.TEMP i, e2)) =
            let
              val s = munchExp e2
            in
              emit (moveInstr s i "109:muchStm(T.MOVE(T.TEMP i, e2))")
            end
          | munchStm (T.LABEL lab) =
            emit (A.LABEL { assem = S.name lab ^ ":" (* Labels aren't indented *)
                          , lab = lab
                          , doc = "x86gen:munchStm(T.LABEL lab)"})

          (* JUMP *)
          | munchStm (T.CJUMP (oper, T.CONST i, e2, lab1, lab2)) =
            raise TODO

          | munchStm (T.CJUMP (oper, e1, T.CONST i, lab1, lab2)) =
            raise TODO

          | munchStm (T.CJUMP (oper, e1, e2, lab1, lab2)) =
            raise TODO

          | munchStm (T.JUMP (T.NAME lab, llst)) =
            emit (A.OPER {  assem = "\tjmp " ^ S.name lab ^ "\n"
                          , dst = []
                          , src = []
                          , jump = SOME( llst )
                          , doc = "x86gen:munchStm(T.JUMP(T.NAME lab, llst))"
              })

          (* EXP *)
          | munchStm (T.EXP (T.CALL (T.NAME lab, args))) =
            raise TODO
            
          | munchStm (T.EXP exp) =
             (munchExp(exp);())

          (* If no match so far, complain *)
          | munchStm (T.JUMP a) =
            emit (A.OPER { assem = "\t# JUMP: bad JUMP in munchStm!"
                         , src = []
                         , dst = []
                         , jump = NONE
                         , doc = "x86gen:127"})

          | munchStm (T.MOVE a) =
            emit (A.MOVE { assem = "\t# MOVE: bad MOVE in munchStm!"
                         , src = Tm.newtemp ()
                         , dst = Tm.newtemp ()
                         , doc = "x86gen:133"})

        and munchArgs args =
            (* in the simple approach used here, we pass all args in memory *)
            let val rargs = rev args (* push args right-to-left *)
                fun munchArgs1 [] = []
                  | munchArgs1 (ah::at) = raise TODO
            in  munchArgs1 rargs
            end

        (* Memory access *)
        and munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^ "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:151"}))


          | munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST n, e))) =
            result (fn r => raise TODO)

          | munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST n))) =
            result (fn r => raise TODO)

          | munchExp (T.MEM e) =
            result (fn r => raise TODO)

          (* PLUS *)
          | munchExp (T.BINOP (T.PLUS, e1, T.CONST i)) =
            (* We have to return the value in r, so we move value from munchExp e1 into r *)
            result ( fn r => (emit (moveInstr (munchExp e1) r "183");
              emit (A.OPER {  assem = "\tadd $" ^ int i ^ ", `d0"
                              , src = [r]
                              , dst = [r]
                              , jump = NONE
                              , doc = "x86gen:188"})))

          | munchExp (T.BINOP (T.PLUS, T.CONST i, e1)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.PLUS, e1, e2)) =
            (* Hint, p203: use src=[r,_] and do not use `s0,
             * which specifies that r is used *)
            result (fn r => raise TODO)

          (* MINUS *)
          | munchExp (T.BINOP (T.MINUS, e1, T.CONST i)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.MINUS, T.CONST 0, e1)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.MINUS, T.CONST i, e1)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.MINUS, e1, e2)) =
            result (fn r => raise TODO)

          (* MULTIPLY *)
          | munchExp (T.BINOP (T.MUL, e1, e2)) =
            result (fn r => raise TODO)

          (* DIVIDE *)

          | munchExp (T.BINOP (T.DIV, e1, e2)) =
            (* Hint from
             * http://www.cs.mun.ca/~rod/winter2004/cs3724/notes/asm.html:
             *
             * "To divide x by y, we first convert it into 64-bits, and
             * them use idivl.
             *
             *  movl  x, %eax
             *  cltd
             *  idivl y
             *
             * The quotient is in %eax, and the remainder is in %edx."
             *)
            result (fn r => raise TODO)

          (* AND *)
          | munchExp (T.BINOP (T.AND, e1, T.CONST i)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.AND, T.CONST i, e1)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.AND, e1, e2)) =
            result (fn r => raise TODO)

          (* OR *)
          | munchExp (T.BINOP (T.OR, e1, T.CONST i)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.OR, T.CONST i, e1)) =
            result (fn r => raise TODO)

          | munchExp (T.BINOP (T.OR, e1, e2)) =
            result (fn r => raise TODO)

          (* Other constructs *)
          | munchExp (T.TEMP t) = t

          | munchExp (T.ESEQ (s, e)) = result (fn r => raise TODO)

          | munchExp (T.NAME label) =
            result (fn r => raise TODO)

            (* TODO: Could we use a A.MOVE? *)
          | munchExp (T.CONST n) =
            result (fn r => emit (A.OPER { assem = "\tmovl $" ^ int n ^ ", `d0" 
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:munchExp(T.CONST n)" }))
            
          (* If no match so far, complain *)
          | munchExp (tr as T.CALL (_, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad CALL in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad CALL!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:248"})))
          | munchExp (tr as T.BINOP (_, _, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad BINOP in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad BINOP!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:258"})))
    in
        munchStm stm;
        rev (!ilist)
    end

end (* X86Gen *)

