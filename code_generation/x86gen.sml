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

        (* e1 oper e2 <=> e2 (flipoper oper) e1 *)
        fun flipoper oper =
          case oper of
              T.LT => T.GT
            | T.GT => T.LT
            | T.GE => T.LE
            | T.LE => T.GE
            | _ => oper

        fun moveInstr s d doc = A.MOVE { assem = "\tmovl `s0, `d0"
                                       , src = s
                                       , dst = d
                                       , doc = "x86gen:" ^ doc}

        fun adjustSP count = A.OPER { assem = "\taddl $" ^ int count ^ ", `d0"
                                    , src = [F.SP] (* old-SP used *)
                                    , dst = [F.SP]
                                    , jump = NONE
                                    , doc = "x86gen:64"}

        fun allocArgs count = adjustSP (~F.wordSize*count)
        fun freeArgs count = adjustSP (F.wordSize*count)

        fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)

          (* MOVE *)
          | munchStm (T.MOVE (T.TEMP t, T.CALL (T.NAME l, args))) =
            ( emit (A.OPER { assem = "\tcall " ^ S.name l
                           , src = munchArgs args
                           , dst = F.calldefs
                           , jump = NONE
                           , doc = "x86gen:77"})
            ; emit (moveInstr F.EAX t "78")
            ; emit (freeArgs (length args)))

          | munchStm (T.MOVE (T.MEM e1, T.CALL (T.NAME l, args))) =
            let 
                val t = Tm.newtemp ()
                val d0 = munchExp e1
            in  
                emit (A.OPER { assem = "\tcall " ^ S.name l
                             , src = munchArgs args
                             , dst = F.calldefs
                             , jump = NONE
                             , doc = "x86gen:90"});
                emit (moveInstr F.EAX t "91");
                emit (freeArgs (length args));
                emit (A.OPER  { assem = "\tmovl `s0, (`d0)"
                              , src = [t]
                              , dst = [d0]
                              , jump = NONE
                              , doc = "x86gen:96"})
            end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)) =
              let
                val d0 = munchExp e1
                val s0 = munchExp e2
              in
                emit( A.OPER {    assem = "\tmovl `s0, "  ^ int i ^ "(`d0)"
                                , src = [s0]
                                , dst = [d0]
                                , jump = NONE
                                , doc = "x86gen:108"
                  })
              end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2)) = (*TODO: Is this the same as above?*)
              let
                val d0 = munchExp e1
                val s0 = munchExp e2
              in
                emit( A.OPER {    assem = "\tmovl `s0, "  ^ int i ^ "(`d0)"
                                , src = [s0]
                                , dst = [d0]
                                , jump = NONE
                                , doc = "x86gen:121"
                  })
              end

          | munchStm (T.MOVE (T.MEM (T.CONST i), e2)) = (*TODO*)
              let
                val s0 = munchExp e2
              in
                emit( A.OPER {    assem = "\tmovl `s0, ("  ^ int i ^ ")"
                                , src = [s0]
                                , dst = []
                                , jump = NONE
                                , doc = "x86gen:133"
                  })
              end            

          | munchStm (T.MOVE (T.MEM e1, e2)) =
            let
              val d0 = munchExp e1
              val s0 = munchExp e2
            in
              emit (A.OPER  { assem = "\tmovl `s0, (`d0)"
                            , src = [s0]
                            , dst = [d0]
                            , jump = NONE
                            , doc = "x86gen:146"})
            end

          | munchStm (T.MOVE (T.TEMP i, e2)) =
            let
              val s = munchExp e2
            in
              emit (moveInstr s i "153")
            end
          | munchStm (T.LABEL lab) =
            emit (A.LABEL { assem = S.name lab ^ ":" (* Labels aren't indented *)
                          , lab = lab
                          , doc = "x86gen:158"})

          (* JUMP *)
          (*  The general strategy is this:
              Firstly, execute cmp on registers
              Then the relevant jump conditonal
          *)
          | munchStm (T.CJUMP (oper, T.CONST i, e2, lab1, lab2)) =
            (emit (A.OPER { assem = "\tcmp $" ^ int i ^ ", `s0"
                          , src = [munchExp e2]
                          , dst = []
                          , jump = NONE
                          , doc = "x86gen:170"});
            emit (A.OPER { assem = "\t" ^ (operator2jump (flipoper oper)) ^ " " ^ S.name lab1
                          , src = []
                          , dst = []
                          , jump = SOME([lab1,lab2])
                          , doc = "x86gen:175"}))

          | munchStm (T.CJUMP (oper, e1, T.CONST i, lab1, lab2)) =
            (* In AT&T, we have syntex cmp arg2, arg1 *)
            (emit (A.OPER { assem = "\tcmp $" ^ int i ^ ", `s0"
                          , src = [munchExp e1]
                          , dst = []
                          , jump = NONE
                          , doc = "x86gen:183"});
            emit (A.OPER {  assem = "\t" ^ (operator2jump oper) ^ " " ^ S.name lab1
                          , src = []
                          , dst = []
                          , jump = SOME([lab1, lab2])
                          , doc = "x86gen:188"}))

          | munchStm (T.CJUMP (oper, e1, e2, lab1, lab2)) =
            let 
              val s = munchExp e1
              val d = munchExp e2
            in
              (emit (A.OPER { assem = "\tcmp `d0, `s0"
                            , src = [s,d]
                            , dst = [d]
                            , jump = NONE
                            , doc = "x86gen:199"}));
              (emit (A.OPER { assem = "\t" ^ (operator2jump oper) ^ " " ^ S.name lab1
                            , src = []
                            , dst = []
                            , jump = SOME([lab1,lab2])
                            , doc = "x86gen:204"}))
            end

          | munchStm (T.JUMP (T.NAME lab, llst)) =
            emit (A.OPER {  assem = "\tjmp " ^ S.name lab ^ "\n"
                          , dst = []
                          , src = []
                          , jump = SOME( llst )
                          , doc = "x86gen:212"
              })

          (* EXP *)
          | munchStm (T.EXP (T.CALL (T.NAME lab, args))) =
            ( emit (A.OPER { assem = "\tcall " ^ S.name lab
                           , src = munchArgs args
                           , dst = F.calldefs
                           , jump = NONE
                           , doc = "x86gen:221"})
            ; emit (freeArgs (length args)))
            
          | munchStm (T.EXP exp) =
             (munchExp(exp);())

          (* If no match so far, complain *)
          | munchStm (T.JUMP a) =
            emit (A.OPER { assem = "\t# JUMP: bad JUMP in munchStm!"
                         , src = []
                         , dst = []
                         , jump = NONE
                         , doc = "x86gen:233"})

          | munchStm (T.MOVE a) =
            emit (A.MOVE { assem = "\t# MOVE: bad MOVE in munchStm!"
                         , src = Tm.newtemp ()
                         , dst = Tm.newtemp ()
                         , doc = "x86gen:239"})

        and munchArgs args =
            (* in the simple approach used here, we pass all args in memory *)
            let val rargs = rev args (* push args right-to-left *)
                fun munchArgs1 [] = []
                  | munchArgs1 (ah::at) = 
                        let val eh = munchExp ah
                          in
                            eh::(munchArgs1 at)   
                        end
                
                fun assembler src = emit(A.OPER{assem = "\tpushl `s0"
                                        , src = [src]
                                        , dst = []
                                        , jump=NONE
                                        , doc = "x86gen:255"});

                fun pushArgs (e::es) = (assembler e; pushArgs es)
                  | pushArgs [] = ()
                  val rtmp = rev (munchArgs1 args)

            in
              emit (allocArgs(length args)); 
              pushArgs rtmp; 
              rtmp
            end

        (* Memory access *)
        and munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^ "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:273"}))


          | munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST n, e))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^ "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:281"}))
(*TODO: What about the opposite for negative?*)
          | munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int (~n) ^ "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:288"}))

          | munchExp (T.MEM e) =
            result (fn r => emit (A.OPER  { assem = "\tmovl (`s0), `d0"
                                    , src = [munchExp e]
                                    , dst = [r]
                                    , jump = NONE
                                    , doc = "x86gen:295"}))

          (* PLUS *)
          | munchExp (T.BINOP (T.PLUS, e1, T.CONST i)) =
            (* We have to return the value in r, so we move value from munchExp e1 into r *)
            result ( fn r => (emit (moveInstr (munchExp e1) r "300");
              emit (A.OPER {  assem = "\taddl $" ^ int i ^ ", `d0"
                              , src = [r]
                              , dst = [r]
                              , jump = NONE
                              , doc = "x86gen:305"})))

          | munchExp (T.BINOP (T.PLUS, T.CONST i, e1)) =
            result ( fn r => (emit (moveInstr (munchExp e1) r "308");
              emit (A.OPER {  assem = "\taddl $" ^ int i ^ ", `d0"
                              , src = [r]
                              , dst = [r]
                              , jump = NONE
                              , doc = "x86gen:313"})))

          | munchExp (T.BINOP (T.PLUS, e1, e2)) =
            (* Hint, p203: use src=[r,_] and do not use `s0,
             * which specifies that r is used *)
            result (fn r => (emit (moveInstr (munchExp e1) r "318");
                            emit (  A.OPER  { assem = "\taddl `s0, `d0"
                                            , src = [munchExp e2]
                                            , dst = [r]
                                            , jump = NONE
                                            , doc = "x86gen:323"})))

          (* MINUS *)
          | munchExp (T.BINOP (T.MINUS, T.CONST 0, e1)) =
                      result (fn r => (emit (moveInstr (munchExp e1) r "x86gen:201");
                        emit( A.OPER { assem = "\tnegl `d0"
                                      , src = [r]
                                      , dst = [r]
                                      , jump = NONE
                                      , doc = "x86gen:332"})))

          | munchExp (T.BINOP (T.MINUS, e1, T.CONST i)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "335");
                            emit (  A.OPER  { assem = "\tsubl $" ^ int i ^ ", `d0"
                                            , src = [r]
                                            , dst = [r]
                                            , jump = NONE
                                            , doc = "x86gen:340"})))

          | munchExp (T.BINOP (T.MINUS, T.CONST i, e1)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:347"});
                            emit (A.OPER  { assem = "\tsubl `s0, `d0"
                                          , src = [munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:352"})))

          | munchExp (T.BINOP (T.MINUS, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "355");
                            emit (A.OPER  { assem = "\tsubl `s0, `d0"
                                          , src = [munchExp e2]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86frame:360"})))

          (* MULTIPLY *)
          | munchExp (T.BINOP (T.MUL, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "359");
                              emit( A.OPER  { assem = "\timull `s1, `d0"
                                            , src = [r, munchExp e2]
                                            , dst = [r]
                                            , jump = NONE
                                            , doc = "x86gen:369"})))
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
            let 
              val res1 = munchExp e1
              val res2 = munchExp e2
            in
              result (fn r => (   emit (  moveInstr res2 r "389");
                                  emit (  moveInstr res1 F.EAX "390");
                                  emit (  A.OPER  { assem = "\tcltd"
                                                  , src = [F.EAX]
                                                  , dst = [F.EAX, F.EDX]
                                                  , jump = NONE
                                                  , doc = "x86gen:395"});
                                  emit(   A.OPER  { assem="\tidivl `s0"
                                                  , src = [r]
                                                  , dst = [F.EAX]
                                                  , jump = NONE
                                                  , doc = "x86gen: 400"});
                                  emit( moveInstr F.EAX r "401" )))
            end

          (* AND *)
          | munchExp (T.BINOP (T.AND, e1, T.CONST i)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, `d0"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 409"});
                                emit(A.OPER{assem="\tandl $" ^ int i ^ ", `s0"
                                        , src = [r]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 414"})))

          | munchExp (T.BINOP (T.AND, T.CONST i, e1)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, `d0"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 420"});
                                emit(A.OPER{assem="\tandl $" ^ int i ^ ", `s0"
                                        , src = [r]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 425"})))

          | munchExp (T.BINOP (T.AND, e1, e2)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, `d0"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 431"});
                                emit(A.OPER{assem="\tandl `s1, `s0"
                                        , src = [r, munchExp e2]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 436"})))

          (* OR *)
          | munchExp (T.BINOP (T.OR, e1, T.CONST i)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, d0`"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 443"});
                                emit(A.OPER{assem="\torl $" ^ int i ^ ", `s0"
                                        , src = [r]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 448"})))

          | munchExp (T.BINOP (T.OR, T.CONST i, e1)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, d0`"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 454"});
                                emit(A.OPER{assem="\torl $" ^ int i ^ ", `s0"
                                        , src = [r]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 459"})))

          | munchExp (T.BINOP (T.OR, e1, e2)) =
            result (fn r => (emit(A.MOVE{assem="\tmovl `s0, d0`"
                                        , src=munchExp e1
                                        , dst=r
                                        , doc = "x86gen: 465"});
                                emit(A.OPER{assem="\torl `s1, `s0"
                                        , src = [r, munchExp e2]
                                        , dst = [r]
                                        , jump = NONE
                                        , doc = "x86gen: 470"})))

          (* Other constructs *)
          | munchExp (T.TEMP t) = t

          | munchExp (T.ESEQ (s, e)) = (*TODO: Why can we come inside here, removed by canon.sml, right?*)
            (munchStm s;
            result (fn r => emit(A.MOVE{assem="\tmovl `s0,`d0"
                                        , src = munchExp e
                                        , dst = r
                                        , doc = "x86gen:480"})))

          | munchExp (T.NAME label) =
            result (fn r => emit (A.OPER  { assem = "\tmovl $" ^ S.name label ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:487"}))

          | munchExp (T.CONST n) =
            result (fn r => emit (A.OPER { assem = "\tmovl $" ^ int n ^ ", `d0" 
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:494" }))
        
          (* If no match so far, complain *)
          | munchExp (tr as T.CALL (_, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad CALL in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad CALL!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:505"})))
          | munchExp (tr as T.BINOP (_, _, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad BINOP in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad BINOP!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:514"})))
    in
        munchStm stm;
        rev (!ilist)
    end

end (* X86Gen *)

