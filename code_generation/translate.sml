structure Translate: TRANSLATE =
struct

structure F = X86Frame
structure TAbs = TAbsyn
structure T = Tree
structure PT = PrintTree(F)

exception TODO (* TODO: replace 'raise TODO' with suitable code *)

val err = ErrorMsg.error

datatype level =
         Top
       | Level of {frame: F.frame, parent: level} * unit ref

type access = level * F.access

datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm

type breakpoint = Tree.label

type frag = F.frag

val bogus = Ex (T.CONST 90)

local
    val frags: (frag list) ref = ref []
in
    fun addFrag frag = (frags := frag::(!frags))
    fun getFrags () = !frags
end

val outermost = Top

exception Bug of string

fun newLevel {parent=l, name=n, formals=f} =
    let
        (* 'true': static link, which always escapes *)
        val frame = F.newFrame {name = n, formals = true::f}
    in
        Level ({frame = frame, parent = l}, ref ())
    end

fun allocLocal Top _ =
    raise Bug "attempt to allocate local variable in top scope"
  | allocLocal (l as Level ({frame, parent}, _)) b =
    (l, F.allocLocal frame b)

fun accessOfFormal l nr escaping =
    (l, F.accessOfFormal nr escaping)

fun formals Top =
    raise Bug "attempt to get formals of top scope"
  | formals (l as Level ({frame,parent}, _)) =
    let
        val fFormals = F.formals frame
        fun facc2acc level facc = (level,facc)
    in
        map (facc2acc l) fFormals
    end

fun seq [] = T.EXP (T.CONST 0)
  | seq [s] = s
  | seq (h::t) = T.SEQ (h, seq t)

fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let
        val r = Temp.newtemp ()
        val t = Temp.newLabel "unEx_t"
        val f = Temp.newLabel "unEx_f"
    in
        T.ESEQ ( seq [ T.MOVE (T.TEMP r, T.CONST 1)
                     , genstm (t, f)
                     , T.LABEL f
                     , T.MOVE (T.TEMP r, T.CONST 0)
                     , T.LABEL t]
               , T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ (s, T.CONST 0)

fun unNx (Ex e) = T.EXP(e)
  | unNx (Cx genstm) = (*TODO: Maybe optimize this part*)
      let
        val r = Temp.newtemp ()
        val t = Temp.newLabel "unCx_t"
        val f = Temp.newLabel "unCx_f"
    in
        T.SEQ ( seq [ T.MOVE (T.TEMP r, T.CONST 1)
                     , genstm (t, f)
                     , T.LABEL f
                     , T.MOVE (T.TEMP r, T.CONST 0)
                     , T.LABEL t]
               , T.EXP(T.TEMP r)) (*Could be move inside seq => the same*)
    end
  | unNx (Nx s) = s

fun unCx (Ex (T.CONST 0)) = (fn(_,f)=>T.JUMP(T.NAME f, [f])) (*TODO: Is it okay to use "_" here?*)
  | unCx (Ex (T.CONST _)) = (fn(t,_)=>T.JUMP(T.NAME t, [t]))
  | unCx (Ex e) = (fn(t,f)=>T.CJUMP(T.NE, e, T.CONST 0, t,f))
  | unCx (Cx genstm) = genstm
  | unCx (Nx _) = raise Bug "Error: Should never occur"

val empty = Ex (T.CONST 0)

val newBreakPoint = Temp.newLabel

fun levelEq (Level (_, u1), Level (_, u2)) = (u1 = u2)
  | levelEq _ = false

fun followStaticLink toLevel (fromLevel as Level ({frame, parent}, _)) =
    if levelEq (toLevel, fromLevel)
    then T.TEMP F.FP
    else 
      let 
        val parent' = followStaticLink toLevel parent
        val offset' = F.staticLinkOffset frame
      in
        T.MEM(T.BINOP(T.PLUS, T.CONST offset', parent')) (*TODO: Made from p. 156*)
      end
  | followStaticLink _ Top =
    T.TEMP F.FP (* delivered to built-in functions like chr,ord,.. *)

fun simpleVar (acc, fromLevel) = (* must return Ex (TEMP _) or Ex (MEM _) *)
    let
      val (lev', acc') = acc
      val exp' = F.exp acc' (followStaticLink lev' fromLevel)
    in
      Ex(exp')
    end

fun fieldVar (var, offset) =
    (* must return Ex (TEMP _) or Ex (MEM _) *)
    let
      val var' = unEx var
      val temp = Temp.newtemp()
      val nilLabel = Temp.newLabel "if_nil"
      val notNilLabel = Temp.newLabel "not_nil"
    in
     Ex(
        T.ESEQ( seq [ T.MOVE( T.TEMP temp, var')
                    , T.CJUMP(T.EQ, T.TEMP temp, T.CONST 0, nilLabel, notNilLabel)
                    , T.LABEL nilLabel
                    , T.EXP( F.externalCall("recFieldError",[]) )
                    , T.LABEL notNilLabel],
          T.MEM(T.BINOP(T.PLUS, T.TEMP temp, T.BINOP(T.MUL,T.CONST offset,T.CONST F.wordSize)))
          )
        )
    end

fun subscript2IR (arr, offset) =
    (* must return Ex (TEMP _) or Ex (MEM _) *)
    let
        val offsetT = Temp.newtemp ()
        val addressT = Temp.newtemp ()
        val maxInxT = Temp.newtemp ()
        val negativeL = Temp.newLabel "subs_neg"
        val nonNegativeL = Temp.newLabel "subs_nneg"
        val overflowL = Temp.newLabel "subs_ovf"
        val noOverflowL = Temp.newLabel "subs_novf"
        val arr' = unEx arr
        val offset' = unEx offset
        val size = T.MEM(T.BINOP(T.PLUS, T.TEMP addressT, T.CONST (~F.wordSize))) (* if elemtents starts at i, the arrays size is at i-Wordsize *)
    in
      (* First checks for negative index, then for overflow. If an error, call external error handling *)
        Ex( T.ESEQ (seq [
              T.MOVE (T.TEMP addressT, arr'),
              T.MOVE (T.TEMP offsetT, offset'),
              T.CJUMP(T.GT, T.TEMP offsetT, T.CONST ~1, nonNegativeL, negativeL),
              T.LABEL negativeL,
              T.EXP(F.externalCall("arrInxError", [T.TEMP offsetT])),
              T.LABEL nonNegativeL,
              T.CJUMP(T.LT, T.TEMP offsetT, size, noOverflowL, overflowL),
              T.LABEL overflowL,
              T.EXP(F.externalCall("arrInxError", [T.TEMP offsetT])),
              T.LABEL noOverflowL],
              T.MEM(T.BINOP(T.PLUS, T.TEMP addressT, T.BINOP(T.MUL, T.TEMP offsetT, T.CONST F.wordSize))))
          )
    end

fun assign2IR (var, exp) =
    let
        val var = unEx var
        val exp = unEx exp
    in
        Nx (T.MOVE (var, exp))
    end

fun break2IR break =
    Nx (T.JUMP (T.NAME break, [break]))

fun int2IR i = Ex (T.CONST i) (* Return a constant of that value *)

fun nil2IR () = Ex (T.CONST 0)

fun ifThen2IR (test, thenExp) =
    let
        val test' = unCx test
        val labelThen = Temp.newLabel "if_then"
        val labelEnd = Temp.newLabel "if_end"
    in
        case (test', thenExp)
         of (_, Cx func) =>
            Cx (fn (t, f) =>
                   seq [ test' (labelThen, labelEnd)
                       , T.LABEL labelThen
                       , func (t, f)(*TODO: Maybe change to unCx thenExp instead..*)
                       , T.LABEL labelEnd])
          | (_, Nx _) =>
                Nx(
                  seq[test'(labelThen,labelEnd)
                      , T.LABEL labelThen
                      ,  unNx thenExp (*TODO:Why does using func not work?*)
                      , T.LABEL labelEnd]
                )
          | (_, Ex _) =>
            Ex (
                T.ESEQ((seq[test'(labelThen,labelEnd)
                      , T.LABEL labelThen
                      ,  unNx thenExp (*TODO:Why does using func not work?*)
                      , T.LABEL labelEnd])
                  ,T.CONST 0)
                )
                  
    end

fun ifThenElse2IR (test, thenExp, elseExp) =
    let
        val test' = unCx test
        val labelThen = Temp.newLabel "if_then"
        val labelElse = Temp.newLabel "if_else"
        val labelJoin = Temp.newLabel "if_join"
    in
        case (test', thenExp, elseExp)
         of (_, Cx _, Cx _) =>
          Cx (fn (t, f) =>
                   seq [ test' (labelThen, labelElse)
                       , T.LABEL labelThen
                       , unCx thenExp (t, f)
                       , T.JUMP (T.NAME labelJoin, [labelJoin])
                       , T.LABEL labelElse
                       , unCx elseExp(t,f)
                       , T.LABEL labelJoin])
          | (_, Ex _, Ex _) =>
            let
                val r = Temp.newtemp () (* suggested on page 162 *)
           in
            Ex( 
                T.ESEQ ( seq [ test'(labelThen,labelElse)
                     , T.LABEL labelThen
                     , T.MOVE (T.TEMP r, unEx thenExp) 
                     , T.JUMP (T.NAME labelJoin, [labelJoin])
                     , T.LABEL labelElse
                     , T.MOVE (T.TEMP r, unEx elseExp)
                     , T.LABEL labelJoin]
               , T.TEMP r)
            )
            end
          (*| (_, Nx _, _) =>*) (*TODO: Why do they give us two of Nx??Optimize??? I think it is just to make the pattern match exostive*)
            (*raise TODO*)
          | (_, Nx _, Nx _) =>
            Nx(
              seq [ test'(labelThen,labelElse)
                     , T.LABEL labelThen
                     , unNx thenExp 
                     , T.JUMP (T.NAME labelJoin, [labelJoin])
                     , T.LABEL labelElse
                     , unNx elseExp
                     , T.LABEL labelJoin]
              )
            
          | (_, Cx _, Ex _) => (*TODO: Optimize See book: 162 for example*)
          let
              val r = Temp.newtemp () (* suggested on page 162 *)
           in
            Ex( 
                T.ESEQ ( seq [ test'(labelThen,labelElse)
                     , T.LABEL labelThen
                     , T.MOVE (T.TEMP r, unEx thenExp) (*TODO: I think we need to use unCx instead to optimize, also below*)
                     , T.JUMP (T.NAME labelJoin, [labelJoin])
                     , T.LABEL labelElse
                     , T.MOVE (T.TEMP r, unEx elseExp)
                     , T.LABEL labelJoin]
               , T.TEMP r)
            )
            end
          | (_, Ex _, Cx _) =>
            let
                val r = Temp.newtemp () (* suggested on page 162 *)
           in
            Ex( 
                T.ESEQ ( seq [ test'(labelThen,labelElse)
                     , T.LABEL labelThen
                     , T.MOVE (T.TEMP r, unEx thenExp) 
                     , T.JUMP (T.NAME labelJoin, [labelJoin])
                     , T.LABEL labelElse
                     , T.MOVE (T.TEMP r, unEx elseExp)
                     , T.LABEL labelJoin]
               , T.TEMP r)
            )
            end
          | (_, _, _) =>
            raise Bug "encountered thenBody and elseBody of different kinds"
    end

fun binop2IR (oper, left, right) =
    let val left' = unEx left
        val right' = unEx right
      in
        Ex(T.BINOP(oper,left',right'))
    end

fun relop2IR (oper, left, right) =
    let val left' = unEx left
        val right' = unEx right
    in
      Cx ((fn(t,f)=>T.CJUMP(oper, left', right', t,f)))
    end

fun exponent2IR (left, right) =
    Ex (T.CALL ( T.NAME (Temp.namedLabel "exponent") (*TODO: Check function name*)
               , [unEx left, unEx right])) (*TODO: could also have used external call*)

fun intOp2IR (TAbs.PlusOp, left, right)   = binop2IR (T.PLUS, left, right)
  | intOp2IR (TAbs.MinusOp, left, right)  = binop2IR (T.MINUS, left, right)
  | intOp2IR (TAbs.TimesOp, left, right)  = binop2IR (T.MUL, left, right)
  | intOp2IR (TAbs.DivideOp, left, right) = binop2IR (T.DIV, left, right)
  | intOp2IR (TAbs.EqOp, left, right)     = relop2IR (T.EQ, left, right)
  | intOp2IR (TAbs.NeqOp, left, right)    = relop2IR (T.NE, left, right)
  | intOp2IR (TAbs.LtOp, left, right)     = relop2IR (T.LT, left, right)
  | intOp2IR (TAbs.LeOp, left, right)     = relop2IR (T.LE, left, right)
  | intOp2IR (TAbs.GtOp, left, right)     = relop2IR (T.GT, left, right)
  | intOp2IR (TAbs.GeOp, left, right)     = relop2IR (T.GE, left, right)
  | intOp2IR (TAbs.ExponentOp, left, right) = exponent2IR (left, right)(*TODO: I can not find pow function, we need to implment it?*)


fun let2IR ([], body) = body
  | let2IR (decls, body) = Ex (T.ESEQ (seq (map unNx decls), unEx body))

fun eseq2IR [] = raise Bug "attempt to eseq2IR an empty sequence"
  | eseq2IR (exp :: exps) =
    let
        fun eseq2IR' exp [] = unEx exp
          | eseq2IR' exp (exp'::exps') =
            T.ESEQ (unNx exp, eseq2IR' exp' exps')
    in
        Ex (eseq2IR' exp exps)
    end

fun seq2IR [] =
    let
        val mtseqLabel = Temp.newLabel "mtseq"
    in
        Nx (T.LABEL mtseqLabel)
    end
  | seq2IR [exp] = exp
  | seq2IR (exp :: exps) = Nx (T.SEQ (unNx exp, unNx (seq2IR exps)))

fun string2IR str =
    let
        val label = Temp.newLabel "string"
    in
        addFrag (F.STRING (label, str));
        Ex (T.NAME label)
    end

val defaultStringIR = string2IR "DefaultString"

fun relopStr2IR (left, right, str) =
    Ex (F.externalCall (str, [unEx left, unEx right]))

fun stringOp2IR (TAbs.EqOp, l, r)  = relopStr2IR (l, r, "stringEqual")
  | stringOp2IR (TAbs.NeqOp, l, r) = relopStr2IR (l, r, "stringNotEq")
  | stringOp2IR (TAbs.LtOp, l, r)  = relopStr2IR (l, r, "stringLess")
  | stringOp2IR (TAbs.LeOp, l, r)  = relopStr2IR (l, r, "stringLessEq")
  | stringOp2IR (TAbs.GtOp, l, r)  = relopStr2IR (l, r, "stringGreater")
  | stringOp2IR (TAbs.GeOp, l, r)  = relopStr2IR (l, r, "stringGreaterEq")
  | stringOp2IR (_, _, _)     = raise Bug "illegal operation on strings"

fun while2IR (test, body, done) =
    let
        val test = unCx test
        val body = unNx body
        val labelTest = Temp.newLabel "while_test"
        val labelBody = Temp.newLabel "while_body"
    in

        Nx(
            seq[T.LABEL labelTest
                , test(labelBody,done)
                , T.LABEL labelBody
                , body
                , T.JUMP(T.NAME labelTest, [labelTest])
                , T.LABEL done]
          )
    end

fun for2IR (var, done, lo, hi, body) =
        let
        val var' = unEx var
        val bodyL = Temp.newLabel "for_body"
        val doneL = done(*Temp.newLabel "for_done"*)
        val hiReg = Temp.newtemp ()
        val testReg = Temp.newtemp ()
        val high = unEx hi
        val low = unEx lo
        val body' = unNx body (* Semantic dictate that body expression are unit types *)


    in
      Nx (
          seq [ T.MOVE(var',low)
              , T.MOVE(T.TEMP hiReg, high)
              , T.CJUMP(T.LE, var', T.TEMP hiReg, bodyL, doneL) (* Actually, we test here if low <= high *)
              , T.LABEL bodyL
              , body'
              , T.MOVE(T.TEMP testReg, var')
              , T.MOVE(var', T.BINOP(T.PLUS, var', T.CONST 1)) (* Increment var. TODO: Can we avoid testReg? *)
              (*  Here we have var' - 1 <= high. Only continue if var' - 1 < high *)
              (* The test var' <=  high would cause overflow of var' if high is maximal int we can represent,
              next time we end up here, since we add 1 to var' before *)
              , T.CJUMP(T.LT, T.TEMP testReg, T.TEMP hiReg, bodyL, doneL)
              , T.LABEL doneL ]
        )
        (*Nx(

          )*)(*raise TODO*)
    end

fun funCall2IR ( toLevel as Level ({frame, parent}, _)
               , fromLevel
               , label
               , exps) =
    let
        val sl = followStaticLink parent fromLevel
    in
        Ex (T.CALL (T.NAME label, sl :: (map unEx exps)(*raise TODO*)))
    end
  | funCall2IR (Top, _, _, _) =
    raise Bug "called function seems to have above-top-level context"

fun procCall2IR ( toLevel as Level ({frame, parent}, _)
                , fromLevel
                , label
                , exps) =
    let
        val sl = followStaticLink parent fromLevel
    in
        Nx (T.EXP (T.CALL (T.NAME label, sl :: (map unEx exps)(*raise TODO*))))
    end
  | procCall2IR (Top, _, _, _) =
    raise Bug "called procedure seems to have above-top-level context"

fun array2IR (size, init) =
    Ex (T.CALL ( T.NAME (Temp.namedLabel "initArray")
               , [unEx size, unEx init])) (*TODO: could also have used external call*)

fun record2IR explist =
    let
        val size = T.CONST (length explist)
        val r = Temp.newtemp ()
        val setup = T.MOVE ( T.TEMP r
                           , T.CALL (T.NAME (Temp.namedLabel "allocRecord") (*TODO:Check this function*)
               , [size]) (* call "allocRecord" *))
        fun step (exp, n) =
            T.MOVE ( T.MEM(T.BINOP(T.PLUS, T.TEMP r, T.CONST (n*F.wordSize)))(* the n-th field in the record *)(*T.MEM(T.BINOP(T.PLUS, T.CONST offset', parent'))*)
                   , unEx exp)
        fun steps ([], n) = []
          | steps (e::es, n) = (step (e, n))::(steps (es, n+1))
    in
        Ex (T.ESEQ (seq (setup :: steps (explist, 0)), T.TEMP r))
    end




fun funEntryExit {level = Level ({frame, parent}, _), body = body} =
    let
        val body' = F.funEntryExit1 (frame, unEx body)
        val frag = F.PROC {body = body', frame = frame}
    in
        addFrag frag
    end
  | funEntryExit {level = Top, ...} =
    raise Bug "attempt to add function at top level"

fun procEntryExit {level = Level ({frame, parent}, _), body = body} =
    let
        val body' = F.procEntryExit1 (frame, unNx body)
        val frag = F.PROC {body = body', frame = frame}
    in
        addFrag frag
    end
  | procEntryExit {level = Top, ...} =
    raise Bug "attempt to add procedure at top level"

(* Gives a new frame to a function *)
fun funHeader2IR (parent, name, formals) =
  let
    val nameL = Temp.newLabel name (* Function names are symbols *)
  in
    {level = (newLevel {parent = parent, name = nameL, formals = formals}), label = nameL}
  end

fun getResult () = getFrags ()

(* ---------- Printing ---------- *)

fun asStringLevel Top = Symbol.name Temp.topLabel
  | asStringLevel (Level ({frame, parent}, _)) =
    "Level {frame = " ^
    F.asStringFrame frame ^
    ", parent = " ^
    asStringLevel parent ^
    "}"

fun asStringAccess (level, access) =
    "(" ^
    asStringLevel level ^
    ", " ^
    F.asStringAccess access ^
    ")"

fun asStringExp (Ex t) = "Ex(" ^ PT.asStringExp t ^ ")"
  | asStringExp (Nx t) = "Nx(" ^ PT.asStringStm t ^ ")"
  | asStringExp (Cx f) =
    let
        val labelTrue = Temp.newLabel "true_label"
        val labelFalse = Temp.newLabel "false_label"
    in
        "Cx(" ^ PT.asStringStm (f (labelTrue, labelFalse)) ^ ")"
    end

fun printX asStringX (outstream, x) =
    ( TextIO.output (outstream, asStringX x ^ "\n")
    ; TextIO.flushOut outstream)

val printLevel  = printX asStringLevel
val printAccess = printX asStringAccess
val printExp    = printX asStringExp

fun prIR (ir_val:frag list) = List.app(fn x=>PT.printFrag(TextIO.stdOut,x)) ir_val


end (* Translate *)
