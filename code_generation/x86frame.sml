structure X86Frame: FRAME =
struct

(* We will use the following format for an activation record:
 *
 * ^ higher addresses
 * ------------------------------ previous frame
 *   ...
 *   argument n                   [FP+4*(2+n)]
 *   ...
 *   argument 1                   [FP+4*(2+1)]
 *   static link                  [FP+4*2]
 *   return address               [FP+4]
 * ------------------------------ current frame
 *   old frame pointer            [FP]            <-- frame pointer
 *   local variable 1             [FP-4*1]
 *   ...
 *   local variable m             [FP-4*m]
 *   temp 1                       [FP-4*(m+1)]
 *   ...
 *   temp k                       [FP-4*(m+k)]  <-- stack pointer, [SP]
 *   ---------------- if CALL..
 *   argument n'
 *   ...
 *   argument 1
 *   static link
 *   return address
 * ------------------------------ ..next frame will look like this
 *   old frame pointer
 *   ...
 * ------------------------------
 * v lower addresses
 *
 *)

structure Sy = Symbol
structure Tm = Temp
structure Tr = Tree
structure A = Assem

exception Bug
exception TODO

fun raiseBug msg = (print ("Bug: " ^ msg ^ "\n"); raise Bug)

type frame = { name: Tm.label           (* label of code of function *)
             , formals: bool list       (* for each formal, does it escape? *)
             , locals: int ref}         (* number of local vars *)

datatype access =
         InFrame of int                 (* value in memory, offset from FP *)
       | InReg of Tm.temp               (* value in register *)

datatype frag =
         PROC of {body: Tr.stm, frame: frame}
       | STRING of Tm.label * string

val FP = Tm.newtemp ()                  (* Frame pointer: %ebp *)
val SP = Tm.newtemp ()                  (* Stack pointer: %esp *)
val RV = Tm.newtemp ()                  (* Return value: %eax *)

val EAX = RV                            (* %eax: add, mult, .. *)
val EBX = Tm.newtemp ()                 (* %ebx: base of array *)
val ECX = Tm.newtemp ()                 (* %ecx: count of loop *)
val EDX = Tm.newtemp ()                 (* %edx: data *)
val ESI = Tm.newtemp ()                 (* %esi: source index (string cpy) *)
val EDI = Tm.newtemp ()                 (* %edi: destination index *)
val EIP = Tm.newtemp ()                 (* %eip: instruction pointer *)

val wordSize = 4                        (* 32 bit words *)

(* Register Lists - note that we do _not_ perform register
 * allocation as described in the textbook, so we leave the
 * '..saves' lists empty because no action will be needed to
 * save the registers around function invocations; the
 * following register list definitions must be modified
 * extensively in order to fit in with register allocation,
 * in case such a phase is added; also, 'allregs' must be
 * extended to include extra registers if any such are used
 * in the generated code (the x86 has various other registers,
 * e.g., used for floating points or MMX instructions, etc.etc.
 * but we have no intention to use assembly instructions that
 * involve those registers) *)

val allregs = [EAX, EBX, ECX, EDX, ESI, EDI, EIP, FP, SP]
val specialregs = [FP, SP, RV]
val argregs = nil
val calleesaves = nil
val callersaves = nil
val calldefs = callersaves @ [RV]

fun getEmptyRegister lst = let
    fun aux lst (r::rs) = if (List.exists (fn reg => r=reg) lst)
                            then aux lst rs 
                            else r
        | aux _ [] = raise Bug
in
    aux lst [EAX, EBX, ECX, EDX]
end

fun isRegister tmp =
    List.exists (fn reg => tmp=reg) allregs

fun newFrame {name, formals} =
    {name=name, formals=formals, locals = ref 0}

fun frameSize (f: frame) =
    let
        val overhead = 4 (* old FP *)
        val localsSize = wordSize * (!(#locals f))
    in
        overhead + localsSize
    end

fun name (fr:frame) = #name fr

fun offsetOfFormal nr = 8 + nr * wordSize

fun accessOfFormal nr escaping =
    if escaping then InFrame (offsetOfFormal nr)
    else InReg (Tm.newtemp ())

fun formals (fr:frame) =
    let
        fun accessesOfFormals _ [] = []
          | accessesOfFormals nr (h::r) =
            accessOfFormal nr h :: accessesOfFormals (nr+1) r
    in
        accessesOfFormals 0 (#formals fr)
    end

fun allocLocal (f:frame) escapes =
    let
        val noOfLocals = #locals f
    in
        if escapes
        then ( noOfLocals := !noOfLocals + 1
             ; InFrame (~(!noOfLocals) * wordSize))
        else InReg (Tm.newtemp ())
    end

fun staticLinkOffset _ =
    (* on p127 the static link is at the frame pointer, but we
     * store the old frame pointer there and the static link two
     * steps up *)
    8

fun number2formal nr =
    Tr.MEM (Tr.BINOP ( Tr.PLUS
                     , Tr.TEMP FP
                     , Tr.CONST (offsetOfFormal nr)))

fun exp (InFrame k) fp =
    Tr.MEM (Tr.BINOP (Tr.PLUS, fp, Tr.CONST k))
  | exp (InReg temp) _ =
    Tr.TEMP temp

fun asEncode s =
    let
        fun ase [] = []
          | ase (#"\\"::cs) = #"\\" :: #"\\" :: (ase cs)
          | ase (#"\""::cs) = #"\\" :: #"\"" :: (ase cs)
          | ase (c::cs) =
            let
                val ordc = ord(c)
                val charsc = Int.fmt StringCvt.OCT ordc
            in
                if 32 <= ordc andalso ordc < 128 then c :: (ase cs)
                else (#"\\" :: (explode charsc)) @ (ase cs)
            end
    in
        (implode o ase o explode) s
    end

fun string (label, str) =
    "\t.data\n" ^
    Sy.name label ^ ":\n" ^
    "\t.long " ^ Int.toString (size str) ^ "\n" ^
    "\t.asciz \"" ^ (asEncode str) ^ "\"\n"

fun move (reg, var) = Tr.MOVE (Tr.TEMP reg, Tr.TEMP var)

fun seq [] = Tr.EXP (Tr.CONST 0)
  | seq [exp] = exp
  | seq (exp :: exps) = (Tr.SEQ (exp, (seq exps)))

fun externalCall (str, args) =
    (* the gcc generated code expects arguments pushed,
     * then the return address (due to CALL instruction),
     * and it will initially push the frame pointer; it
     * delivers the return value in register EAX; hence
     * it uses the same layout as the one we use for Tiger,
     * except that it omits the static link *)
    Tr.CALL (Tr.NAME (Tm.namedLabel str), args)

fun frameInfo {name, formals, locals} =
    "# FRAME " ^
    Sy.name name ^ "(" ^
    Int.toString (length formals) ^ " formals, " ^
    Int.toString (!locals) ^
    " locals)"

fun instr2temps (A.OPER {dst, src, ...}) = dst @ src
  | instr2temps (A.LABEL _) = nil
  | instr2temps (A.MOVE {dst, src, ...}) = [dst, src]

fun createTempOffsetMap (f: frame) (body: A.instr list) =
    (* create an int Temp.Table.table that maps each temp used
     * in body into its offset; along the way, allocate the space
     * for the temps in the frame *)
    let
        val tempsMayDup = List.concat (map instr2temps body)
        fun undup ([]: Temp.temp list) = []
          | undup (x::xs) =
            if List.exists (fn y => x=y) xs then undup xs else x :: undup xs
        val temps = undup tempsMayDup
        fun addTmp (tmp, toMap) =
            if isRegister tmp then toMap else
            let
                val (InFrame offset) = allocLocal f true
            in
                Temp.Table.enter (toMap, tmp, offset)
            end
    in
        foldl addTmp Temp.Table.empty temps
    end

fun spillAllTemps toMap body =
    (* Using the temp-to-offset map 'toMap', transform the
     * instructions in 'body' such that every temp is located
     * InFrame; note that we only ever use `s0, `s1, `d0 in
     * generated assembly, which means that we can safely use
     * certain registers locally: %ebx, %ecx, and %edx.  We will
     * load and use these registers within that short list of
     * instructions that we create in each case, thus ensuring
     * that it makes no difference what they were used for
     * before, and causes no harm that they will be arbitrarily
     * overwritten.  Also note that even though some instructions
     * have (length dst > 1) we never use other elements than the
     * head, because there is no `dk for any k>0; similarly,
     * OPER instructions with (length src > 2) will only use at
     * most the first two elements.
     *
     * Note that `s0 needs to be loaded to a register which will
     * then be used; `d0 needs to be loaded to a register R, then
     * R is operated upon, then its value is stored to `d0.
     *
     * Sanity checks: for 'assem = assem' instructions with OPER, the
     * src/dst lists must have same length as the original instruction,
     * but the non-register temps have been changed to registers.  For
     * the added 'movl' instructions, length src + length dst = 1.
     * With MOVE, we change src,dst by replacing 0/1/2 of them to
     * EBX (one other temp) EBX,ECX (two).
     *)
    let
        fun ofs tmp =
            case Temp.Table.look (toMap, tmp)
             of SOME offset => if offset >= 0
                               then Int.toString offset
                               else "-" ^ Int.toString (~offset)
              | NONE => raiseBug "Encountered unknown temp in spillAllTemps"
        fun expand (A.OPER {src=s0::ss, jump = SOME (j::js), ...}) =
            raiseBug "Encountered OPER that uses temps and jumps"
          | expand (i as (A.OPER {src=[], dst=[], ...})) =
            [i]
          | expand (i as A.OPER {assem, src=[s0], dst=[], jump, doc}) =
            if isRegister s0 then [i]
            else (* s0 other temp *)
                [ A.OPER { assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                         , src = []
                         , dst = [EBX]
                         , jump = NONE
                         , doc = doc ^ " x86frame:273"}
                , A.OPER { assem = assem
                         , src = [EBX]
                         , dst = []
                         , jump = jump
                         , doc = doc ^ " x86frame:278"}]
          | expand (i as A.OPER {assem, src=(s0::s1::ss), dst=[], jump, doc}) =
            if isRegister s0 then
                if isRegister s1 then [i]
                else (* s0 register, s1 other temp *)
                    let
                        val s1' = getEmptyRegister [s0]
                    in
                        [   A.MOVE  {   assem = "\tmovl `" ^ ofs s1 ^ "(%ebp), `d0"
                                    ,   src = s1
                                    ,   dst = s1'
                                    ,   doc = doc ^ " x86frame:289"},
                            A.OPER  {   assem = assem
                                    ,   src = s0::s1'::ss
                                    ,   dst = []
                                    ,   jump = jump
                                    ,   doc = doc ^ " x86frame:294"}]
                    end
            else if isRegister s1 then
                (* s0 other temp, s1 register *)
                let
                    val s0' = getEmptyRegister [s1]
                in
                    [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                ,   src = s0
                                ,   dst = s0'
                                ,   doc = doc ^ " x86frame:304"},
                        A.OPER  {   assem = assem
                                ,   src = s0'::s1::ss
                                ,   dst = []
                                ,   jump = jump
                                ,   doc = doc ^ " x86frame:309"}]
                end
            else (* s0,s1 other temps *)
                [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                            ,   src = s0
                            ,   dst = EAX
                            ,   doc = doc ^ " x86frame:315"},
                    A.MOVE  {   assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                            ,   src = s1
                            ,   dst = EDX
                            ,   doc = doc ^ " x86frame:319"},
                    A.OPER  {   assem = assem
                            ,   src = EAX::EDX::ss
                            ,   dst = []
                            ,   jump = jump
                            ,   doc = doc ^ " x86frame:324"}]
          | expand (i as A.OPER {assem, src=[], dst=(d0::ds), jump, doc}) =
            if isRegister d0 then [i]
            else (* d0 other temp *)
                [ 
                    A.MOVE  {   assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                            ,   src = d0
                            ,   dst = EAX
                            ,   doc = doc ^ " x86frame:332"},
                    A.OPER  {   assem = assem 
                            ,   src = []
                            ,   dst = EAX::ds
                            ,   jump = jump
                            ,   doc = doc ^ " x86frame:337" },
                    A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                            ,   src = EAX
                            ,   dst = d0
                            ,   doc = doc ^ " x86frame:341"}]
          | expand (i as A.OPER {assem, src=[s0], dst=(d0::ds), jump, doc}) =
            if isRegister s0 then
                if isRegister d0 then [i]
                else (* s0 register, d0 other temp *)
                    let
                        val d = getEmptyRegister [s0]
                    in
                    [A.OPER {assem = assem
                            , src = [s0]
                            , dst = (d::ds)
                            , jump = jump
                            , doc = doc ^ " x86frame:353"}, 
                    A.MOVE { assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                            , src = d
                            , dst = d0
                            , doc = doc ^ " x86frame:357"}]
                    end
            else if isRegister d0 then
                (* s0 other temp, d0 register *)
                [ A.MOVE {    assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                            , src = s0
                            , dst = EBX
                            , doc = doc ^ " x86frame:364"},

                 A.OPER {     assem = assem
                            , src = [EBX]
                            , dst = [d0]
                            , jump = jump
                            , doc = doc ^ " x86frame:370"}]
            else (* s0,d0 other temp *)
                (* OLD_R_OPTIMIZATION *)
                if s0=d0 then
                    [ A.MOVE { assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                             , src = s0
                             , dst = EAX
                             , doc = doc ^ " x86frame:377"},
                     A.OPER { assem = assem
                            (* Exploit source and destination is the same*)
                            , src = [EAX]
                            , dst = EAX::ds
                            , jump = jump
                            , doc = doc ^ " x86frame:383"},
                     A.MOVE { assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                            , src = EAX
                            , dst = d0
                            , doc = doc ^ " x86frame:387"}
                    ]
                else (* s0<>d0, and instruction does not use old-d0 *)
                    [ A.MOVE { assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                             , src = s0
                             , dst = EBX
                             , doc = doc ^ " x86frame:393" },
                     A.MOVE { assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                             , src = d0
                             , dst = EAX
                             , doc = doc ^ " x86frame:397" },
                     A.OPER { assem = assem
                            , src = [EBX]
                            , dst = (EAX::ds)
                            , jump = jump
                            , doc = doc ^ " x86frame:402"},
                     A.MOVE { assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                            , src = EAX
                            , dst = d0
                            , doc = doc ^ " x86frame:406"}]
          | expand (i as A.OPER { assem, src=(s0::s1::ss)
                                , dst=(d0::ds), jump, doc}) =
            if isRegister s0 then
                if isRegister s1 then
                    if isRegister d0 then [i]
                    else (* s0,s1 register, d0 other temp *)
                        (* OLD_R_OPTIMIZATION: s0<>d0, no pre-load needed *)
                        let
                            val d0' = getEmptyRegister[s0,s1]
                        in
                            [   A.MOVE  {   assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                                        ,   src = d0
                                        ,   dst = d0'
                                        ,   doc = doc ^ " x86frame:420"},
                                A.OPER  {   assem = assem
                                        ,   src = s0::s1::ss
                                        ,   dst = d0'::ds
                                        ,   jump = jump
                                        ,   doc = doc ^ " x86frame:425"},
                                A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                        ,   src = d0'
                                        ,   dst = d0
                                        ,   doc = " x86frame:429"}]
                        end
                else (* s0 register, s1 other temp *)
                    if isRegister d0 then
                        (* s0 register, s1 other temp, d0 register *)
                        let
                            val s1' = getEmptyRegister [s0, d0]
                        in
                            [   A.MOVE  {   assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                                        ,   src = s1
                                        ,   dst = s1'
                                        ,   doc = doc ^ " x86frame:440"},
                                A.OPER  {   assem = assem
                                        ,   src = s0::s1'::ss
                                        ,   dst = d0::ds
                                        ,   jump = jump
                                        ,   doc = doc ^ " x86frame:445"}]
                        end
                    else (* s0 register, s1,d0 other temp *)
                        let
                            val d0' = getEmptyRegister [s1]
                            val s1' = getEmptyRegister [s1, d0']
                        in
                            [   A.MOVE  {   assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                                        ,   src = s1
                                        ,   dst = s1'
                                        ,   doc = doc ^ " x86frame:455"},
                                A.MOVE  {   assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                                        ,   src = d0
                                        ,   dst = d0'
                                        ,   doc = doc ^ " x86frame:459"},
                                A.OPER  {   assem = assem
                                        ,   src = s0::s1'::ss
                                        ,   dst = d0'::ds
                                        ,   jump = jump
                                        ,   doc = doc ^ " x86frame:464"},
                                A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                        ,   src = d0'
                                        ,   dst = d0
                                        ,   doc = " x86frame:468"}]
                        end
            else (* s0 other temp *)
                if isRegister s1 then
                    if isRegister d0 then
                        (* s0 other temp, s1,d0 register *)
                        let val s = getEmptyRegister [s1,d0]
                        in
                            [ A.MOVE { assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                             , src = s0
                             , dst = s
                             , doc = doc ^ " x86frame:479" },
                             A.OPER { assem = assem
                             , src = (s::s1::ss)
                             , dst = (d0::ds)
                             , jump = jump
                             , doc = doc ^ " x86frame:484"} ]
                         end
                    else (* s0 other temp, s1 register, d0 other temp *)
                        (* OLD_R_OPTIMIZATION *)
                        if s0=d0 then
                            (* instruction uses old-d0, and "`s0" is
                             * not used in assem; must preload d0 *)
                            let
                                val s0' = getEmptyRegister [s1]
                            in
                                [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                            ,   src = s0
                                            ,   dst = s0'
                                            ,   doc = doc ^ " x86frame:497"},
                                    A.OPER  {   assem = assem
                                            ,   src = s0'::s1::ss
                                            ,   dst = s0'::ds
                                            ,   jump = jump
                                            ,   doc = doc ^ " x86frame:502"},
                                    A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                            ,   src = s0'
                                            ,   dst = d0
                                            ,   doc = doc ^ " x86frame:506"}]
                            end
                        else (* s0<>d0, and instruction does not use old-d0 *)
                            let
                                val s0' = getEmptyRegister [s1]
                                val d0' = getEmptyRegister [s0', s1]
                            in
                                [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                            ,   src = s0
                                            ,   dst = s0'
                                            ,   doc = doc ^ " x86frame:516"},
                                    A.MOVE  {   assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                                            ,   src = d0
                                            ,   dst = d0'
                                            ,   doc = doc ^ " x86frame:520"},
                                    A.OPER  {   assem = assem
                                            ,   src = s0'::s1::ss
                                            ,   dst = d0'::ds
                                            ,   jump = jump
                                            ,   doc = doc ^ " x86frame:525"},
                                    A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                            ,   src = d0'
                                            ,   dst = d0
                                            ,   doc = doc ^ " x86frame:529"}]
                            end
                else if isRegister d0 then
                    (* s1, s2 are other temp *)
                    let
                        val s0' = getEmptyRegister [d0]
                        val s1' = getEmptyRegister [d0,s0']
                    in
                        [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                    ,   src = s0
                                    ,   dst = s0'
                                    ,   doc = doc ^ " x86frame:540"},
                            A.MOVE  {   assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                                    ,   src = s1
                                    ,   dst = s1'
                                    ,   doc = doc ^ " x86frame:544"},
                            A.OPER  {   assem = assem
                                    ,   src = s0'::s1'::ss
                                    ,   dst = d0::ds
                                    ,   jump = jump
                                    ,   doc = doc ^ " x86frame:549"}]
                    end
                else (* s0,s1,d0 other temp *)
                    (* OLD_R_OPTIMIZATION *)
                    if s0=d0 then
                        (* instruction uses old-d0, and "`s0" is
                         * not used in assem; must preload d0 *)
                        [   A.MOVE  {   assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                    ,   src = s0
                                    ,   dst = EAX
                                    ,   doc = doc ^ " x86frame:559"},
                            A.MOVE  {   assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                                    ,   src = s1
                                    ,   dst = EBX
                                    ,   doc = doc ^ " x86frame:563"},
                            A.OPER  {   assem = assem
                                    ,   src = EAX::EBX::ss
                                    ,   dst = EAX::ds
                                    ,   jump = jump
                                    ,   doc = doc ^ " x86frame:568"},
                            A.MOVE  {   assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                    ,   src = EAX
                                    ,   dst = d0
                                    ,   doc = doc ^ " x86frame:572"}]
                     else (* s0<>d0, and instruction does not use old-d0 *)
                         [  A.MOVE {    assem = "\tmovl " ^ ofs s0 ^ "(%ebp), `d0"
                                    ,   src = s0
                                    ,   dst = EBX
                                    ,   doc = doc ^ " x86frame:577"},
                            A.MOVE {    assem = "\tmovl " ^ ofs s1 ^ "(%ebp), `d0"
                                    ,   src = s1
                                    ,   dst = EDX
                                    ,   doc = doc ^ " x86frame:581"},
                            A.MOVE {    assem = "\tmovl " ^ ofs d0 ^ "(%ebp), `d0"
                                    ,   src = d0
                                    ,   dst = EAX
                                    ,   doc = doc ^ " x86frame:585"},
                            A.OPER {    assem = assem
                                    ,   src = (EBX::EDX::ss)
                                    ,   dst = (EAX::ds)
                                    ,   jump = jump
                                    ,   doc = doc ^ " x86frame:590"},
                            A.MOVE {    assem = "\tmovl `s0, " ^ ofs d0 ^ "(%ebp)"
                                    ,   src = EAX
                                    ,   dst = d0
                                    ,   doc = doc ^ " x86frame:594"}]
          | expand (i as (A.LABEL _)) =
            [i]
          | expand (i as A.MOVE {assem, src, dst, doc}) =
            (* we ignore assem because src/dst reveals the semantics
             * of the A.MOVE exactly, so we can reconstruct it *)
            if isRegister src then
                if isRegister dst then [i]
                else (* src register, dst other temp *)
                    [A.MOVE {  assem = "\tmovl `s0, " ^ ofs dst ^ "(%ebp)"
                            ,  dst = dst
                            ,  src = src
                            ,  doc = doc ^ " x86frame:606" }]
            else if isRegister dst then
                (* src other temp, dst register *)
                [A.MOVE {  assem = "\tmovl " ^ ofs src ^ "(%ebp), `d0"
                            , dst = dst
                            , src = src
                            , doc = doc ^ "x86frame:612"}]
            else
                (* src, dst other temp *)
                [ A.MOVE {  assem = "\tmovl " ^ ofs src ^ "(%ebp), `d0"
                         ,  dst = EBX
                         ,  src = src
                         ,  doc = doc ^ "x86frame:618"},
                 A.MOVE {   assem = "\tmovl `s0, " ^ ofs dst ^ "(%ebp)"
                        ,   dst = dst
                        ,   src = EBX
                        ,   doc = doc ^ "x86frame:622"}]
    in
        List.concat (map expand body)
    end

fun entryExit1 adjustBody (frame,body) =
    let
        (* extend body to save 'callee save' registers in temporaries *)
        val toSave = calleesaves
        val saved = map (fn _ => Temp.newtemp ()) toSave
        val saveIR = seq (ListPair.mapEq move (saved, toSave))
        val restoreIR = seq (ListPair.mapEq move (toSave, saved))
        val body' = seq [saveIR, adjustBody body, restoreIR]
        val funFormals = formals frame
        (* perform 'view shift': copy arguments passed in registers
         * to frame or temp---but we do not pass any arguments in
         * registers, so this will currently not do anything *)
        fun moveArg (arg, access) =
            Tr.MOVE (exp access (Tr.TEMP FP), Tr.TEMP arg)
        val viewShift = seq (ListPair.map moveArg (argregs, funFormals))
    in
        case funFormals
         of [] => body'
          | _  => Tr.SEQ (viewShift, body')
    end

fun funEntryExit1 (frame, body) =
    entryExit1 (fn body => Tr.MOVE (Tr.TEMP RV, body)) (frame, body)

fun procEntryExit1 (frame, body) =
    entryExit1 (fn body => body) (frame, body)

(* inform register allocator about register usage *)
fun procEntryExit2 (frame, body) =
    (A.OPER { assem = "\t# SP, FP, calleesaves, argregs have values"
            , src = []
            , dst = [SP, FP] @ calleesaves @ argregs
            , jump = NONE
            , doc = ""})
    :: body
    @ [A.OPER { assem = "\t# FP, SP, RV, calleesaves still live"
              , src = specialregs @ calleesaves
              , dst = []
              , jump = SOME []
              , doc = ""}]

fun procEntryExit3 ( frame as {name, formals, locals}: frame
                   , body : A.instr list) =
    let
        val toMap = createTempOffsetMap frame body
        val body' = spillAllTemps toMap body
        val size = frameSize frame
        val id = Sy.name name
    in
        { prolog = "\t.text\n" ^
                   "# PROCEDURE " ^ id ^ "\n" ^
                   "\t.globl\t" ^ id ^ "\n" ^
                   "\t.func\t" ^ id ^ "\n" ^
                   "\t.type\t" ^ id ^ ", @function\n" ^
                   id ^ ":\n" ^
                   "\t" ^ frameInfo frame ^ "\n" ^
                   "\tpushl %ebp\n" ^
	           "\tmovl %esp, %ebp\n" ^
	           "\tsubl $" ^ Int.toString (size-4) ^ ", %esp\n"
        , body = body'
        , epilog = "\tleave\n" ^
                   "\tret\n" ^
                   "\t.size\t" ^ id ^ ", .-" ^ id ^ "\n" ^
                   "\t.endfunc\n" ^
                   "# END " ^ id ^ "\n\n"}
    end

(* ---------- Printing ---------- *)

val tempMap = Temp.Table.fromList [ (FP,  "%ebp")
                                  , (SP,  "%esp")
                                  , (EAX, "%eax")
                                  , (EBX, "%ebx")
                                  , (ECX, "%ecx")
                                  , (EDX, "%edx")
                                  , (ESI, "%esi")
                                  , (EDI, "%edi")
                                  , (EIP, "%eip")]

fun asStringReg r =
    case Temp.Table.look (tempMap, r)
     of SOME regname => regname
      | NONE => Temp.makestring r

fun asStringFrame (frame:frame) =
    "{name = " ^
    Sy.name (#name frame) ^
    ", formals = " ^
    concat (map (fn b => if b then "t" else "f") (#formals frame)) ^
    ", locals = " ^
    Int.toString (!(#locals frame)) ^
    "}"

fun asStringAccess (InFrame i) =
    "InFrame(" ^
    Int.toString i ^
    ")"
  | asStringAccess (InReg tmp) =
    "InReg(" ^
    Tm.makestring tmp ^
    ")"

fun printx asStringX (outstream, x) =
    ( TextIO.output (outstream, asStringX x ^ "\n")
    ; TextIO.flushOut outstream)

fun printFrame (outstream, f)  = printx asStringFrame  (outstream, f)
fun printAccess (outstream, a) = printx asStringAccess (outstream, a)

end (* X86Frame *)
