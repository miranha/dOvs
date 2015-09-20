functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol

(* [these functions and similar ones may be convenient
 * for the creation of abstract syntax trees] *)

datatype lvaluePartSpec = Field of S.symbol
                        | Subscript of A.exp

fun makeLvaluePartSpec (v, pos, l::r) =
  (case l
    of Field idsym =>
       makeLvaluePartSpec (A.FieldVar (v, idsym, pos), pos, r)
     | Subscript exp =>
       makeLvaluePartSpec (A.SubscriptVar (v, exp, pos), pos,r))
  | makeLvaluePartSpec (v, _, nil) = v

fun makeBinop (e1, bop, e2, p) =
    A.OpExp  { left = e1
             , oper = bop
             , right = e2
             , pos = p}

fun makeIf (et, en, el, p) =
    A.IfExp  { test = et
             , thn = en
             , els = el
             , pos = p}

fun makeVarDec (idsym, ty, e, p) =
    A.VarDec { name = idsym
             , escape = ref true
             , typ = ty
             , init = e
             , pos = p}

fun makeFundecl (idsym, ps, rty, e, p) =
             { name = idsym
             , params = ps
             , result = rty
             , body = e
             , pos = p} : A.fundecldata


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\028\000\003\000\027\000\004\000\026\000\008\000\025\000\
\\016\000\024\000\030\000\023\000\033\000\022\000\034\000\021\000\
\\041\000\020\000\042\000\019\000\000\000\
\\001\000\002\000\036\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\009\000\056\000\000\000\
\\001\000\009\000\074\000\000\000\
\\001\000\011\000\065\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\025\000\031\000\000\000\
\\001\000\011\000\073\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\025\000\031\000\000\000\
\\001\000\013\000\071\000\000\000\
\\001\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\031\000\055\000\000\000\
\\001\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\035\000\076\000\000\000\
\\001\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\036\000\054\000\000\000\
\\001\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\036\000\085\000\000\000\
\\001\000\019\000\072\000\000\000\
\\001\000\028\000\053\000\000\000\
\\001\000\040\000\080\000\000\000\
\\088\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\010\000\030\000\014\000\029\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\017\000\033\000\018\000\032\000\025\000\031\000\000\000\
\\107\000\017\000\033\000\018\000\032\000\025\000\031\000\000\000\
\\108\000\025\000\031\000\000\000\
\\109\000\025\000\031\000\000\000\
\\110\000\025\000\031\000\000\000\
\\111\000\017\000\033\000\018\000\032\000\025\000\031\000\000\000\
\\112\000\000\000\
\\113\000\007\000\057\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\025\000\031\000\000\000\
\\114\000\000\000\
\\115\000\002\000\028\000\003\000\027\000\004\000\026\000\008\000\025\000\
\\016\000\024\000\030\000\023\000\033\000\022\000\034\000\021\000\
\\041\000\020\000\042\000\019\000\000\000\
\\116\000\000\000\
\\117\000\005\000\075\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\018\000\032\000\025\000\031\000\000\000\
\\118\000\000\000\
\\119\000\002\000\028\000\003\000\027\000\004\000\026\000\008\000\025\000\
\\016\000\024\000\030\000\023\000\033\000\022\000\034\000\021\000\
\\041\000\020\000\042\000\019\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\032\000\077\000\000\000\
\\124\000\000\000\
\\139\000\008\000\045\000\010\000\044\000\012\000\043\000\028\000\042\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\005\000\070\000\000\000\
\\147\000\000\000\
\\148\000\002\000\061\000\000\000\
\\149\000\015\000\035\000\016\000\034\000\017\000\033\000\018\000\032\000\
\\025\000\031\000\000\000\
\"
val actionRowNumbers =
"\001\000\026\000\027\000\029\000\
\\028\000\022\000\023\000\033\000\
\\021\000\025\000\032\000\031\000\
\\024\000\055\000\054\000\030\000\
\\016\000\019\000\020\000\002\000\
\\001\000\001\000\001\000\043\000\
\\018\000\017\000\053\000\003\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\014\000\011\000\
\\009\000\039\000\004\000\041\000\
\\001\000\062\000\001\000\047\000\
\\057\000\006\000\038\000\037\000\
\\036\000\035\000\034\000\001\000\
\\001\000\001\000\040\000\043\000\
\\048\000\060\000\008\000\013\000\
\\007\000\005\000\045\000\056\000\
\\010\000\050\000\051\000\042\000\
\\062\000\059\000\001\000\015\000\
\\044\000\047\000\001\000\001\000\
\\061\000\063\000\001\000\046\000\
\\012\000\052\000\058\000\001\000\
\\049\000\000\000"
val gotoT =
"\
\\001\000\085\000\002\000\016\000\014\000\015\000\015\000\014\000\
\\016\000\013\000\017\000\012\000\019\000\011\000\020\000\010\000\
\\023\000\009\000\024\000\008\000\026\000\007\000\028\000\006\000\
\\029\000\005\000\030\000\004\000\031\000\003\000\032\000\002\000\
\\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\035\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\036\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\037\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\039\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\025\000\038\000\026\000\007\000\028\000\006\000\
\\029\000\005\000\030\000\004\000\031\000\003\000\032\000\002\000\
\\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\045\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\046\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\047\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\048\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\049\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\050\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\056\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\021\000\058\000\022\000\057\000\000\000\
\\002\000\060\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\062\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\018\000\061\000\019\000\011\000\020\000\010\000\
\\023\000\009\000\024\000\008\000\026\000\007\000\028\000\006\000\
\\029\000\005\000\030\000\004\000\031\000\003\000\032\000\002\000\
\\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\064\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\065\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\066\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\002\000\039\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\025\000\067\000\026\000\007\000\028\000\006\000\
\\029\000\005\000\030\000\004\000\031\000\003\000\032\000\002\000\
\\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\076\000\022\000\057\000\000\000\
\\000\000\
\\002\000\077\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\062\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\018\000\079\000\019\000\011\000\020\000\010\000\
\\023\000\009\000\024\000\008\000\026\000\007\000\028\000\006\000\
\\029\000\005\000\030\000\004\000\031\000\003\000\032\000\002\000\
\\033\000\001\000\000\000\
\\002\000\080\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\002\000\081\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\082\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\084\000\014\000\015\000\015\000\014\000\016\000\013\000\
\\017\000\012\000\019\000\011\000\020\000\010\000\023\000\009\000\
\\024\000\008\000\026\000\007\000\028\000\006\000\029\000\005\000\
\\030\000\004\000\031\000\003\000\032\000\002\000\033\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 86
val numrules = 62
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "CARET"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID seqexp1, seqexp1left, seqexp1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 seqexp1 = seqexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, seqexp1left, seqexp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID unaryop1, unaryop1left, unaryop1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  unaryop1 = unaryop1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, unaryop1left, unaryop1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.ntVOID binop1, binop1left, binop1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
binop1 = binop1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, binop1left, binop1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID callexp1, callexp1left, callexp1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  callexp1 = callexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, callexp1left, callexp1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.ntVOID assignment1, assignment1left, 
assignment1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  assignment1 = assignment1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, assignment1left, assignment1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID forexp1, forexp1left, forexp1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 forexp1 = forexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, forexp1left, forexp1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID whileexp1, whileexp1left, 
whileexp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  whileexp1 = whileexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, whileexp1left, whileexp1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.ntVOID ifexp1, ifexp1left, ifexp1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
ifexp1 = ifexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, ifexp1left, ifexp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID ifelseexp1, ifelseexp1left, 
ifelseexp1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ifelseexp1 = ifelseexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, ifelseexp1left, ifelseexp1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 lvalue1 = lvalue1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID arrcreate1, arrcreate1left, 
arrcreate1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  arrcreate1 = arrcreate1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, arrcreate1left, arrcreate1right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID reccreate1, reccreate1left, 
reccreate1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  reccreate1 = reccreate1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, reccreate1left, reccreate1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID letexp1, letexp1left, letexp1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 letexp1 = letexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, letexp1left, letexp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 28, ( result, MINUS1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
seqsexps1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  seqsexps1 = 
seqsexps1 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, exp1left, exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID seqsexps1, _, seqsexps1right)) :: _
 :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  seqsexps1 = seqsexps1 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, exp1left, seqsexps1right), rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 24, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
callexpcon1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  callexpcon1 = callexpcon1 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, exp1left, exp1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID callexpcon1, _, callexpcon1right))
 :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  callexpcon1 = callexpcon1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, exp1left, callexpcon1right), rest671)

end
|  ( 31, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, ID1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 32, ( result, FOR1left, exp3right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1
 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 31, ( result, WHILE1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 29, ( result, IF1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, IF1left, exp3right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, TYPE1left, ty1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ntVOID arrty1, arrty1left, arrty1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
arrty1 = arrty1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, arrty1left, arrty1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID recty1, recty1left, recty1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
recty1 = recty1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, recty1left, recty1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
fielddeccontent1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  
fielddeccontent1 = fielddeccontent1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.ntVOID fielddec1, fielddec1left, 
fielddec1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  fielddec1 = fielddec1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, fielddec1left, fielddec1right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.ntVOID fielddeccontent1, _, 
fielddeccontent1right)) :: _ :: ( _, ( MlyValue.ntVOID fielddec1, 
fielddec1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  fielddec1 = fielddec1 ()
 val  fielddeccontent1 = fielddeccontent1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, fielddec1left, fielddeccontent1right), 
rest671)
end
|  ( 45, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, ID1left, ID2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID fielddeccontent1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  fielddeccontent1 = fielddeccontent1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID 
fielddeccontent1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  fielddeccontent1 = fielddeccontent1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, ID1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, ID1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID subscript1, subscript1left, 
subscript1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  subscript1 = subscript1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, subscript1left, subscript1right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.ntVOID fieldexp1, fieldexp1left, 
fieldexp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  fieldexp1 = fieldexp1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, fieldexp1left, fieldexp1right), rest671
)
end
|  ( 54, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, lvalue1left, ID1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, ID1left, exp2right), rest671)
end
|  ( 57, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
reccreatecon1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  reccreatecon1 = reccreatecon1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ntVOID fieldcreate1, fieldcreate1left, 
fieldcreate1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  fieldcreate1 = fieldcreate1 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, fieldcreate1left, fieldcreate1right), 
rest671)
end
|  ( 59, ( ( _, ( MlyValue.ntVOID reccreatecon1, _, reccreatecon1right
)) :: _ :: ( _, ( MlyValue.ntVOID fieldcreate1, fieldcreate1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 fieldcreate1 = fieldcreate1 ()
 val  reccreatecon1 = reccreatecon1 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, fieldcreate1left, reccreatecon1right), 
rest671)
end
|  ( 60, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, ID1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
