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
\\001\000\001\000\216\000\005\000\216\000\007\000\216\000\009\000\216\000\
\\011\000\216\000\013\000\216\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\216\000\026\000\216\000\
\\030\000\216\000\031\000\216\000\034\000\216\000\035\000\216\000\
\\037\000\216\000\038\000\216\000\042\000\216\000\043\000\216\000\
\\044\000\216\000\047\000\020\000\000\000\
\\001\000\001\000\217\000\005\000\217\000\007\000\217\000\009\000\217\000\
\\011\000\217\000\013\000\217\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\217\000\026\000\217\000\
\\030\000\217\000\031\000\217\000\034\000\217\000\035\000\217\000\
\\037\000\217\000\038\000\217\000\042\000\217\000\043\000\217\000\
\\044\000\217\000\047\000\020\000\000\000\
\\001\000\001\000\218\000\005\000\218\000\007\000\218\000\009\000\218\000\
\\011\000\218\000\013\000\218\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\218\000\026\000\218\000\
\\030\000\218\000\031\000\218\000\034\000\218\000\035\000\218\000\
\\037\000\218\000\038\000\218\000\042\000\218\000\043\000\218\000\
\\044\000\218\000\047\000\020\000\000\000\
\\001\000\001\000\219\000\005\000\219\000\007\000\219\000\009\000\219\000\
\\011\000\219\000\013\000\219\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\219\000\026\000\219\000\
\\030\000\219\000\031\000\219\000\034\000\219\000\035\000\219\000\
\\037\000\219\000\038\000\219\000\042\000\219\000\043\000\219\000\
\\044\000\219\000\047\000\020\000\000\000\
\\001\000\001\000\220\000\005\000\220\000\007\000\220\000\009\000\220\000\
\\011\000\220\000\013\000\220\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\220\000\026\000\220\000\
\\030\000\220\000\031\000\220\000\034\000\220\000\035\000\220\000\
\\037\000\220\000\038\000\220\000\042\000\220\000\043\000\220\000\
\\044\000\220\000\047\000\020\000\000\000\
\\001\000\001\000\221\000\005\000\221\000\007\000\221\000\009\000\221\000\
\\011\000\221\000\013\000\221\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\025\000\221\000\026\000\221\000\
\\030\000\221\000\031\000\221\000\034\000\221\000\035\000\221\000\
\\037\000\221\000\038\000\221\000\042\000\221\000\043\000\221\000\
\\044\000\221\000\047\000\020\000\000\000\
\\001\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\016\000\015\000\029\000\014\000\032\000\013\000\033\000\012\000\
\\036\000\011\000\040\000\010\000\041\000\009\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\075\000\000\000\
\\001\000\002\000\083\000\000\000\
\\001\000\002\000\111\000\012\000\110\000\028\000\109\000\000\000\
\\001\000\002\000\113\000\000\000\
\\001\000\002\000\116\000\000\000\
\\001\000\002\000\139\000\000\000\
\\001\000\002\000\145\000\000\000\
\\001\000\002\000\147\000\000\000\
\\001\000\002\000\150\000\000\000\
\\001\000\006\000\093\000\027\000\092\000\000\000\
\\001\000\006\000\131\000\000\000\
\\001\000\006\000\144\000\019\000\143\000\000\000\
\\001\000\007\000\082\000\009\000\081\000\015\000\032\000\016\000\031\000\
\\017\000\030\000\018\000\029\000\019\000\028\000\020\000\027\000\
\\021\000\026\000\022\000\025\000\023\000\024\000\024\000\023\000\
\\025\000\022\000\026\000\021\000\047\000\020\000\000\000\
\\001\000\008\000\094\000\000\000\
\\001\000\009\000\079\000\000\000\
\\001\000\009\000\104\000\000\000\
\\001\000\009\000\130\000\000\000\
\\001\000\011\000\103\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\021\000\026\000\
\\022\000\025\000\023\000\024\000\024\000\023\000\025\000\022\000\
\\026\000\021\000\047\000\020\000\000\000\
\\001\000\011\000\134\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\021\000\026\000\
\\022\000\025\000\023\000\024\000\024\000\023\000\025\000\022\000\
\\026\000\021\000\047\000\020\000\000\000\
\\001\000\013\000\101\000\000\000\
\\001\000\013\000\140\000\000\000\
\\001\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\030\000\078\000\047\000\020\000\000\000\
\\001\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\034\000\117\000\047\000\020\000\000\000\
\\001\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\035\000\077\000\047\000\020\000\000\000\
\\001\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\035\000\146\000\047\000\020\000\000\000\
\\001\000\019\000\091\000\000\000\
\\001\000\019\000\102\000\000\000\
\\001\000\019\000\152\000\000\000\
\\001\000\019\000\153\000\000\000\
\\001\000\027\000\076\000\000\000\
\\001\000\027\000\127\000\000\000\
\\001\000\037\000\072\000\000\000\
\\001\000\038\000\107\000\000\000\
\\001\000\039\000\125\000\000\000\
\\158\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\159\000\042\000\043\000\043\000\042\000\044\000\041\000\000\000\
\\160\000\000\000\
\\161\000\044\000\041\000\000\000\
\\162\000\000\000\
\\163\000\042\000\043\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\002\000\116\000\000\000\
\\171\000\000\000\
\\172\000\005\000\129\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\176\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\180\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\181\000\000\000\
\\182\000\008\000\054\000\010\000\053\000\012\000\052\000\014\000\051\000\000\000\
\\182\000\010\000\100\000\014\000\051\000\000\000\
\\182\000\010\000\100\000\014\000\051\000\039\000\123\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\016\000\015\000\029\000\014\000\032\000\013\000\033\000\012\000\
\\036\000\011\000\040\000\010\000\041\000\009\000\000\000\
\\186\000\000\000\
\\187\000\007\000\082\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\021\000\026\000\
\\022\000\025\000\023\000\024\000\024\000\023\000\025\000\022\000\
\\026\000\021\000\047\000\020\000\000\000\
\\188\000\000\000\
\\189\000\002\000\019\000\003\000\018\000\004\000\017\000\008\000\016\000\
\\016\000\015\000\029\000\014\000\032\000\013\000\033\000\012\000\
\\036\000\011\000\040\000\010\000\041\000\009\000\000\000\
\\190\000\000\000\
\\191\000\005\000\106\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\021\000\026\000\
\\022\000\025\000\023\000\024\000\024\000\023\000\025\000\022\000\
\\026\000\021\000\047\000\020\000\000\000\
\\192\000\000\000\
\\193\000\027\000\033\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\047\000\020\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\207\000\002\000\085\000\000\000\
\\208\000\000\000\
\\209\000\005\000\136\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\027\000\021\000\026\000\
\\022\000\025\000\023\000\024\000\024\000\023\000\025\000\022\000\
\\026\000\021\000\047\000\020\000\000\000\
\\210\000\000\000\
\\211\000\017\000\030\000\018\000\029\000\047\000\020\000\000\000\
\\212\000\017\000\030\000\018\000\029\000\047\000\020\000\000\000\
\\213\000\047\000\020\000\000\000\
\\214\000\047\000\020\000\000\000\
\\215\000\047\000\020\000\000\000\
\\222\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\047\000\020\000\000\000\
\\223\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\047\000\020\000\000\000\
\\224\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\225\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\031\000\118\000\047\000\020\000\000\000\
\\226\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\227\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\228\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\019\000\028\000\020\000\027\000\021\000\026\000\022\000\025\000\
\\023\000\024\000\024\000\023\000\025\000\022\000\026\000\021\000\
\\047\000\020\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\"
val actionRowNumbers =
"\007\000\093\000\092\000\091\000\
\\090\000\045\000\082\000\083\000\
\\112\000\046\000\008\000\007\000\
\\007\000\007\000\074\000\088\000\
\\086\000\069\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\064\000\050\000\049\000\051\000\
\\048\000\046\000\042\000\009\000\
\\010\000\011\000\040\000\034\000\
\\032\000\087\000\025\000\023\000\
\\068\000\012\000\096\000\007\000\
\\078\000\104\000\106\000\105\000\
\\005\000\003\000\006\000\004\000\
\\002\000\001\000\103\000\102\000\
\\101\000\100\000\107\000\065\000\
\\052\000\047\000\074\000\036\000\
\\020\000\024\000\007\000\007\000\
\\007\000\085\000\075\000\084\000\
\\007\000\070\000\030\000\037\000\
\\028\000\026\000\080\000\043\000\
\\076\000\013\000\007\000\014\000\
\\057\000\033\000\110\000\108\000\
\\076\000\072\000\007\000\094\000\
\\007\000\071\000\089\000\079\000\
\\007\000\113\000\053\000\044\000\
\\057\000\054\000\062\000\041\000\
\\059\000\027\000\021\000\007\000\
\\007\000\077\000\029\000\098\000\
\\073\000\007\000\080\000\016\000\
\\031\000\007\000\058\000\015\000\
\\022\000\017\000\035\000\109\000\
\\070\000\097\000\018\000\095\000\
\\081\000\056\000\055\000\063\000\
\\059\000\007\000\019\000\061\000\
\\007\000\038\000\060\000\066\000\
\\039\000\111\000\007\000\007\000\
\\098\000\067\000\099\000\000\000"
val gotoT =
"\
\\001\000\155\000\013\000\006\000\015\000\005\000\022\000\004\000\
\\023\000\003\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\038\000\003\000\037\000\004\000\036\000\005\000\035\000\
\\010\000\034\000\011\000\033\000\012\000\032\000\000\000\
\\000\000\
\\013\000\006\000\015\000\043\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\044\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\045\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\047\000\016\000\046\000\022\000\004\000\
\\023\000\003\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\014\000\048\000\000\000\
\\013\000\006\000\015\000\053\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\054\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\055\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\056\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\057\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\058\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\059\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\060\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\061\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\062\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\063\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\064\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\065\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\066\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\012\000\067\000\000\000\
\\000\000\
\\000\000\
\\005\000\068\000\000\000\
\\002\000\069\000\003\000\037\000\004\000\036\000\005\000\035\000\
\\010\000\034\000\011\000\033\000\012\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\078\000\000\000\
\\000\000\
\\000\000\
\\020\000\082\000\000\000\
\\013\000\006\000\015\000\084\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\086\000\018\000\085\000\022\000\004\000\
\\023\000\003\000\024\000\002\000\025\000\001\000\000\000\
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
\\013\000\006\000\015\000\088\000\016\000\087\000\022\000\004\000\
\\023\000\003\000\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\093\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\094\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\095\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\096\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\014\000\097\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\103\000\000\000\
\\000\000\
\\017\000\078\000\000\000\
\\006\000\106\000\000\000\
\\013\000\006\000\015\000\110\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\007\000\113\000\009\000\112\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\117\000\000\000\
\\000\000\
\\013\000\006\000\015\000\118\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\013\000\006\000\015\000\119\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\014\000\120\000\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\122\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\124\000\009\000\112\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\126\000\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\130\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\131\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\021\000\133\000\000\000\
\\000\000\
\\013\000\006\000\015\000\135\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\019\000\136\000\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\139\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\009\000\140\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\120\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\146\000\000\000\
\\013\000\006\000\015\000\147\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\149\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\006\000\015\000\152\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\013\000\006\000\015\000\153\000\022\000\004\000\023\000\003\000\
\\024\000\002\000\025\000\001\000\000\000\
\\021\000\154\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 156
val numrules = 73
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
 | ID of unit ->  (string) | control_exp of unit ->  (A.exp)
 | boolean_exp of unit ->  (A.exp)
 | comparison_exp of unit ->  (A.exp) | arith_exp of unit ->  (A.exp)
 | id_eq_seq_tail of unit ->  ( ( S.symbol * A.exp * pos )  list)
 | id_eq_seq of unit ->  ( ( S.symbol * A.exp * pos )  list)
 | function_args_tail of unit ->  ( ( A.exp * pos )  list)
 | function_args of unit ->  ( ( A.exp * pos )  list)
 | exp_seq_tail of unit ->  ( ( A.exp * pos )  list)
 | exp_seq of unit ->  ( ( A.exp * pos )  list)
 | exp of unit ->  (A.exp)
 | lvalue_tail of unit ->  (lvaluePartSpec list)
 | lvalue of unit ->  (A.var) | fundec of unit ->  (A.fundecldata)
 | fundecs of unit ->  (A.fundecldata list)
 | vardec of unit ->  (A.decl) | tyfield of unit ->  (A.fielddata)
 | tyfields_tail of unit ->  (A.fielddata list)
 | tyfields of unit ->  (A.fielddata list) | ty of unit ->  (A.ty)
 | tydec of unit ->  (A.tydecldata)
 | tydecs of unit ->  (A.tydecldata list) | dec of unit ->  (A.decl)
 | decs of unit ->  (A.decl list) | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
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
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | (T 45) => "LOW_PRECEDENCE"
  | (T 46) => "CARET"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40)
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.decs (fn _ => (nil))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in (dec :: decs)
end)
 in ( LrTable.NT 1, ( result, dec1left, decs1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.tydecs tydecs1, tydecs1left, tydecs1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
tydecs as tydecs1) = tydecs1 ()
 in (A.TypeDec (List.rev tydecs))
end)
 in ( LrTable.NT 2, ( result, tydecs1left, tydecs1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardec as vardec1) = vardec1 ()
 in (vardec)
end)
 in ( LrTable.NT 2, ( result, vardec1left, vardec1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.fundecs fundecs1, fundecs1left, 
fundecs1right)) :: rest671)) => let val  result = MlyValue.dec (fn _
 => let val  (fundecs as fundecs1) = fundecs1 ()
 in (A.FunctionDec (List.rev fundecs))
end)
 in ( LrTable.NT 2, ( result, fundecs1left, fundecs1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tydecs (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in (tydec :: nil)
end)
 in ( LrTable.NT 3, ( result, tydec1left, tydec1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) => let val  
result = MlyValue.tydecs (fn _ => let val  (tydecs as tydecs1) = 
tydecs1 ()
 val  (tydec as tydec1) = tydec1 ()
 in (tydec :: tydecs)
end)
 in ( LrTable.NT 3, ( result, tydecs1left, tydec1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (
{ name = (S.symbol ID), 
                                                ty =  ty, 
                                                pos = TYPEleft }
)
end)
 in ( LrTable.NT 4, ( result, TYPE1left, ty1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy (S.symbol ID, IDleft))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy tyfields)
end)
 in ( LrTable.NT 5, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy (S.symbol ID, ARRAYleft))
end)
 in ( LrTable.NT 5, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
nil))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.tyfields_tail tyfields_tail1, _, 
tyfields_tail1right)) :: ( _, ( MlyValue.tyfield tyfield1, 
tyfield1left, _)) :: rest671)) => let val  result = MlyValue.tyfields
 (fn _ => let val  (tyfield as tyfield1) = tyfield1 ()
 val  (tyfields_tail as tyfields_tail1) = tyfields_tail1 ()
 in (tyfield :: tyfields_tail)
end)
 in ( LrTable.NT 6, ( result, tyfield1left, tyfields_tail1right), 
rest671)
end
|  ( 14, ( rest671)) => let val  result = MlyValue.tyfields_tail (fn _
 => (nil))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( MlyValue.tyfields_tail tyfields_tail1, _, 
tyfields_tail1right)) :: ( _, ( MlyValue.tyfield tyfield1, _, _)) :: (
 _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields_tail (fn _ => let val  (tyfield as tyfield1) = 
tyfield1 ()
 val  (tyfields_tail as tyfields_tail1) = tyfields_tail1 ()
 in (tyfield :: tyfields_tail)
end)
 in ( LrTable.NT 7, ( result, COMMA1left, tyfields_tail1right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID2, ID2left, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfield (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
{ name = (S.symbol ID1),
                                                escape = ref true,
                                                typ = (S.symbol ID2, ID2left),
                                                pos = ID1left }
)
end)
 in ( LrTable.NT 8, ( result, ID1left, ID2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
makeVarDec(S.symbol ID,
                                                         NONE,
                                                         exp,
                                                         VARleft)
)
end)
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
makeVarDec(S.symbol ID1,
                                                         SOME (S.symbol ID2, ID2left),
                                                         exp,
                                                         VARleft)
)
end)
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in (fundec :: nil)
end)
 in ( LrTable.NT 10, ( result, fundec1left, fundec1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundecs as fundecs1) = 
fundecs1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (fundec :: fundecs)
end)
 in ( LrTable.NT 10, ( result, fundecs1left, fundec1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (makeFundecl (S.symbol ID, tyfields, NONE, exp, FUNCTIONleft))
end
)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
makeFundecl (S.symbol ID1, tyfields, SOME (S.symbol ID2, ID2left), exp, FUNCTIONleft)
)
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.lvalue_tail lvalue_tail1, _, 
lvalue_tail1right)) :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)
) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (lvalue_tail as lvalue_tail1) = lvalue_tail1 ()
 in (
makeLvaluePartSpec(A.SimpleVar (S.symbol ID, IDleft), IDleft, lvalue_tail)
)
end)
 in ( LrTable.NT 12, ( result, ID1left, lvalue_tail1right), rest671)

end
|  ( 24, ( rest671)) => let val  result = MlyValue.lvalue_tail (fn _
 => (nil))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 25, ( ( _, ( MlyValue.lvalue_tail lvalue_tail1, _, 
lvalue_tail1right)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
DOT1left, _)) :: rest671)) => let val  result = MlyValue.lvalue_tail
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (lvalue_tail as lvalue_tail1) = lvalue_tail1 ()
 in (Field (S.symbol ID) :: lvalue_tail)
end)
 in ( LrTable.NT 13, ( result, DOT1left, lvalue_tail1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.lvalue_tail lvalue_tail1, _, 
lvalue_tail1right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, (
 _, LBRACK1left, _)) :: rest671)) => let val  result = 
MlyValue.lvalue_tail (fn _ => let val  (exp as exp1) = exp1 ()
 val  (lvalue_tail as lvalue_tail1) = lvalue_tail1 ()
 in (Subscript exp :: lvalue_tail)
end)
 in ( LrTable.NT 13, ( result, LBRACK1left, lvalue_tail1right), 
rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.exp_seq (fn _ => (
nil))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp_seq_tail exp_seq_tail1, _, 
exp_seq_tail1right)) :: ( _, ( MlyValue.exp exp1, (expleft as exp1left
), _)) :: rest671)) => let val  result = MlyValue.exp_seq (fn _ => let
 val  (exp as exp1) = exp1 ()
 val  (exp_seq_tail as exp_seq_tail1) = exp_seq_tail1 ()
 in ((exp, expleft) :: exp_seq_tail)
end)
 in ( LrTable.NT 15, ( result, exp1left, exp_seq_tail1right), rest671)

end
|  ( 29, ( rest671)) => let val  result = MlyValue.exp_seq_tail (fn _
 => (nil))
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp_seq_tail exp_seq_tail1, _, 
exp_seq_tail1right)) :: ( _, ( MlyValue.exp exp1, expleft, _)) :: ( _,
 ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_seq_tail (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exp_seq_tail as exp_seq_tail1) = exp_seq_tail1 ()
 in ((exp, expleft) :: exp_seq_tail)
end)
 in ( LrTable.NT 16, ( result, SEMICOLON1left, exp_seq_tail1right), 
rest671)
end
|  ( 31, ( rest671)) => let val  result = MlyValue.function_args (fn _
 => (nil))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.function_args_tail function_args_tail1, _, 
function_args_tail1right)) :: ( _, ( MlyValue.exp exp1, (expleft as 
exp1left), _)) :: rest671)) => let val  result = 
MlyValue.function_args (fn _ => let val  (exp as exp1) = exp1 ()
 val  (function_args_tail as function_args_tail1) = 
function_args_tail1 ()
 in ((exp, expleft) :: function_args_tail)
end)
 in ( LrTable.NT 17, ( result, exp1left, function_args_tail1right), 
rest671)
end
|  ( 33, ( rest671)) => let val  result = MlyValue.function_args_tail
 (fn _ => (nil))
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 34, ( ( _, ( MlyValue.function_args_tail function_args_tail1, _, 
function_args_tail1right)) :: ( _, ( MlyValue.exp exp1, expleft, _))
 :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.function_args_tail (fn _ => let val  (exp as exp1) = exp1 ()
 val  (function_args_tail as function_args_tail1) = 
function_args_tail1 ()
 in ((exp, expleft) :: function_args_tail)
end)
 in ( LrTable.NT 18, ( result, COMMA1left, function_args_tail1right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp lvalue)
end)
 in ( LrTable.NT 14, ( result, lvalue1left, lvalue1right), rest671)

end
|  ( 36, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 14, ( result, NIL1left, NIL1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 14, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 38, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (exp_seq as exp_seq1) = 
exp_seq1 ()
 in (A.SeqExp exp_seq)
end)
 in ( LrTable.NT 14, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 39, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp INT)
end)
 in ( LrTable.NT 14, ( result, INT1left, INT1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (makeBinop(A.IntExp 0, A.MinusOp, exp, MINUSleft))
end)
 in ( LrTable.NT 14, ( result, MINUS1left, exp1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp (STRING, STRINGleft))
end)
 in ( LrTable.NT 14, ( result, STRING1left, STRING1right), rest671)

end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( 
MlyValue.function_args function_args1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (function_args as function_args1) = function_args1 ()
 in (
A.CallExp { func = S.symbol ID,
                                                     args = function_args,
                                                     pos = IDleft }
)
end)
 in ( LrTable.NT 14, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, 
arith_exp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (arith_exp as arith_exp1) = arith_exp1 ()
 in (arith_exp)
end)
 in ( LrTable.NT 14, ( result, arith_exp1left, arith_exp1right), 
rest671)
end
|  ( 44, ( ( _, ( MlyValue.comparison_exp comparison_exp1, 
comparison_exp1left, comparison_exp1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (comparison_exp as 
comparison_exp1) = comparison_exp1 ()
 in (comparison_exp)
end)
 in ( LrTable.NT 14, ( result, comparison_exp1left, 
comparison_exp1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.boolean_exp boolean_exp1, boolean_exp1left,
 boolean_exp1right)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (boolean_exp as boolean_exp1) = boolean_exp1 ()
 in (boolean_exp)
end)
 in ( LrTable.NT 14, ( result, boolean_exp1left, boolean_exp1right), 
rest671)
end
|  ( 46, ( ( _, ( MlyValue.control_exp control_exp1, control_exp1left,
 control_exp1right)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (control_exp as control_exp1) = control_exp1 ()
 in (control_exp)
end)
 in ( LrTable.NT 14, ( result, control_exp1left, control_exp1right), 
rest671)
end
|  ( 47, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.id_eq_seq 
id_eq_seq1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (id_eq_seq as id_eq_seq1) = id_eq_seq1 ()
 in (
A.RecordExp { typ = S.symbol ID,
                                                       fields = id_eq_seq,
                                                       pos = IDleft }
)
end)
 in ( LrTable.NT 14, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp { typ = S.symbol ID,
                                                      size = exp1,
                                                      init = exp2,
                                                      pos = IDleft }
)
end)
 in ( LrTable.NT 14, ( result, ID1left, exp2right), rest671)
end
|  ( 49, ( rest671)) => let val  result = MlyValue.id_eq_seq (fn _ =>
 (nil))
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 50, ( ( _, ( MlyValue.id_eq_seq_tail id_eq_seq_tail1, _, 
id_eq_seq_tail1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _
, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val 
 result = MlyValue.id_eq_seq (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (id_eq_seq_tail as id_eq_seq_tail1) = id_eq_seq_tail1 ()
 in ((S.symbol ID, exp, IDleft) :: id_eq_seq_tail)
end)
 in ( LrTable.NT 19, ( result, ID1left, id_eq_seq_tail1right), rest671
)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.id_eq_seq_tail (fn
 _ => (nil))
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.id_eq_seq_tail id_eq_seq_tail1, _, 
id_eq_seq_tail1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _
, ( MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.id_eq_seq_tail (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (id_eq_seq_tail as id_eq_seq_tail1) = id_eq_seq_tail1 ()
 in ((S.symbol ID, exp, IDleft) :: id_eq_seq_tail)
end)
 in ( LrTable.NT 20, ( result, COMMA1left, id_eq_seq_tail1right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.arith_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.PlusOp, exp2, exp1left))
end)
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.arith_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.MinusOp, exp2, exp1left))
end)
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.arith_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.TimesOp, exp2, exp1left))
end)
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.arith_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.DivideOp, exp2, exp1left))
end)
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.arith_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.ExponentOp, exp2, exp1left))
end)
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.EqOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.NeqOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.GtOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.LtOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.GeOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.comparison_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinop(exp1, A.LeOp, exp2, exp1left))
end)
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolean_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeIf(exp1,exp2, SOME (A.IntExp 0), exp1left))
end)
 in ( LrTable.NT 23, ( result, exp1left, exp2right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolean_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeIf(exp1, A.IntExp 1, SOME exp2, exp1left))
end)
 in ( LrTable.NT 23, ( result, exp1left, exp2right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.control_exp (fn _ => let val  (lvalue
 as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.AssignExp { var = lvalue,
                                                             exp = exp,
                                                             pos = lvalueleft }
)
end)
 in ( LrTable.NT 24, ( result, lvalue1left, exp1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, IF1left, _)) :: rest671)
) => let val  result = MlyValue.control_exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (makeIf(exp1, exp2, NONE,exp1left))
end)
 in ( LrTable.NT 24, ( result, IF1left, exp2right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.control_exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (makeIf(exp1, exp2, SOME exp3,exp1left))
end)
 in ( LrTable.NT 24, ( result, IF1left, exp3right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, WHILE1left, _)) :: 
rest671)) => let val  result = MlyValue.control_exp (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.WhileExp { test = exp1,
                                                            body = exp2,
                                                            pos = exp1left }
)
end)
 in ( LrTable.NT 24, ( result, WHILE1left, exp2right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.control_exp (fn _ =>
 let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp { var = S.symbol ID,
                                                          escape = ref true,
                                                          lo = exp1,
                                                          hi = exp2,
                                                          body = exp3,
                                                          pos = FORleft }
)
end)
 in ( LrTable.NT 24, ( result, FOR1left, exp3right), rest671)
end
|  ( 71, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.control_exp (fn _ => (
A.BreakExp BREAKleft))
 in ( LrTable.NT 24, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 72, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, (
 _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.control_exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (
A.LetExp { decls = decs,
                                                          body = A.SeqExp exp_seq,
                                                          pos = LETleft}
)
end)
 in ( LrTable.NT 24, ( result, LET1left, END1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
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
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun LOW_PRECEDENCE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
end
end
