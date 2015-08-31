fun MaxExp _: NumExp = 0
    | MaxExp _: IdExp = 0
    | MaxExp OpExp(el,_,er) =  max[MaxExp el, MaxExp er]
    | MaxExp EseqExp (st, exp) = max[MaxStm st, MaxExp exp]

and fun MaxStm CompoundStm (lstm, rstm) = max[MaxStm lstm, MaxStm rstm]
| MaxStm AssignStm (id,exp) = maxExp exp
| MaxStm ExpList(h::tl)= Max[len h::tl, MaxExp h, MaxStm tl]

fun max [x] = x
    | max(x::xs) = if x > max xs then x else max xs