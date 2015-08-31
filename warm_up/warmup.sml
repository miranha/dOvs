structure G = SLgrammar

(* ... *)

(* placeholder definitions for not implemented functions *)

exception NotImplemented

fun stringOfStm _ = raise NotImplemented
fun buildEnv _    = raise NotImplemented
fun interpStm _   = raise NotImplemented
fun printEnv _    = raise NotImplemented

(* ... *)

fun interp (s: G.stm): unit =
    let val _ = print ("Executing: " ^ (stringOfStm s) ^ "\n")
        val env = buildEnv s
        val env' = interpStm (s, env)
    in printEnv env'
    end

(* ----- Example for testing ----- *)

val prog =
  (* a := 5+3; b := (print(a,a-1), 10*a); print(b) *)
  G.CompoundStm (
    G.AssignStm ("a", G.OpExp (G.NumExp 5, G.Plus, G.NumExp 3)),
    G.CompoundStm (
      G.AssignStm ("b", G.EseqExp (
        G.PrintStm [G.IdExp "a", G.OpExp (G.IdExp "a", G.Minus, G.NumExp 1)],
        G.OpExp (G.NumExp 10, G.Times, G.IdExp "a"))),
      G.PrintStm [G.IdExp "b"]))

(* ... *)

(* Calling the interpreter on the example program. Uncomment to proceed
   -- default implementation will raise NotImplemented exception *)

(* val _ = interp prog *)



fun MaxExp _: NumExp = 0
  | MaxExp _: IdExp = 0
  | MaxExp OpExp(el,_,er) =  max[MaxExp el, MaxExp er]
  | MaxExp EseqExp (st, exp) = max[MaxStm st, MaxExp exp]

and fun MaxStm CompoundStm (lstm, rstm) = max[MaxStm lstm, MaxStm rstm]
      | MaxStm AssignStm (id,exp) = maxExp exp
      | MaxStm ExpList(h::tl)= Max[len h::tl, MaxExp h, MaxStm tl]

fun max [x] = x
  | max(x::xs) = if x > max xs then x else max xs
