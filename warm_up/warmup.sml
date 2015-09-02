(* First part is a collection of helper functions *)

(* maxAux keeps track of the current encoutered max value while traversing the list.
max peels of the head of a list, sets as current max value, then let maxAux traverse the list*)
fun maxAux [] curMax = curMax
  | maxAux (x::xs) curMax = maxAux xs (if curMax > x then curMax else x)

fun max [] = 0
  | max(x::xs) = maxAux xs x

fun applyFunToList f [] = []
  | applyFunToList f (x::xs) = f x :: applyFunToList f xs

(* Define the grammar of the Straight Line Language *)

structure SLgrammar =
struct

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
	     | OpExp of exp * binop * exp
	     | EseqExp of stm * exp
end

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

exception SyntaxError

fun maxExp ( G.NumExp(number) ) = 0
  | maxExp ( G.IdExp(id) ) = 0
  | maxExp ( G.OpExp(lExp,_,rExp) ) = max[maxExp lExp, maxExp rExp]
  | maxExp ( G.EseqExp (stm, exp) ) = max[maxStm stm, maxExp exp]
and maxStm ( G.CompoundStm(lStm, rStm) ) = max[maxStm lStm, maxStm rStm]
  | maxStm ( G.AssignStm( _, exp) ) = maxExp(exp)
  | maxStm ( G.PrintStm( list ) ) = max(length list::applyFunToList maxExp list)

(* Here begins the quest to write the interpreter *)

type table = string -> int option
val emptyTable : table = fn x => NONE
fun updateTable (tab : table, key : string, value : int option) 
  = fn x => if x = key then value else NONE
fun lookUpTable (t : table, key: string) = t key

fun interpStm (G.CompoundStm(stm0, stm1), env : table) : table = 
  interpStm(stm1, interpStm(stm0, env))
  | interpStm (G.AssignStm(id, exp), env) =
    let val res = interpExp(exp, env) in
	updateTable(#2 res, id, #1 res) end
  | interpStm (G.PrintStm (expList), env) = interpPrint(expList, env)

and interpExp (G.NumExp(number), env : table) = (SOME number, env)
  | interpExp (G.IdExp(id), env) = (lookUpTable(env, id), env)
  | interpExp (G.OpExp(exp0, opr, exp1), env) = 
    let val res0 = interpExp(exp0, env) in 
	let val res1 
		= interpExp(exp1, (#2 res0))
	in (if (#1 res0 = NONE) orelse (#1 res1 = NONE) then
		NONE
	    else SOME (if (opr = G.Plus) then valOf (#1 res0) + valOf (#1 res1)
		       else if (opr = G.Minus) then valOf (#1 res0) - valOf (#1 res1)
		       else if (opr = G.Times) then valOf (#1 res0) * valOf (#1 res1)
		       else valOf (#1 res0) div valOf (#1 res1))
	   , (#2 res1))
	end
    end
  | interpExp (G.EseqExp(stm, exp), env) = interpExp(exp, interpStm(stm, env))

and interpPrint ([] : G.exp list, env : table) = (print ("\n"); env)
  | interpPrint (x::xs, env) = let val res = interpExp(x, env)
			       in (print (if #1 res = NONE 
					 then "error" 
					  else Int.toString( 
						  valOf( #1 res)) ^ " ");
				  interpPrint (xs, #2 res))
			       end

fun interp stm = interpStm (stm, emptyTable)
