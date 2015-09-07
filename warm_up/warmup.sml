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
      G.PrintStm [G.IdExp "a"]))


val prog2 = 
    G.CompoundStm(
	G.PrintStm[G.IdExp "a"],
	G.CompoundStm(
		G.PrintStm[G.OpExp (G.IdExp "A", G.Plus, G.NumExp 15), G.OpExp (G.IdExp "B", G.Plus, G.NumExp 16)],
		G.AssignStm("Teacher", G.IdExp "Alan")
	)
    )

(* Prog 3 *)
val prog3 = 
    G.CompoundStm(
	G.PrintStm[G.IdExp "A"],
	G.PrintStm[G.IdExp "B"]
    )

(* Note: Because the language is so strict, the compilers doesn't agree if you do something unexpected, in Prog 3 we tried to put a PrintStm inside a PrintStm, and the compiler wouldn't let us *)

(* Prog 4 : Test to make sure it can find the correct longest PrintStm if already inside a PrintStm via EseqExp *)
val prog4 =
    G.PrintStm[
	G.EseqExp(
		G.PrintStm[G.IdExp "A", G.IdExp "B", G.IdExp "C"],
		G.IdExp "a"
	)
    ]

(* Prog 5: Test to make sure it doesn't just return the last seen PrintStm *)
val prog5 =
    G.CompoundStm(
	G.PrintStm[G.IdExp "A"],
	G.CompoundStm(
		G.PrintStm[G.NumExp 3, G.IdExp "C"],
		G.AssignStm("A", G.EseqExp(G.PrintStm[G.IdExp "D"],G.NumExp 3))
	)
    )
(*Test nested prints
Source: a:=3; print(a+1,(print(a+3), a*4)); print(4/2); 
Expected output: 4 6 /n 12 /n 2*)

val prog6 = 
    G.CompoundStm(
	G.AssignStm("a", G.NumExp 3), (*1st stm*)
	G.CompoundStm(
	    G.PrintStm[G.OpExp(G.IdExp "a", G.Plus, G.NumExp 1), 
		      G.EseqExp(G.PrintStm[G.OpExp(G.IdExp "a",G.Plus, G.NumExp 3)],
			       G.OpExp(G.IdExp "a", G.Times, G.NumExp 4))], (* 2nd stm *)
	    G.PrintStm[G.OpExp(G.NumExp 4, G.Div, G.NumExp 2)]))
(* ... *)

(* Calling the interpreter on the example program. Uncomment to proceed
   -- default implementation will raise NotImplemented exception *)

(* val _ = interp prog *)

exception SyntaxError
exception unAssignedIdentifier of string

(* The max arguments function *)
fun maxExp ( G.NumExp(number) ) = 0
  | maxExp ( G.IdExp(id) ) = 0
  | maxExp ( G.OpExp(lExp,_,rExp) ) = max[maxExp lExp, maxExp rExp]
  | maxExp ( G.EseqExp (stm, exp) ) = max[maxStm stm, maxExp exp]
and maxStm ( G.CompoundStm(lStm, rStm) ) = max[maxStm lStm, maxStm rStm]
  | maxStm ( G.AssignStm( _, exp) ) = maxExp(exp)
  | maxStm ( G.PrintStm( list ) ) = max(length list::applyFunToList maxExp list)

val maxArgs = maxStm

(* Here begins the quest to write the interpreter *)

exception DivisionByZero
exception unAssignedIdentifier of string

type table = string -> int option
val emptyTable : table = fn x => NONE
(*fun updateTable (tab : table, key : string, value : int option) 
  = fn x => if x = key then value else tab key*)

    fun updateTable ( t: table, y:string, v:int option) x =
         if x = y then v
                  else t x

fun lookUpTable (t : table, key: string) = t key

fun interpStm (G.CompoundStm(stm0, stm1), env : table) : table = 
  interpStm(stm1, interpStm(stm0, env))
  | interpStm (G.AssignStm(id, exp), env) =
    let val res = interpExp(exp, env) in
	updateTable(#2 res, id, #1 res) end
  | interpStm (G.PrintStm (expList), env) = interpPrint(expList, env)

and interpExp (G.NumExp(number), env : table) = (SOME number, env)
  | interpExp (G.IdExp(id), env) = 
    let val res0 = lookUpTable(env, id)
    in
	case res0 of
	NONE => raise unAssignedIdentifier id
       |SOME _ => (res0, env)
    end

  | interpExp (G.OpExp(exp0, opr, exp1), env) = 
    let val res0 = interpExp(exp0, env) in 
	let val res1 
		= interpExp(exp1, (#2 res0))
	in (if (#1 res0 = NONE) orelse (#1 res1 = NONE) then
		NONE
	    else SOME (case opr of 
			  G.Plus => valOf (#1 res0) + valOf (#1 res1)
			| G.Minus => valOf (#1 res0) - valOf (#1 res1)
			| G.Times => valOf (#1 res0) * valOf (#1 res1)
			| G.Div => if(valOf (#1 res1)=0) then raise DivisionByZero else valOf (#1 res0) div valOf (#1 res1)),(#2 res1))
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


fun interp stm = 
  let val res = interpStm (stm, emptyTable) in () end 
  handle DivisionByZero => print("Not allowed to divide by 0" ^ "\n")
      | unAssignedIdentifier id =>  print ("Identifier not assigned yet " ^ id ^ "\n" )

val binPrg1 = G.PrintStm([
	G.OpExp(
	    G.NumExp(4),G.Minus,
	    G.OpExp(G.NumExp(3),G.Minus,G.NumExp(5))
	)])
val binPrg2 = G.PrintStm([
	G.OpExp(
	    G.OpExp(G.NumExp 4,G.Minus,G.NumExp 3),
	    G.Minus,G.NumExp 5)
])
