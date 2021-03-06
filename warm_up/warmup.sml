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



(* First part is a collection of helper functions *)

(* maxAux keeps track of the current encoutered max value while traversing the list.
max peels of the head of a list, sets as current max value, then let maxAux traverse the list*)
fun maxAux [] curMax = curMax
  | maxAux (x::xs) curMax = maxAux xs (if curMax > x then curMax else x)

fun max [] = 0
  | max(x::xs) = maxAux xs x

fun applyFunToList f [] = []
  | applyFunToList f (x::xs) = f x :: applyFunToList f xs

(* ... *)

(* The max arguments function *)
fun maxExp ( G.NumExp(number) ) = 0
  | maxExp ( G.IdExp(id) ) = 0
  | maxExp ( G.OpExp(lExp,_,rExp) ) = max[maxExp lExp, maxExp rExp]
  | maxExp ( G.EseqExp (stm, exp) ) = max[maxStm stm, maxExp exp]
and maxStm ( G.CompoundStm(lStm, rStm) ) = max[maxStm lStm, maxStm rStm]
  | maxStm ( G.AssignStm( _, exp) ) = maxExp(exp)
  | maxStm ( G.PrintStm( list ) ) = max(length list::applyFunToList maxExp list)

val maxargs = maxStm


(* --- Interpreter of statements ---*)
(* Here we have all the code needed to handle the function
interp 
*)

(* Some code to give exeptions, other to handle table *)
exception DivisionByZero
exception unAssignedIdentifier of string

type table = (string * int option) list
val emptyTable : table = []
(*fun updateTable (tab : table, key : string, value : int option) 
  = fn x => if x = key then value else tab key*)

fun updateTable'' (t, key:string, value:int option) = (key, value)::t

fun lookupTable ([], key) = NONE
  | lookupTable ((k,v)::xs, key) = if k = key then v
				   else lookupTable (xs, key)


fun updateAux' ([], key , value) acc = (key, value) :: acc 
  (* found nothing in list, add pair to list *)
  | updateAux' ((x,y)::xs, key, value) acc
    = if x = key then
	  acc @ [(key, value)] @ xs
      else updateAux' (xs, key, value) acc @ [(x,y)]

fun updateTable' (t, key, value) = updateAux (t, key, value) []
and updateAux ([], key , value) acc = (key, value) :: acc 
  (* found nothing in list, add pair to list *)
  | updateAux ((x,y)::xs, key, value) acc
    = if x = key then
	  acc @ [(key, value)] @ xs
      else updateAux (xs, key, value) acc @ [(x,y)]
val emptyTable = []


(* Here, we start the build env function 
---
Only an assign statement gives rise to a valid new id. Uassigned id's
are an error handled at runtime
*)

(* Handle arguments type expression*)
fun buildEnvAux (G.CompoundStm(stm1,stm2), env) =
  buildEnvAux (stm2, buildEnvAux(stm1, env))

  (*We do not assign value to things *)
  | buildEnvAux (G.AssignStm(id,exp), env) = 
    let val env' = buildEnvAux2(exp, env) 
    in updateTable'(env',id,NONE)
    end

  | buildEnvAux (G.PrintStm(x::xs), env) 
    = buildEnvAux(G.PrintStm(xs), buildEnvAux2 (x, env))

  | buildEnvAux (G.PrintStm([]), env) = env

(* Handle arguments of type expression *)
and buildEnvAux2 (G.NumExp(_), env) = env 

  | buildEnvAux2 (G.IdExp(_), env) = env

  | buildEnvAux2 (G.OpExp(exp1,_,exp2), env) = 
    buildEnvAux2 (exp2, buildEnvAux2(exp1,env))

  | buildEnvAux2 (G.EseqExp(stm,exp), env) = 
    buildEnvAux2 (exp, buildEnvAux(stm,env))

fun buildEnv (stm) : (G.id * int option) list = buildEnvAux(stm,[])


(* ------------------------------------------------ *)
(* Here we write the functions interStm and interpExp, along
with various helper functions *)

val updateTable = updateTable'

fun printIntOpList ([], string) = print(string ^ "\n")
  | printIntOpList (x::xs, string) = 
    printIntOpList (xs, string ^ " " ^ Int.toString (valOf x))

fun interpStm (G.CompoundStm(stm0, stm1), env : table) : table = 
  interpStm(stm1, interpStm(stm0, env))

  | interpStm (G.AssignStm(id, exp), env) =
    let val (value, env') = interpExp(exp, env) in
	updateTable(env', id, value) end

  | interpStm (G.PrintStm (expList), env) = interpPrint(expList, env)

and interpExp (G.NumExp(number), env) = (SOME number, env)

  | interpExp (G.IdExp(id), env) = 
    let val res0 = lookupTable(env, id)
    in
	case res0 of
	NONE => raise unAssignedIdentifier id
       |SOME _ => (res0, env)
    end

  | interpExp (G.OpExp(exp0, opr, exp1), env) = 
    let val (iOpt1, env1) = interpExp(exp0, env)
	val (iOpt2, env2) = interpExp(exp1, env1)
    in (if (iOpt1 = NONE) orelse (iOpt2 = NONE) then
	    NONE
	else 
	    SOME ( let val lInt = valOf iOpt1
		       val rInt = valOf iOpt2 in
		       (* We know that iOpt's are some *)
		       case opr of 
			   G.Plus => lInt + rInt
			 | G.Minus => lInt -rInt
			 | G.Times => lInt * rInt
			 | G.Div => if(rInt = 0) 
				    then raise DivisionByZero 
				    else lInt div rInt
		   end
		 ),env2)
    end

  | interpExp (G.EseqExp(stm, exp), env) = interpExp(exp, interpStm(stm, env))

and expListToInt ([], acc, env) = (acc,env)
  |  expListToInt ((x::xs), acc, env) = 
    let val (i, env') = interpExp(x, env) in
	expListToInt (xs, acc @ [i], env')
    end

and interpPrint (list, env) = let val (intList, env') 
				      = expListToInt(list,[],env)
				  val res = printIntOpList (intList, "")
			      in env'
			      end
			      



(* Begin of stringofStm implementation *)

fun map_operator(G.Plus) = "+" 
  | map_operator (G.Minus) = "-"
  | map_operator (G.Div) = "/"
  | map_operator (G.Times) = "*"   

fun stringOfExp (G.NumExp(number)) = Int.toString(number)
  | stringOfExp (G.IdExp(id)) = id
  | stringOfExp (G.OpExp(lExp,operator,rExp))= "(" ^ stringOfExp(lExp) ^ map_operator operator ^ stringOfExp(rExp) ^ ")" 
  | stringOfExp (G.EseqExp(stm,exp)) = "(" ^ stringOfStm(stm) ^ ","  ^ stringOfExp(exp) ^ ")"
and stringOfStm (G.CompoundStm(lStm, rStm)) = stringOfStm(lStm) ^ ";" ^ stringOfStm(rStm)
  | stringOfStm (G.AssignStm(id,exp)) = id ^ ":=" ^ stringOfExp(exp)
  | stringOfStm (G.PrintStm(list)) = "print(" ^ String.concatWith "," (map stringOfExp list) ^ ")" 

fun interp (stm : G.stm) = 
  let 
      val _ = print ("Executing: " ^ (stringOfStm stm) ^ "\n")
      val _ = interpStm (stm, emptyTable) 
  in () end 
  handle DivisionByZero => print("Error: Division by zero not allowed" ^ "\n")
      | unAssignedIdentifier id =>  print ("Error: Using unassigned variable: " ^ id ^ "\n" )

(* placeholder definitions for not implemented functions *)
(*
exception NotImplemented

fun stringOfStm _ = raise NotImplemented
fun buildEnv _    = raise NotImplemented
fun interpStm _   = raise NotImplemented
fun printEnv _    = raise NotImplemented
*)
(* ... *)

fun interp' (s: G.stm): unit =
    let val _ = print ("Executing: " ^ (stringOfStm s) ^ "\n")
        val env = buildEnv s
        val env' = interpStm (s, env)
    in ()(*printEnv env'*)
    end
    handle DivisionByZero => print("Error: Division by zero not allowed" ^ "\n")
	 | unAssignedIdentifier id =>  print ("Error: Using unassigned variable: " ^ id ^ "\n" )

(* Here are the test cases from the Report *)

(*

These test cases are described inside the report


Test cases:

1) source code: a:=2; b:= a+2; print(b+a,b-a,b*a,b/a)
*)

val test1 = G.CompoundStm(
	G.AssignStm("a",G.NumExp 2),
	G.CompoundStm(
	    G.AssignStm("b", G.OpExp(G.IdExp "a", G.Plus, G.NumExp 2)),
	    G.PrintStm[G.OpExp(G.IdExp "b", G.Plus, G.IdExp "a"),
	    			G.OpExp(G.IdExp "b", G.Minus, G.IdExp "a"),
	    			G.OpExp(G.IdExp "b", G.Times, G.IdExp "a"),
	    			G.OpExp(G.IdExp "b", G.Div, G.IdExp "a")]))

(*
2) source code: print((3-5)/2); print(a);
*)

val test2 = G.CompoundStm(
	G.PrintStm[G.OpExp(G.OpExp(G.NumExp 3, G.Minus, G.NumExp 5),G.Div, G.NumExp 2)],
	G.PrintStm[G.IdExp "a"])


(*
3) source code: a:=3; print((print(a+3), a*4)); print(4/2);
*)

val test3 = 
    G.CompoundStm(
	G.AssignStm("a", G.NumExp 3),
	G.CompoundStm(
	    G.PrintStm[G.OpExp(G.IdExp "a", G.Plus, G.NumExp 1), 
		      G.EseqExp(G.PrintStm[G.OpExp(G.IdExp "a",G.Plus, G.NumExp 3)],
			       G.OpExp(G.IdExp "a", G.Times, G.NumExp 4))],
	    G.PrintStm[G.OpExp(G.NumExp 4, G.Div, G.NumExp 2)]))

(*
4) source code: Test4: a:=0; print(a); Print(2/a); Print(2+a);
*)

val test4 = G.CompoundStm(
	G.AssignStm("a", G.NumExp 0),
	G.CompoundStm(
	    G.PrintStm[G.IdExp "a"],
	    G.CompoundStm(
		G.PrintStm[G.OpExp(G.NumExp 2, G.Div, G.IdExp "a")],
		G.PrintStm[G.OpExp(G.NumExp 2, G.Minus, G.NumExp 1)])))


(*
5) source code: a:=2; print(a) ; a:= 4; Print(a);
*)

val test5 = G.CompoundStm(
	G.AssignStm("a", G.NumExp 2),
	G.CompoundStm(
	    G.PrintStm[G.IdExp "a"],
	    G.CompoundStm(
		G.AssignStm("a", G.NumExp 4),
		G.PrintStm[G.IdExp "a"])))


(* ----- Example for testing ----- *)
(* These are additional experiments in order to test the maxArgs and interp *)

val prog =
  (* a := 5+3; b := (print(a,a-1), 10*a); print(b) *)
  G.CompoundStm (
    G.AssignStm ("a", G.OpExp (G.NumExp 5, G.Plus, G.NumExp 3)),
    G.CompoundStm (
      G.AssignStm ("b", G.EseqExp (
        G.PrintStm [G.IdExp "a", G.OpExp (G.IdExp "a", G.Minus, G.NumExp 1)],
        G.OpExp (G.NumExp 10, G.Times, G.IdExp "a"))),
      G.PrintStm [G.IdExp "b"]))


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

(* ... *)

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
