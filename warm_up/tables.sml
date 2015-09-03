(* Blatanly stolen from the lecturer for own nefarious purposes. Meant as a placeholder for now, hopefully we are able to implement a more efficient version of tables at a later date *)


(* Example of functional tables from Lecture 02 on Friday 28 August 2015 *)

(*
usage: > sml
       - use "tabledemo.sml";
       - open TableDemo;
*)

(* Note Friday 28 August 2015 - we will talk about structures later *)

structure TableDemo =
struct
    fun myTable (key:string)   =
      if key = "x" then 42
      else if key = "y" then 84
      else if key = "z" then 168
      else 0 (* default value *)

    fun myUpdatedTable (key:string) =
      if key = "w" then 336
                   else myTable key

    (* Functional tables: we represent tables as a functions
       of type (string -> int option). Given a table t, a lookup
       into key k is performed as (t k). The returned value is
       (SOME v) if the value v corresponds to the key k, and is
       NONE otherwise
    *)

    type intTable = string -> int option

    fun myTableOpt (key : string) =
      if key = "x" then SOME 42
      else if key = "y" then SOME 84
      else if key = "z" then SOME 168
      else NONE



    (* alternative way to define the same function *)
    fun myTableOpt' (key : string) =
      (case key of
         "x" => SOME 42
       | "y" => SOME 84
       | "z" => SOME 168
       | _   => NONE
       )


    (* yet another way ... *)
    fun myTableOpt'' "x" = SOME 42
     |  myTableOpt'' "y" = SOME 84
     |  myTableOpt'' "z" = SOME 168
     |  myTableOpt'' _ = NONE


    fun myUpdatedTableOpt (key:string) =
      if key = "w" then SOME 336
                   else myTableOpt key


    fun emptyTable  ( x : string ) = NONE: int option


    (* this is an equivalent declaration to emptyTable above *)
    val emptyTable': intTable = fn x => NONE


    fun updateTable ( t: intTable, y:string, v:int) x =
         if x = y then SOME v
                  else t x

    (* an equivalent declaration to updateTable above *)
    fun updateTable' ( t: intTable, y: string, v:int) =
       fn x => if x = y then SOME v
                        else t x

    (* the same as before, now with some debugging... *)

    fun updateTable'' ( t: intTable, y:string, v:int) x =
     (print ("inside update table with " ^ y ^
             " being set to " ^ Int.toString(v) ^ "\n");
        if x = y then SOME v
                 else t x
     )

    (* Some examples *)

    val t1:intTable = updateTable (emptyTable, "x", 42)
    val v1 = t1 "x" (* SOME 42*)

    val t2 = updateTable (t1, "y", 84)
    val v2 = t2 "y" (* SOME 84 *)

    val t3 = updateTable (t2, "x", 43) (* 'overwriting' x *)
    val v3 = t3 "x" (* SOME 43 *)

    (* Exercise 1 - curry the first 3 arguments to updateTable,
       fix the examples to accommodate the new signatures *)

    (* Exercise 2 - uncurry all the arguments to updateTable - what goes wrong? *)
end
