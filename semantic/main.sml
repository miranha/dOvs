structure Main =
struct

fun compile infile =
    let
        val absyn = Parse.parse infile
        val tabsyn = Semant.transProg absyn
    in
        if !ErrorMsg.anyErrors then OS.Process.exit 1
        else ()
    end

fun exportedFn (self, [infile]) = (compile infile; 0)
  | exportedFn (self, _) = 
    (print "Expects argument <infile>"; ~1)

end (* Main *)

