structure Env :> ENV = 
struct

structure Sy = Symbol
structure Ty = Types
structure Trans = Translate
structure T = Temp

type access = unit ref
type ty = Ty.ty

datatype enventry = VarEntry of {access: Trans.access, ty: ty}
                  | FunEntry of {level: Trans.level, label: T.label, formals: ty list, result: ty}

fun enter ((symbol, entry), env) = Sy.enter (env, symbol, entry)

val baseTenv =
    foldl enter Sy.empty 
          [ (Sy.symbol "int", Ty.INT)
          , (Sy.symbol "string", Ty.STRING)]

val baseVenv = foldl enter Sy.empty [

  (Sy.symbol "print",       FunEntry {level = Trans.outermost, label= Temp.newLabel("print"), formals = [Ty.STRING]
                                     , result = Ty.UNIT}),

  (Sy.symbol "flush",       FunEntry {level = Trans.outermost, label= Temp.newLabel("flush"), formals = []
                                     , result = Ty.UNIT}),

  (Sy.symbol "getchar",		FunEntry {level = Trans.outermost, label= Temp.newLabel("getchar"), formals = []
  									 , result = Ty.STRING}),

  (Sy.symbol "ord",			FunEntry {level = Trans.outermost, label= Temp.newLabel("ord"), formals = [Ty.STRING]
  									 , result = Ty.INT}),

  (Sy.symbol "chr", 		FunEntry {level = Trans.outermost, label= Temp.newLabel("chr"), formals = [Ty.INT]
  									 , result = Ty.STRING}),

  (Sy.symbol "size", 		FunEntry {level = Trans.outermost, label= Temp.newLabel("size"), formals = [Ty.STRING]
  									 , result = Ty.INT}),

  (Sy.symbol "substring", 	FunEntry {level = Trans.outermost, label= Temp.newLabel("substring"), formals = [Ty.STRING, Ty.INT, Ty.INT]
  									 , result = Ty.STRING}),

  (Sy.symbol "concat", 			FunEntry {level = Trans.outermost, label= Temp.newLabel("concat"), formals = [Ty.STRING, Ty.STRING]
  									 , result = Ty.STRING}),

  (Sy.symbol "not", 			FunEntry {level = Trans.outermost, label= Temp.newLabel("not"), formals = [Ty.INT]
  									 , result = Ty.INT}),

  (Sy.symbol "exit", 			FunEntry {level = Trans.outermost, label= Temp.newLabel("exit"), formals = [Ty.INT]
  									 , result = Ty.UNIT})

]

end (* Env *)

