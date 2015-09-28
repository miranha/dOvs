(* AU Compilation 2015.
 *
 * This the main working file for Part 4 - Semantic Analysis
 *
 *)



(* Our signature that for the semantic interface exposes only one function. *)

signature SEMANT =
sig
  val transProg: Absyn.exp -> TAbsyn.exp
end

structure Semant :> SEMANT =
struct


structure S = Symbol
structure A = Absyn
structure E = Env
structure Ty = Types
structure PT = PrintTypes
structure TAbs = TAbsyn

(* Use the extra record to add more information when traversing the tree
   It should become obvious when you actually need it, what to do.
   Alternatively, you have to add extra parameters to your functions *)

type extra = {}

(* placehloder for declarations, the final code should compile without this *)
val TODO_DECL = TAbs.TypeDec [] (* Delete when possible *)

(* Error messages *)

val err = ErrorMsg.error

fun out msg pos = err pos msg

fun errorInt (pos, ty) =
    err pos ("INT required, " ^ PT.asString ty ^ " provided")

fun errorUnit (pos, ty) =
    err pos ("UNIT required, " ^ PT.asString ty ^ " provided")

fun errorNil (pos, id) =
    err pos ("need to give " ^ S.name id ^ " a type when assigning the value nil")

fun errorVar (pos, id) =
  err pos ((S.name id) ^ " is undefined")

(* Write additional error messages here *)


(* -- Helper functions -- *)

(* Use the below three funs like:
   throw errorUnit (pos,ty) ifFalse isUnit(_) *)

fun ifTrue test err args = if test
                           then err args
                           else ()

fun ifFalse test err args = if test
                            then ()
                            else err args

fun throw err args testFun test  = testFun test err args


fun lookupTy tenv sym pos =
    let
        val tyOpt = S.look (tenv, sym)
    in
        tyOpt
    end

fun lookupVar venv sym pos =
  let
    val varOpt = S.look (venv,sym)
  in
    varOpt
  end

fun actualTy (Ty.NAME (s, ty)) pos =
    Ty.ERROR (* TODO *)
  | actualTy t _ = t

fun checkInt (ty, pos) =
    case ty
     of Ty.INT => true
      | Ty.ERROR => false      
      | _ => (errorInt (pos, ty); false)

fun isUnit (ty) =
    case ty
     of Ty.UNIT => true
      | Ty.ERROR => true
      | _ => false

fun checkAssignable (declared: Ty.ty, assigned: Ty.ty, pos, msg) =
    let
        val aDeclared = actualTy declared pos
        val aAssigned = actualTy assigned pos
    in
        () (* TODO *)
    end

(* Helper functions to make life easier *)
fun makePair (expDesc, ty) =
    { exp = expDesc, 
      ty = ty} : TAbs.exp

fun makeVar (varDesc, ty) =
  makePair(
    TAbs.VarExp {
    var = varDesc,
    ty = ty
  }, ty)

fun convertOper (oper) =
  case oper of
      A.EqOp => TAbs.EqOp
    | A.NeqOp => TAbs.NeqOp
    | A.LtOp => TAbs.LtOp
    | A.LeOp => TAbs.LeOp
    | A.GtOp => TAbs.GtOp
    | A.GeOp => TAbs.GeOp
    | A.PlusOp => TAbs.PlusOp
    | A.MinusOp => TAbs.MinusOp
    | A.TimesOp => TAbs.MinusOp
    | A.DivideOp => TAbs.DivideOp
    | A.ExponentOp => TAbs.ExponentOp

fun makeBinop(texp1, opt, texp2) =
  makePair( TAbs.OpExp {left = texp1,
    oper = convertOper(opt),
    right = texp2}, Ty.INT )

fun transTy (tenv, t) = Ty.ERROR (* TODO *)

fun transExp (venv, tenv, extra : extra) =
    let
        (* this is a placeholder value to get started *)
        val TODO = {exp = TAbs.ErrorExp, ty = Ty.ERROR}

        fun trexp (A.NilExp) = TODO
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.IntExp value) = makePair (TAbs.IntExp(value), Ty.INT)
          | trexp (A.StringExp(s,_)) = makePair (TAbs.StringExp(s), Ty.STRING)
          | trexp (A.OpExp(data)) = let val texp1 = trexp(#left data)
                                        val texp2 = trexp(#right data)
                                        in
                                          if checkInt(#ty texp1, #pos data) andalso checkInt(#ty texp2, #pos data) then
                                            makeBinop(texp1, #oper data, texp2)
                                          else makePair(TAbs.ErrorExp, Ty.ERROR)
                                    end
          | trexp _ = (print("sry, got nothing\n"); TODO)

          (* It should be possible to reuse this in other functions *)
        and trvar (A.SimpleVar (id, pos)) = let val ty = lookupVar venv id pos in
                                              case ty of
                                              SOME(Env.VarEntry({ty = t})) => makeVar(TAbs.SimpleVar(id), t)
                                              |_ => (errorVar(pos, id); makePair(TAbs.ErrorExp,Ty.ERROR))
                                              end
          | trvar (A.FieldVar (var, id, pos)) = TODO
          | trvar (A.SubscriptVar (var, exp, pos)) = TODO
    in
        trexp
    end

and transDec ( venv, tenv
             , A.VarDec {name, escape, typ = NONE, init, pos}, extra : extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)

  | transDec ( venv, tenv
             , A.VarDec {name, escape, typ = SOME (s, pos), init, pos=pos1}, extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)

  | transDec (venv, tenv, A.TypeDec tydecls, extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)

  | transDec (venv, tenv, A.FunctionDec fundecls, extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)

and transDecs (venv, tenv, decls, extra : extra) =
    let fun visit venv tenv decls result =
            case decls
             of [] => {decls = result, venv = venv, tenv = tenv}
              | (d::ds) =>
                let
                    val { decl = decl
                        , venv = venv'
                        , tenv = tenv'} = transDec (venv, tenv, d, extra)
                in
                    visit venv' tenv' ds (result @ (decl :: []))
                end
    in
        visit venv tenv decls []
    end

fun transProg absyn =
    transExp (Env.baseVenv, Env.baseTenv, {}) absyn

end (* Semant *)
