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

fun errorIfTest(pos, ty) =
    err pos ("INT required in test, " ^ PT.asString ty ^ " provided")

fun errorIfThen(pos, ty1, ty2) =
    err pos ("then and else exp must be same type, then exp is type " 
        ^ PT.asString ty1 ^ " and else exp is type " ^ PT.asString ty2)

fun errorUnit (pos, ty) =
    err pos ("UNIT required, " ^ PT.asString ty ^ " provided")

fun errorNil (pos, id) =
    err pos ("need to give " ^ S.name id ^ " a type when assigning the value nil")

fun errorVar (pos, id) =
  err pos ((S.name id) ^ " is undefined")

(* Write additional error messages here *)


(* -- Helper functions -- *)

val ERRORPAIR = {exp = TAbs.ErrorExp, ty = Ty.ERROR} (* In case of major errors *)

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
    | A.TimesOp => TAbs.TimesOp
    | A.DivideOp => TAbs.DivideOp
    | A.ExponentOp => TAbs.ExponentOp

fun makeBinop(texp1, opt, texp2) =
  makePair( TAbs.OpExp {left = texp1,
    oper = convertOper(opt),
    right = texp2}, Ty.INT )

fun makeIfElse({exp = test, ty = ty1}: TAbs.exp, { exp = thn, ty = ty2} : TAbs.exp, elsexp, pos) =
  if ty1 <> Ty.INT then
    (errorIfTest (pos, ty1); ERRORPAIR)
  else case elsexp of
    NONE => makePair( TAbs.IfExp {
              test = test,
              thn = thn,
              els = elsexp
            }, ty2)
    | _ => ERRORPAIR

fun transTy (tenv, t) = Ty.ERROR (* TODO *)

fun transExp (venv, tenv, extra : extra) =
    let
        (* this is a placeholder value to get started *)
        val TODO = {exp = TAbs.ErrorExp, ty = Ty.ERROR}
        val NILPAIR = {exp = TAbs.NilExp, ty = Ty.UNIT}

        fun trexp (A.NilExp) = TODO
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.IntExp value) = makePair (TAbs.IntExp(value), Ty.INT)
          | trexp (A.StringExp(s,_)) = makePair (TAbs.StringExp(s), Ty.STRING)
          | trexp (A.OpExp({left = left, oper = oper, right = right, pos = pos})) = 
                                  let val texp1{exp1, ty1} : TAbs.exp= trexp(left)
                                        val texp2{exp2, ty2} : TAbs.exp= trexp(right)
                                        in
                                          if checkInt(#ty texp1, pos) andalso checkInt(#ty texp2, pos) then
                                            makeBinop(texp1, oper, texp2)
                                          else makePair(TAbs.ErrorExp, Ty.ERROR)
                                    end
          | trexp(A.SeqExp(explist)) = trseqexp(explist) (* *)
          | trexp(A.IfExp(ifdata)) = trifexp(ifdata)
          | trexp(A.WhileExp(whiledata)) = trwhileexp(whiledata)
          | trexp _ = (print("sry, got nothing\n"); TODO)


        and trwhileexp({test = tst, body = bdy, pos = ps} : A.whiledata) = let
                                                                            val {exp = test, ty = testty} : TAbs.exp = trexp(tst)
                                                                            val bdyty = trexp(bdy)
                                                                           in
                                                                            case testty of
                                                                              T.INT => (print("Success"); TODO)
                                                                            | _ => (print("Failed"); TODO)
          (*
            * When we are making a let expression, we have to use the transExp to interpt with the extended enviorment

              Notes: Fieldvar: 1. Evalutate the var, if typechecked to be of T.RECORD
                                                      | If record, search record for entry with ID
                                                            If ID found, return that plus the type that matches that ID
                                                            If not, Error
                                                      | Else Error

                      SubscriptVar Evalute the var, typecheck to see if of type T.ARRAY
                                                      | if Array, check to see if ARRAY[exp1] if exp1 evaluates to type Ty.INT.
                                                            If int, return ARRAY.ty
                                                            If not int, Error
                                                      | If not Array, Error.
            *)
          (* It should be possible to reuse this in other functions *)
        and trvar (A.SimpleVar (id, pos)) = let val ty = lookupVar venv id pos in
                                              case ty of
                                              SOME(Env.VarEntry({ty = t})) => makeVar(TAbs.SimpleVar(id), t)
                                              |_ => (errorVar(pos, id); makePair(TAbs.ErrorExp,Ty.ERROR))
                                              end
          | trvar (A.FieldVar (var, id, pos)) = TODO
          | trvar (A.SubscriptVar (var, exp, pos)) = TODO
        
        and trseqexp(explist) = trseqexpaux(explist, Ty.UNIT, []) (* The empty sequence has nothing, so its type is unit and the exp is nil *)
        
        and trseqexpaux ([], ty,acc) = makePair(TAbs.SeqExp(acc), ty)
          | trseqexpaux ((exp, pos)::xs, ty, acc) = let val res = trexp(exp)
                                                      in trseqexpaux (xs, #ty res, acc @ [res]) 
                                                    end

        (* Determine type of each part first, then offload work to helper function *)
        and trifexp ( { test = test, thn = thn, els = els, pos = pos} : A.ifdata ) =
          let val elsvalue = case els of 
                                NONE => NONE
                              | SOME(exp) => SOME(trexp(exp))
          in makeIfElse(trexp(test), trexp(thn), elsvalue, pos) end
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
