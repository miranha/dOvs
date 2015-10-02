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


val Equality = [A.EqOp, A.NeqOp]

val Ordering = [ A.LtOp , A.LeOp 
                    , A.GtOp , A.GeOp ]

val err = ErrorMsg.error

fun out msg pos = err pos msg

fun errorInt (pos, ty) =
    err pos ("INT required, " ^ PT.asString ty ^ " provided")

fun errorIfTest(pos, ty) =
    err pos ("INT required in test, " ^ PT.asString ty ^ " provided")

fun errorIfElse(pos, ty1, ty2) =
    err pos ("then and else exp must be same type, then exp is type " 
        ^ PT.asString ty1 ^ " and else exp is type " ^ PT.asString ty2)

fun errorIfThen(pos, ty) =
  err pos ("UNIT required in then clause, " ^ PT.asString ty ^ " provided")

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


fun makeIfThen( {exp = te, ty = tety} : TAbs.exp, 
  {exp = th, ty = thty} : TAbs.exp, pos) =
  if tety = Ty.INT then
    (* construct the pairs we need *)
    let val tst = makePair(te,tety)
        val thn = makePair(th, thty)
        (* Since no if, then clause must be a unit *)
        in if thty <> Ty.UNIT then
          (errorIfThen(pos, thty); ERRORPAIR)
        (* Everything is kosher, make the relevant TAbs node *)
        else makePair( TAbs.IfExp {
                      test = tst,
                      thn = thn,
                      els = NONE }, Ty.UNIT)
      end
    else (errorIfTest(pos, tety); ERRORPAIR)

(*  Big clause.
    The order is test, then, else, pos
 *)
fun makeIfElse( {exp = te, ty = tety} : TAbs.exp,
  {exp = th, ty = thty} : TAbs.exp,
  {exp = el, ty = elty} : TAbs.exp , pos) =
  if tety = Ty.INT then
    if thty = elty then
      (* everything went well, make the things needed *)
      let val test = makePair(te, tety)
          val thn = makePair(th, thty)
          val els = SOME(makePair(el, elty))
          in
            makePair( TAbs.IfExp {
                test = test,
                thn = thn,
                els = els
              }, thty)
          end 

    else (errorIfElse(pos, thty, elty); ERRORPAIR)
  else (errorIfTest(pos, tety); ERRORPAIR)



fun transTy (tenv, t) = Ty.ERROR (* TODO *)

fun transExp (venv, tenv, extra : extra) =
    let
        (* this is a placeholder value to get started *)
        val TODO = {exp = TAbs.ErrorExp, ty = Ty.ERROR}
        val NILPAIR = {exp = TAbs.NilExp, ty = Ty.UNIT}

        (*
        val OrderTypes : Ty.ty list = [Ty.INT, Ty.STRING ]
        val EqTypes : Ty.ty list  = [Ty.INT : Ty.ty , Ty.RECORD : Ty.ty, Ty.ARRAY : Ty.ty, Ty.STRING : Ty.ty]
        *)

        fun trexp (A.NilExp) = TODO
          | trexp (A.VarExp var) = trvar(var)
          | trexp (A.IntExp value) = makePair (TAbs.IntExp(value), Ty.INT)
          | trexp (A.StringExp(s,_)) = makePair (TAbs.StringExp(s), Ty.STRING)
          | trexp (A.OpExp({left = left, oper = oper, right = right, pos = pos})) = trBinop(left, oper, right, pos)
          | trexp(A.SeqExp(explist)) = trseqexp(explist) (* *)
          | trexp(A.IfExp(ifdata)) = trifexp(ifdata)

          | trexp(A.LetExp(letdata)) = trletexp(letdata) (* *)

          | trexp(A.ForExp(fordata)) = trforexp(fordata)

          | trexp _ = (print("sry, got nothing\n"); TODO)

          (*
            * When we are making a let expression, we have to use the transExp to interpt with the extended enviorment
            *)

        and trforexp({  var = s,
                        escape = e,
                        lo = low,
                        hi = high,
                        body = exp,
                        pos = pos} : A.fordata) = TODO

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
          case els of
              NONE => makeIfThen(trexp(test), trexp(thn), pos)
            | SOME(e) => makeIfElse(trexp(test), trexp(thn), trexp(e), pos)

        and trBinop(exp1, opr, exp2, pos) =
          if List.exists (eql opr) Equality  
              then makeEqExp(trexp(exp1), convertOper(opr), trexp(exp2), pos)
          else if List.exists (eql opr) Ordering  
            then makeOrdExp(trexp(exp1), convertOper(opr), trexp(exp2), pos)
          else makeStdOpExp(trexp(exp1), convertOper(opr), trexp(exp2), pos)
            (* Hackey way to check if an element is in a list *)
        and eql opt1 opt2 = (opt1 = opt2)

        and makeEqExp({exp = exp1, ty = ty1} : TAbs.exp, 
          opr, {exp = exp2, ty = ty2} : TAbs.exp, pos) = 
          case ty1 of
             Ty.STRING => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            | Ty.INT => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            | Ty.RECORD(_) => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            | Ty.ARRAY(_) => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            |_ => (err pos (" LHS is type" ^ (PT.asString ty1) ^ " must be of INT, STRING, RECORD or ARRAY"); ERRORPAIR)

        and makeAux(exp1, ty1, exp2, ty2, pos, opr) =
          if ty1 = ty2 then
            makeBinop( makePair(exp1, ty1), opr, makePair(exp2, ty2), ty1)
          else
            (err pos ("LHS has type " ^ (PT.asString ty1) ^ " RHS has type " ^ (PT.asString ty2) ^ ". They must be equal"); ERRORPAIR)


        and makeOrdExp({exp = exp1, ty = ty1} : TAbs.exp, 
          opr, {exp = exp2, ty = ty2} : TAbs.exp, pos) = 
          case ty1 of
             Ty.STRING => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            | Ty.INT => makeAux(exp1, ty1, exp2, ty2, pos, opr)
            |_ => (err pos (" LHS is type" ^ (PT.asString ty1) ^ " must be of INT, STRING"); ERRORPAIR)

        and makeStdOpExp({exp = exp1, ty = ty1} : TAbs.exp, 
          opr, {exp = exp2, ty = ty2} : TAbs.exp, pos) = 
            if checkInt(ty1, pos) then
              if checkInt(ty2, pos) then
                makeBinop(makePair(exp1,ty1), opr,
                  makePair(exp2,ty2), ty1)
              else ERRORPAIR
            else ERRORPAIR

        and makeBinop(texp1, opr, texp2, ty) =
          makePair( TAbs.OpExp {left = texp1,
            oper = opr, right = texp2}, ty)

        and trletexp(_) = ERRORPAIR
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
