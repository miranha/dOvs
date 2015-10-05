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
    (
      case ty of 
        ref (SOME t) => actualTy t pos
        | ref NONE => Ty.ERROR)
  | actualTy t _ = t

fun checkInt (ty, pos) =
    case (actualTy ty pos)
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

fun makeWhile({exp = tstexp, ty = tstty} : TAbs.exp,
              {exp = bdyexp, ty = bdyty} : TAbs.exp ) = 
                  makePair( TAbs.WhileExp {
                            test = makePair(tstexp, tstty),
                            body = makePair(bdyexp,bdyty)
                            }, Ty.UNIT)

fun makeFor(vr, scp, {exp = lexp, ty = lty} : TAbs.exp,
                    {exp = hexp, ty = hty} : TAbs.exp,
                    {exp = bdyexp, ty = bdyty} : TAbs.exp, venv) = (*TODO Still unfinished.*)
        makePair(TAbs.ForExp{var = vr,
                             escape = scp,
                             lo = makePair(lexp,lty),
                             hi = makePair(hexp,hty),
                             body = makePair(bdyexp,bdyty)}, Ty.UNIT)

fun transTy (tenv, t) = (*Ty.ERROR*) (* TODO *)
  let
    (*Some defintions*)
  in
    case t of
      A.NameTy(nt,pos) => let val res = lookupTy tenv nt pos 
                                  in (case res of NONE => Ty.ERROR
                                    | SOME(_) => Ty.NAME(nt, ref(res))
                                    )
                                  end

        (*TODO: Add support for Records*)
      (* | A.ArrayTy(at,pos) => Ty.ARRAY((lookupTy tenv at pos), ref()) *)
      | _ =>  Ty.ERROR
  end 

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

          | trexp(A.WhileExp(whiledata)) = trwhileexp(whiledata)
          | trexp(A.ForExp(fordata)) = trforexp(fordata, venv)
          | trexp(A.CallExp(calldata)) = trcallexp(calldata)

          | trexp _ = (print("sry, got nothing\n"); TODO)

              (*The following takes as input the data from a while expression, and tries to pattern match first the test against
                Ty.INT, if that succedes then it will try and match the body against Ty.UNIT. If correct, then we have a working Tiger While loop.*)

        and trwhileexp({test = tst, body = bdy, pos = ps} : A.whiledata) = 
                let
                  val {exp = test, ty = testty} : TAbs.exp = trexp(tst)
                  val {exp = body, ty = bodyty} : TAbs.exp = trexp(bdy)
                  val testexp = makePair(test, testty)
                  val bodyexp = makePair(body, bodyty)
                in
                  case testty of
                    Ty.INT => ( case bodyty of 
                    Ty.UNIT => (makeWhile(testexp, bodyexp))
                    | _ => (print("Failed 2.nd"); TODO) )
                    | _ => (print("Failed 1.st"); TODO)
                end

              (* venv=S.enter(venv,name,E.VarEntry{ty=ty})} *)

        and trforexp({var = va, escape = esc, lo = l, hi = h, body = bdy, pos = ps}: A.fordata, venv) = let
          val subvenv = S.enter(venv,va,E.VarEntry{ty=Ty.INT})
          val {exp = lexp, ty = lty} : TAbs.exp = trexp(l)
          val {exp = hexp, ty = hty} : TAbs.exp = trexp(h)
          val {exp = bodyexp, ty = bodyty} : TAbs.exp = transExp(subvenv, tenv, {}) bdy
          val lpair = makePair(lexp,lty)
          val hpair = makePair(hexp, hty)
          val bdypair = makePair(bodyexp,bodyty)
        in
          case lty of
              Ty.INT => ( case hty of
                          Ty.INT => ( case bodyty of
                                        Ty.UNIT => (makeFor(va, esc, lpair, hpair, bdypair, venv)) (*TODO: add symbol to env, and make it decoupled from standard env.*)
                                        | _ => (print("FailedbodyTY"); TODO)
                                    )
                          |_ => (print("FailedHighTY");TODO) 
                        )
              |_ => (print("FailedLowTY"); TODO)
        end
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
          if (actualTy ty1 pos) = (actualTy ty2 pos) then
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

        and trletexp({decls=decls, body=body, pos = pos} : A.letdata) = let val {decls = delcs', venv=venv', tenv=tenv'} =
          transDecs(venv,tenv, decls, extra) 
        in {exp = TAbs.LetExp { decls = delcs', body =  (transExp(venv',tenv',extra) body)}, ty = Ty.UNIT}
          end

        and trcallexp({func = name, args = args, pos = pos}) =
          let val f = lookupVar venv name pos
          in case f of
            SOME(E.FunEntry{formals = formals, result = resultTy}) => 
                      let val {tylst, explst, poslst} = makeArgsList(args,[], [], [])
                      in
                          if checkParam(tylst, formals, poslst, pos, true) then
                            makePair(TAbs.CallExp {func = name, args = explst}, resultTy)
                          else (out ("args in function call to " ^ S.name name ^ " did not match formal parameters") pos; ERRORPAIR)
                        end
            | SOME(E.VarEntry(_)) => (out (S.name name ^ " is a variable in this scope. Did you override your function? ") pos; ERRORPAIR)
            | NONE => (out ("Function " ^ S.name name ^ " is not defined") pos; ERRORPAIR)
          end

        and makeArgsList([], tyLst, expLst, posLst) = {tylst = tyLst, explst = expLst, poslst = posLst}
          | makeArgsList((exp, pos)::xs, tyLst, expLst, posLst) =
              let val {exp, ty} = trexp exp
              in makeArgsList (xs, tyLst @ [ty], expLst @ [makePair(exp, ty)], posLst @ [pos])
              end

        and checkParam([], [], _, _, noErrs) = noErrs
          | checkParam(callTy::xs1, formalTy::xs2, pos::xs3, callPos, noErrs) =
                if (actualTy callTy pos) = (actualTy formalTy pos) then
                  checkParam(xs1, xs2, xs3, callPos, noErrs)
                else (out "type of the expression did not match the formal declaration" pos; checkParam(xs1, xs2, xs3, callPos, false))
          | checkParam(_, _, _ , callPos, noErrs) = (out "there is not the exact same amount of args in call as there are formal declarations" callPos; false)

    in
        trexp
    end

and transDec ( venv, tenv
             , A.VarDec {name, escape, typ = NONE, init, pos}, extra : extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)


  | transDec ( venv, tenv
             , A.VarDec {name, escape, typ = SOME (s, pos), init, pos=pos1}, extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv}  (* TODO *)

  | transDec (venv, tenv, A.TypeDec typdecs, extra) =
    let 
      fun enterTydec([] , tyDecls, tenv, venv) = {decl = TAbs.TypeDec(tyDecls), tenv = tenv, venv = venv}
        | enterTydec({name,ty,pos}::tl,tyDecls, tenv, venv)=
          let val resTy = transTy(tenv, ty)
            val decl = {name = name, ty = resTy}
          in
            enterTydec(tl,tyDecls @ [decl]
              , S.enter(tenv,name,transTy(tenv,ty)), venv)
          end

    in
      enterTydec(typdecs,[], tenv, venv)
    end

  | transDec (venv, tenv, A.FunctionDec fundecls, extra) =
    {decl = TODO_DECL, tenv = tenv, venv = venv} (* TODO *)

*)

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
