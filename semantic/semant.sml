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

type extra = {inloop : bool}

fun inLoop({inloop = b}: extra):bool = b

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

fun errorNilNoneVar(pos, id) =
  err pos ((S.name id) ^ " can't be declared as nil. " ^ (S.name id) ^" must explicitly declared of type RECORD") 

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
        case tyOpt of
          NONE => (out (" Type " ^ S.name sym ^ " has not been defined.") pos; NONE)
          |_ => tyOpt
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
     {
    var = varDesc,
    ty = ty
  }:TAbs.var

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

fun transTy (tenv, t, callName, pos') = (*Ty.ERROR*) (* TODO *)
  let
    fun fdatafun([] , acc) = acc
      | fdatafun({name, escape, typ=(sym,pos1), pos}::xs,acc)=
          let val ty= lookupTy tenv sym pos1
          in
            case ty of
              SOME(t)=>fdatafun(xs,acc@[(name,t)])
              | NONE => acc
            end
    val name = lookupTy tenv callName pos'
    val returnName = (case t of A.NameTy(nt,_) => [nt]
                          |_  => [])
  in
    (case name of
        SOME(Ty.NAME((_,reference))) =>
          reference :=
        (case t of
              A.NameTy(nt,pos) => let val res = lookupTy tenv nt pos
                                      in
                                      (case res of NONE => SOME(Ty.ERROR)
                                                  | SOME(_) => res
                                        )
    
                                          end
             | A.ArrayTy(at, pos) => let val res = lookupTy tenv at pos 
                                          in (case res of NONE => SOME(Ty.ERROR)
                                            | SOME(t) => SOME(Ty.ARRAY(t, ref()))
                                            )
                                          end
              | A.RecordTy(data) => let val recdata = fdatafun(data, []) 
                                      in
                                        SOME(Ty.RECORD(recdata, ref()))
                                      end)
            (*TODO: Add support for Records*)
          (* | A.ArrayTy(at,pos) => Ty.ARRAY((lookupTy tenv at pos), ref()) *)
          (*| _ =>  Ty.ERROR*)
          | _ => (out ("couldn't find typename " ^ S.name callName) pos'); returnName)
  end 

(* Calls floyds algorithm on all type names that could lead to
  a cycle.
  Sets it up so that one pointer is one step ahead of the other.
  If not possible to do so, there can't be a cycle
 *)
fun checkCycles( tenv, (name,pos)::xs ) =
  let val tyOpt = lookupTy tenv name pos
  in
    (case tyOpt of
          SOME(t) =>
            (case t of
                Ty.NAME(_,ref(SOME(t'))) => (checkFloyd(t,t', name, pos); checkCycles(tenv, xs))
                | _ => checkCycles( tenv, xs)
              )
          | _=> checkCycles(tenv, xs)
          )
    end
  | checkCycles( _, []) = ()

(* Uses floyds algorithm for detecting cycles, i.e. one pointer jumps length 1, the other length 2 *)
and checkFloyd(Ty.NAME(name1,ref(SOME(t))),Ty.NAME(name2,ref(SOME(t'))), name, pos) =
        if name1 = name2 then
          (out ("Cycle found at type " ^ S.name name) pos)
        else
          ( case t' of
            Ty.NAME(_,ref(SOME(t''))) => checkFloyd(t,t'', name, pos)
            | _=> ()
            )
    | checkFloyd(_,_,_,_) = ()

fun transExp (venv, tenv, extra : extra) =
    let
        (* this is a placeholder value to get started *)
        val TODO = {exp = TAbs.ErrorExp, ty = Ty.ERROR}
        val NILPAIR = {exp = TAbs.NilExp, ty = Ty.UNIT}

        (*
        val OrderTypes : Ty.ty list = [Ty.INT, Ty.STRING ]
        val EqTypes : Ty.ty list  = [Ty.INT : Ty.ty , Ty.RECORD : Ty.ty, Ty.ARRAY : Ty.ty, Ty.STRING : Ty.ty]
        *)

        fun trexp (A.NilExp) = {exp = TAbs.NilExp, ty = Ty.NIL}

          | trexp (A.VarExp var) = let val {var=var', ty=ty} = trvar var
                                        val v = TAbs.VarExp(makeVar(var',ty))
                                     in makePair(v,ty) end

          | trexp (A.BreakExp(pos)) = (
                if inLoop(extra) then
                  {exp = TAbsyn.BreakExp, ty = Ty.UNIT}
                else (out "ILLEGAL EXPRESSION: Not in loop" pos; {exp = TAbsyn.BreakExp, ty = Ty.UNIT})
            )


          | trexp (A.IntExp value) = makePair (TAbs.IntExp(value), Ty.INT)
          | trexp (A.StringExp(s,_)) = makePair (TAbs.StringExp(s), Ty.STRING)

          | trexp (A.OpExp({left = left, oper = oper, right = right, pos = pos})) = trBinop(left, oper, right, pos)
          | trexp(A.SeqExp(explist)) = trseqexp(explist) (* *)
          | trexp(A.IfExp(ifdata)) = trifexp(ifdata)

          | trexp(A.LetExp(letdata)) = trletexp(letdata) (* *)

          | trexp(A.WhileExp(whiledata)) = trwhileexp(whiledata)
          | trexp(A.ForExp(fordata)) = trforexp(fordata, venv)
          | trexp(A.CallExp(calldata)) = trcallexp(calldata)

          | trexp(A.ArrayExp(arxdata)) = trarray(arxdata)

          |trexp(A.RecordExp(rcxdata)) = trrecord(rcxdata)

          | trexp _ = (print("sry, got nothing\n"); TODO)

              (*The following takes as input the data from a while expression, and tries to pattern match first the test against
                Ty.INT, if that succedes then it will try and match the body against Ty.UNIT. If correct, then we have a working Tiger While loop.*)

        and trrecord({fields=fields, typ=typ, pos = pos}) = let fun travRec([], [], acc) = acc
                                                                  | travRec((name,exp, pos)::xs, (tname,tty)::xl, acc) = 
                                                                    if name=tname then 
                                                                      let val {exp=exp', ty=ty'} = trexp exp
                                                                        in
                                                                          if (actualTy ty' pos) = (actualTy tty pos) 
                                                                            then travRec(xs,xl,acc@[(name,makePair(exp', ty'))])
                                                                          else acc
                                                                      end
                                                                    else acc
                                                                      

                                                                val recty = lookupTy tenv typ pos
                                                                val resty = (case recty of
                                                                  SOME(t) => t
                                                                  | NONE => Ty.ERROR)
                                                              in
                                                                case recty of
                                                                          SOME(Ty.RECORD(t,_)) => 
                                                                                          makePair(TAbs.RecordExp{fields=travRec(fields,t, [])},resty)
                                                                                    | _ => (out "Not a array type" pos; TODO)
                                                                            end


        and trarray({typ=typ, size=size, init=init, pos=pos}: A.arxdata) = let  
                                                                              val {exp= sizeexp, ty = sizety} = trexp(size)
                                                                              val {exp = initexp, ty = initty} = trexp(init)
                                                                              val sizepair = makePair(sizeexp, sizety)
                                                                              val initpair = makePair(initexp, initty)
                                                                              val arryty = lookupTy tenv typ pos
                                                                              val ty' = case arryty of 
                                                                                SOME(t) => t
                                                                                | _ => Ty.ERROR
                                                                            in
                                                                              case arryty of
                                                                                SOME(Ty.ARRAY(t,_)) => 
                                                                                  if (checkInt(sizety, pos)) then
                                                                                    (*Check Init Ty matches Array Ty*)
                                                                                      if (actualTy initty pos) = (actualTy t pos) then
                                                                                          makePair(TAbs.ArrayExp{size=sizepair,init=initpair},ty')
                                                                                        else (out "Not maching type in Array" pos; TODO)
                                                                                  else (out "Failed to match size to INT" pos; TODO)
                                                                                    | _ => (out "Not a array type" pos; TODO)
                                                                            end
        and trwhileexp({test = tst, body = bdy, pos = ps} : A.whiledata) = let
                                                                            val {exp = test, ty = testty} : TAbs.exp = trexp(tst)
                                                                            val {exp = body, ty = bodyty} : TAbs.exp = transExp(venv, tenv, {inloop = true}) bdy
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
                                              |_ => (errorVar(pos, id); makeVar(TAbs.SimpleVar(id),Ty.ERROR))
                                              end
          | trvar (A.FieldVar (var, id, pos)) = (*makeVar(TAbs.SimpleVar(S.symbol "TODO"),Ty.ERROR)*)
                                                  let val {var=varv,ty=tyv} = trvar var
                                                  in
                                                  (case tyv of
                                                    Ty.RECORD(fi, _) => (
                                                                          case List.find (fn x=> #1(x) = id) fi
                                                                            of SOME(sym,tyv') => makeVar(TAbs.FieldVar((makeVar(varv,tyv),
                                                                                      id)), tyv')
                                                                            | NONE => makeVar(TAbs.SimpleVar(S.symbol "TODO2"),Ty.ERROR)
                                                                            )

                                                    | _ => makeVar(TAbs.SimpleVar(S.symbol "TODO2"),Ty.ERROR)
                                                    )
                                                end
          | trvar (A.SubscriptVar (var, exp, pos)) = (*makeVar(TAbs.SimpleVar(S.symbol "TODO"),Ty.ERROR)*)
                                                        let val {var=expv, ty=tyv} = trvar var
                                                          val {exp=expe, ty=tye} = trexp exp
                                                        in
                                                          (case tyv of
                                                            Ty.ARRAY(tyv',_) => (if checkInt(tye,pos) then 
                                                                                    makeVar(TAbs.SubscriptVar((makeVar(expv,tyv),
                                                                                      makePair(expe,tye))), tye)
                                                                                  else makeVar(TAbs.SimpleVar(S.symbol "TODO1"),Ty.ERROR)) 
                                                            | _ => makeVar(TAbs.SimpleVar(S.symbol "TODO2"),Ty.ERROR)
                                                            )
                                                        end
        
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
          val {exp, ty} = (transExp(venv',tenv',extra) body)
        in {exp = TAbs.LetExp { decls = delcs', body = makePair(exp, ty)}, ty = ty}
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
          let val {exp, ty} = transExp(venv, tenv, extra) init
            val decl' = TAbs.VarDec{  name = name, escape = escape, ty = ty, init = makePair(exp, ty)}
            val nildecl =  TAbs.VarDec{  name = name, escape = escape, ty = Ty.ERROR, init = makePair(exp, ty)}
          in case (actualTy ty pos) of
            Ty.NIL => (errorNilNoneVar (pos, name); {decl = nildecl, tenv = tenv, venv = S.enter(venv, name, E.VarEntry{ty = Ty.ERROR})}) (* TODO *)
            | _ => {decl = decl', tenv = tenv, venv = S.enter(venv, name, E.VarEntry{ty = ty})}
          end


  | transDec ( venv, tenv
             , A.VarDec {name, escape, typ = SOME (s, pos), init, pos=pos1}, extra) =
                let val {exp, ty} = transExp(venv, tenv, extra) init
                  val ty' = lookupTy tenv s pos
                  val errDecl = TAbs.VarDec{  name = name, escape = escape, ty = Ty.ERROR, init = makePair(exp, ty)}
                  val errReturn = {decl = errDecl, tenv = tenv, venv = S.enter(venv, name, E.VarEntry{ty = Ty.ERROR})}
                  val decl' = TAbs.VarDec { name = name, escape = escape, ty = ty, init = makePair(exp, ty)}
                in
                  case ty' of
                    NONE => errReturn
                    |SOME(t) => if (actualTy t pos) = (actualTy ty pos1)
                                    then {decl = decl', tenv = tenv, venv = S.enter(venv, name, E.VarEntry{ty = actualTy t pos})} 
                                else (out ("Variable " ^ S.name name ^ " declared as type " ^ S.name s ^ " and RHS has type " ^ PT.asString ty ^ " which is non compatabile") pos1; 
                                  errReturn)
                end
      (* TODO *)


  | transDec (venv, tenv, A.TypeDec typdecs, extra) =
    let 
      fun enterTydec([] , tyDecls, tenv, venv, nameposlst) = (checkCycles(tenv, nameposlst); {decl = TAbs.TypeDec(tyDecls), tenv = tenv, venv = venv})
        | enterTydec({name,ty,pos}::tl,tyDecls, tenv, venv, nameposlst)=
          let val lst = transTy(tenv, ty, name, pos)
            val resty = lookupTy tenv name pos
            val decl = (case resty of
                          SOME(t) => {name = name, ty = t}
                          |NONE => {name = name, ty = Ty.ERROR}
                        )
          in
            enterTydec(tl,tyDecls @ [decl]
              , tenv, venv, (name,pos)::nameposlst)
          end
      fun prepareEnv([], tenv) = tenv
        | prepareEnv({name,ty,pos}::tl, tenv) =
            prepareEnv(tl, S.enter(tenv, name, Ty.NAME((name,ref(NONE)))))
      val tenv' = prepareEnv(typdecs, tenv)
    in
      enterTydec(typdecs,[], tenv', venv,[])
    end

  | transDec (venv, tenv, A.FunctionDec fundecls, extra) =
    let (*val {venv, funlst, ventries} = transFuncs(fundecls, [], venv, tenv, extra, [])
    *) val venv' = getFunctionHeaders(fundecls, [], venv,tenv,extra)
      val funlst = checkFunctions (venv', tenv, fundecls, extra, [], [])
    in
    {decl = TAbs.FunctionDec(funlst), tenv = tenv, venv = venv'} (* TODO *) end

  and getFunctionHeaders({name, params,result,body,pos}::xs,names,venv,tenv,extra) =
    let
     val tOpt = (case result of SOME((id, pos')) => lookupTy tenv id pos'
      | NONE => SOME(Ty.UNIT))
     val {venv = v', lst =l', tylst = tylst, ventries = ventries} = transParams(params, venv, tenv, [], [], [], [])
    in
      if List.exists (fn x => x = name) names 
        then (out (S.name name ^ " has already been declared, skipping it") pos; getFunctionHeaders(xs,names,venv,tenv,extra))
      else
        let
            fun validtype topt = (case topt of
                         NONE =>(out (" Function " ^ S.name name ^ " return type is undeclared"); Ty.ERROR)
                       | SOME(t) => t) 
        in 
          getFunctionHeaders(xs, name::names,S.enter(venv, name, E.FunEntry{formals = tylst, result = (validtype tOpt)}),tenv,extra)
        end
    end
    | getFunctionHeaders([],_,venv,tenv,extra) = venv

  and checkFunctions(venv, tenv, {name, params, result, body, pos}::fundecls, extra, functions, names) =
    if (List.exists (fn x => x = name) names) then
      (out ("Function " ^ S.name name ^ " is already declared, will keep old definition") pos;
        checkFunctions(venv, tenv, fundecls, extra, functions, names))
    else
    let val varOpt = lookupVar venv name pos
      val typeLst = (case varOpt of 
                            SOME(E.FunEntry{formals = formals, result =_}) => formals
                          | _ => [])
      val res = (case varOpt of 
                            SOME(E.FunEntry{formals = _, result = resultty}) => resultty
                          | _ => Ty.ERROR)
      val {venv = v, lst = lst, tylst = tylst, ventries = ventries} = transParams(params, venv, tenv, [], [], [], [])
      fun enterInVenv (venv, (name, ventry)::xs) = enterInVenv(S.enter(venv, name, ventry), xs)
        | enterInVenv (venv, []) = venv
      val venv' = enterInVenv (venv, ventries)
      val {exp, ty} = transExp (venv', tenv, extra) body
      val f = {name = name, params = lst, resultTy = res, body = makePair(exp, ty)}
      val errf = {name = name, params = lst, resultTy = Ty.ERROR, body = makePair(exp, ty)} : TAbs.fundecldata
    in
      if (actualTy res pos) = (actualTy ty pos) then
        checkFunctions(venv, tenv, fundecls, extra, functions  @ [ f ], name::names)
      else
      (out ("The result type " ^ PT.asString res ^ " is not compatible with body type" ^ PT.asString ty) pos;
              checkFunctions(venv, tenv, fundecls, extra, functions  @ [ errf ], name::names)
              )
    end
    | checkFunctions(venv, tenv, [], extra, functions, names) = functions

  and transParams({name = name, escape = escape, typ = (typ, pos1), pos = pos}::xs, venv, tenv, lst, tylst, names, ventries) =
    if List.exists (fn x => x = name) names
      then (out (" parameter " ^ S.name name ^ " already declared, will keep old definition") pos1; transParams(xs, venv, tenv, lst, tylst, names, ventries))
    else
      let val ty = lookupTy tenv typ pos1 (* Check if we have a defined type *)
      in case ty of NONE => 
        let val fData = {name = name, escape = escape, ty = Ty.ERROR} : TAbs.fielddata
        in
        (out ("parameter " ^ S.name name ^ " is decalered as type " ^ S.name typ ^ ", as type that has yet to be declared") pos;
                              transParams(xs, S.enter(venv, name, E.VarEntry{ty = Ty.ERROR}), tenv, lst@[fData], tylst @ [Ty.ERROR], name::names, 
                                ventries @ [(name, E.VarEntry{ty = Ty.ERROR})]))
        end
        | SOME(t) => 
          let val fData = {name = name, escape = escape, ty = t} : TAbs.fielddata in
        transParams(xs, S.enter(venv,name, E.VarEntry{ty = t}), tenv, lst@[fData], tylst @ [t], name::names, ventries @ [(name, E.VarEntry{ty = t})]) end
      end
    | transParams([], venv, _, lst, tylst, _, ventries) = {venv = venv, lst = lst : TAbs.fielddata list, tylst = tylst : Ty.ty list, ventries = ventries}

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
    transExp (Env.baseVenv, Env.baseTenv, {inloop = false}) absyn

end (* Semant *)
