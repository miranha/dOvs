signature IRGEN =
sig
  val transProg: TAbsyn.exp -> Translate.frag list
end

structure IRgen :> IRGEN =
struct

structure S = Symbol
structure TAbs = TAbsyn
structure E = IRgen_Env
structure Ty = Types
structure Tr = Translate

type extra = { level : Tr.level (* frame of enclosing function *)
             , break : Tr.breakpoint option } (* jump to this label on 'break' *)


fun addBreakLabel ({level=l,break=_}, breakpoint) = {level=l,break=SOME(breakpoint)}
fun getLevel {level=l,break=_} = l
fun setLevel {level=_,break=b} l= {level = l, break = b}

(* Helper functions *)

fun actualTy (Ty.NAME (_, ref (SOME ty))) = actualTy ty
  | actualTy t = t



val TODO = {exp=Tr.bogus, ty=Ty.ERROR}

exception Bug of string;        

fun transExp (venv, extra : extra) =
    let                               
        fun trexp {exp=TAbs.NilExp, ty} = {exp=Tr.nil2IR (), ty=ty}
          | trexp{exp=TAbs.IntExp i, ty} = {exp=Tr.int2IR(i), ty=ty}
          | trexp{exp=TAbs.OpExp({left=left,oper=oper,right=right}), ty=ty} = {exp=trBinop(left,oper,right), ty=ty} (*TODO: Add support for strings too*)
          | trexp{exp=TAbs.IfExp({test=test,thn=thn,els=SOME(els)}), ty=ty} = {exp=trIfElseExp(test,thn,els), ty=ty}
          | trexp{exp=TAbs.IfExp({test=test,thn=thn,els=NONE}), ty=ty} = {exp=trIfThenExp(test,thn), ty=ty}
          | trexp{exp=TAbs.VarExp(var), ty=ty} = {exp=trVarExp(var), ty=ty}
          | trexp{exp=TAbs.LetExp(letdata), ty=ty} = {exp=trLetExp(letdata), ty=ty}
          | trexp{exp=TAbs.SeqExp(seqdata), ty=ty} = {exp=trSeqExp(seqdata, ty), ty=ty} (*TODO: What if sequence is empty??*)
          | trexp{exp=TAbs.WhileExp(whiledata), ty=ty} = {exp=trWhileExp(whiledata), ty=ty}
          | trexp{exp=TAbs.AssignExp(assigndata), ty=ty} = {exp=trAssignExp(assigndata), ty=ty}
          | trexp{exp=TAbs.ForExp(fordata), ty=ty} = {exp=trForExp(fordata), ty=ty}
          | trexp{exp=TAbs.StringExp str, ty=ty} = {exp=Tr.string2IR(str), ty=ty}
          | trexp{exp=TAbs.BreakExp, ty=ty} = {exp=trBreakExp(), ty=ty}
          | trexp{exp=TAbs.ArrayExp(arrdata), ty=ty} = {exp=trArrayExp(arrdata), ty=ty}
          | trexp{exp=TAbs.RecordExp(recdata), ty=ty} = {exp=trRecordExp(recdata), ty=ty}
          | trexp{exp=TAbs.CallExp(calldata), ty=ty} = {exp=trCallExp(calldata,ty), ty=ty}
          | trexp _ = TODO
           
        and trBreakExp() = 
          let
            val break' = (#break extra)
          in
            case break' of SOME(br') => Tr.break2IR (br') 
                          | NONE => Tr.bogus (*TODO: Look into this again*)
          end

          and trBinop(left,oper,right) = 
            let val {exp=lexp, ty=tyl} = trexp left
                val {exp=rexp, ty=_} = trexp right 
              in
                case actualTy tyl of (*Both left and right types are same type-> ensured by semant*)
                  Ty.STRING => Tr.stringOp2IR(oper,lexp,rexp) (*TODO: Check that this works as expected*)
                  | Ty.INT => Tr.intOp2IR(oper,lexp,rexp) (*TODO: Also arrays and records can be tested for equality*)
                  | _ => Tr.bogus (*Should never happend-> ensured by semnat*)
            end

          and trIfElseExp(test,thn,els) =
            let val {exp=test', ty=_} = trexp test
                val {exp=thn', ty=_} = trexp thn
                (*val elss = case els of SOME(e)=>e*)
                val {exp=els', ty=_} = trexp els 
              in
                Tr.ifThenElse2IR(test',thn',els')
            end

          and trIfThenExp(test,thn) =
            let val {exp=test', ty=_} = trexp test
                val {exp=thn', ty=_} = trexp thn
              in
                Tr.ifThen2IR(test',thn')
            end

          and trVarExp(var) =
            let
              val {exp=var', ty=_} = trvar var
            in
              var'
            end
            
          and trLetExp({decls=decls, body=body}: TAbs.letdata) =
            let val ({venv=venv'}, expl') = transDecs(venv,decls,extra,[])
                val {exp=exp', ty=_} = transExp(venv',extra) body
            in
              Tr.let2IR(expl',exp')
            end

          and trSeqExp(seqdata,ty) = 
            let
              val seqlist = trSeqExpAux(seqdata,[])
            in
              case actualTy ty of
                Ty.UNIT => Tr.seq2IR(seqlist)
                | _ => Tr.eseq2IR(seqlist) (*TODO: Is this okay?*)
            end
          
          and trSeqExpAux(seq::xs, acc) = 
            let val {exp=res, ty=_} = trexp seq 
            in 
              trSeqExpAux(xs,acc@[res]) 
            end
            | trSeqExpAux([], acc) = acc

          and trWhileExp({test=test, body=body}: TAbs.whiledata) = 
            let
              val done' = Tr.newBreakPoint "while_done" (*TODO: We need to use the break from outside I think..*)
              val {exp=test', ty=_} = trexp test
              val {exp=body', ty=_} = transExp(venv, addBreakLabel(extra, done')) body
              
              (*val break' = case (#break extra) of SOME(e) => e*) (*TODO: make it nice*)
            in
              Tr.while2IR(test',body',done')
            end

          and trAssignExp({var=var, exp=exp}) = 
            let
              val {exp=var', ty=_} = trvar var
              val {exp=exp', ty=_} = trexp exp
            in
              Tr.assign2IR(var',exp')
            end

          and trForExp({var=varname, escape=escape, lo=lo,hi=hi, body=body}) =
            let
              val done' = Tr.newBreakPoint "for_done"
              val {exp=lo', ty=_} = trexp lo
              val {exp=hi', ty=ty} = trexp hi
              val level' = (#level extra)
              val acc' = Tr.allocLocal level' (!escape)
              val var' = Tr.simpleVar(acc',level')
              val venv' = S.enter(venv,varname,(E.VarEntry {access=acc'
                                                 , ty=actualTy ty
                                                 , escape= escape}))
              val {exp=body', ty=_} = transExp(venv', addBreakLabel(extra, done')) body (*TODO: Consider sending value enviroment*)
            in
              Tr.for2IR(var',done',lo',hi',body')
            end

          and trArrayExp({size=size, init=init}) = (* using Tr.array2IR *)
            let 
              val {exp=sizeexp', ty=_} = trexp size
              val {exp=initexp', ty=_} = trexp init
            in
              Tr.array2IR(sizeexp', initexp')
            end

          and trRecordExp({fields=fields}) = (*{ fields: (S.symbol * exp) list}*)
            let
              val expl = trRecExpAux(fields,[])
            in
              Tr.record2IR(expl)
            end

          and trRecExpAux((_, exp)::xs, acc) = 
            let val {exp=res, ty=_} = trexp exp 
            in 
              trRecExpAux(xs,acc@[res]) 
            end
            | trRecExpAux([], acc) = acc


          and trCallExp({func=func, args=args},ty) = (*calldata    = { func: S.symbol, args: exp list}*)
            let
              fun arg {exp=exp, ty=ty'} = exp
              val exps = map arg (map trexp args)
              val {formals, result, label,
                      level} = (case( S.look (venv, func)) of 
                                SOME(E.FunEntry(e)) => e
                                |_ => raise Bug "Function have not been entered into enviorment")
              in
              case actualTy ty of
                Ty.UNIT => Tr.procCall2IR(level, getLevel extra, label, exps)
                | _ => Tr.funCall2IR(level, getLevel extra, label, exps)
            end
        (* The below code suggest how to translate depending what case
        you are in, however, uncommenting the section would result in
        type-errors. You will have to write the rest of the cases your
        selves.

        fun trexp (A.NilExp) _ =
            {exp = Tr.nil2IR (), ty = Ty.NIL}

          | trexp (A.VarExp var) break =
            trvar var false break

          | trexp (A.IntExp i) _ =
            raise TODO

          | trexp (A.StringExp (str, pos)) _ =
            raise TODO

          | trexp (A.OpExp {left, oper, right, pos}) break =
            raise TODO (* NB: many cases here! *)

          | trexp (A.CallExp {func, args, pos}) break =
            (case S.look (venv, func) of
                 SOME (E.FunEntry {formals, result, level=level', label}) =>
                 raise TODO (* using Tr.procCall2IR, Tr.funCall2IR *)
               | SOME (E.VarEntry {ty, ...}) =>
                 raise TODO (* error handling *)
               | NONE =>
                 raise TODO (* error handling *))

          | trexp (A.IfExp {test, thn, els, pos}) break =
            raise TODO (* using Tr.ifThen2IR, Tr.ifThenElse2IR *)

          | trexp (A.WhileExp {test, body, pos}) break =
            raise TODO (* using Tr.newBreakPoint, Tr.while2IR *)

          | trexp (aexp as A.RecordExp {fields, typ, pos}) break =
            raise TODO (* using Tr.record2IR, maybe Tr.nil2IR with errors *)

          | trexp (A.SeqExp []) _ =
            (* ensure there is some expression if the SeqExp is empty *)
            raise TODO

          | trexp (A.SeqExp (aexps as (aexp'::aexps'))) break =
            raise TODO (* using Tr.seq2IR, Tr.eseq2IR *)

          | trexp (A.AssignExp {var, exp, pos}) break =
            raise TODO (* using Tr.assign2IR, checkAssignable *)

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) _ =
            raise TODO (* using Tr.newBreakPoint, Tr.allocLocal, Tr.forIR *)

          | trexp (A.BreakExp pos) break =
            raise TODO (* using Tr.break2IR *)

          | trexp (term as A.LetExp {decls, body, pos}) break =
            raise TODO (* using transDecs, transExp, Tr.let2IR *)

          | trexp (A.ArrayExp {typ, size, init, pos}) break =
            raise TODO (* using Tr.array2IR *) *)

        (* NB: trvar must generate a tree describing the given
         * variable such that it will work for both evaluation and
         * assignment; any expression will be fine for evaluation,
         * but assignment only works with MOVE(TEMP _, _) and
         * MOVE(MEM _, _) because we can only store the new value
         * into a register or into a memory cell (error cases can
         * generate a NoOp value (Ex (CONST 0)), so they avoid
         * the problem).  This means that Tr.simpleVar, Tr.fieldVar, 
         * and Tr.subscript2IR must return an Ex (MEM _) or an 
         * Ex (TEMP _).
         *)
        (*(acc, fromLevel)*)
        and trvar {var=TAbs.SimpleVar id, ty} : {exp:Tr.exp,ty:Ty.ty} = (* using Tr.simpleVar *) 
            let
              val level' = (#level extra)
            in
              case S.look(venv,id) of
                SOME(E.VarEntry{access=access, ty=ty, escape=escape})=>{exp=Tr.simpleVar(access,level'), ty=ty}
            end

          | trvar {var=TAbs.FieldVar (var, id), ty} : {exp:Tr.exp,ty:Ty.ty} = 
          let 
            val {exp=var', ty=varty'} = trvar var
            val lst = (case actualTy varty' of
                          Ty.RECORD(fi,_) => fi
                          | _ => [](* Semant says this never happens*))
            fun index ((s,_)::xs, acc) = if s = id then acc else index(xs, acc + 1)
              | index (_) = ~1 (* Semans says this never happens *)
            val offset = index(lst,0)
          in
             {exp = Tr.fieldVar(var', offset), ty = ty}
          end 

            (*TODO*)
            (* ignore 'mutationRequested': all record fields are mutable *)
            (*TODO*) (* using Tr.fieldVar *)
                                       
          | trvar {var=TAbs.SubscriptVar (var, exp), ty} : {exp:Tr.exp,ty:Ty.ty} = 
          let 
            val {exp=var', ty=_} = trvar var
            val {exp=exp', ty=_} = trexp exp
          in
            {exp=Tr.subscript2IR(var',exp'), ty=ty}  
          end
            (* ignore 'mutationRequested': all array entries are mutable *)
            (*TODO*) (* using Tr.subscript2IR *)

    in
        trexp
    end
        
and transDec ( venv
             , TAbs.VarDec {name,escape=ref esc,ty,init}
             , explist (* accumulate decl elaboration code *)
             , extra) =
    let 
        val {exp=initExp,ty=initTy} = transExp (venv,extra) init
        val acc = Tr.allocLocal (#level extra) esc
        val var = Tr.simpleVar (acc, #level extra)
    in
        ( { venv = S.enter(venv,name,(E.VarEntry { access=acc
                                                 , ty=actualTy ty
                                                 , escape=ref esc})) }
        , explist @ (Tr.assign2IR(var, initExp) :: []))
    end
                
  | transDec (venv, TAbs.TypeDec tydecls, explist, extra) = 
    (* This will actually do nothing, since we do not need typing information *)
    ({ venv = venv}, explist)
        
  | transDec (venv, TAbs.FunctionDec fundecls, explist, extra) =
    let val headers = map (transFunHeader extra) fundecls
      val venv' = enterHeaders(venv,headers,fundecls)
      val _ = map (transFunction extra venv') fundecls
    in
      ( {venv = venv'}, explist ) (* TODO *)
    end

  and transFunction extra venv {name = name, params = params, 
    resultTy = resultTy, body = body} = 
    let 
      val level = (case (S.look (venv, name)) of
                  SOME(E.FunEntry({formals,result,label,level}))
                    => level
                  | _=> raise Bug "Function name not entered into enviorment")
      val access = Tr.formals level

      fun enterFormal (venv, {name,escape,ty}::xs, acc::ys) =
        enterFormal (S.enter(venv,name,
            (E.VarEntry {access = acc, ty = actualTy ty, escape = escape})), xs, ys)
        | enterFormal (venv',[],[]) = venv'
        | enterFormal (venv,[],_) = venv(* raise Bug "Formal list length les than access list" *)

      fun args (sl::xs) = xs (* First access is static link*)

      val venv' = enterFormal(venv,params,args(access))
      val {exp, ty} = transExp(venv',setLevel extra level) body
    in
      case actualTy resultTy of
        Ty.UNIT => Tr.procEntryExit {level = level, body = exp}
        | _ => Tr.funEntryExit{level = level, body = exp}
    end

    (* Since we can have recursive functions, we need give each funtion its level first before
      we can process the body*)
  and transFunHeader extra {name = name, params = params, 
    resultTy = resultTy, body = body} = 
      let 
        val formals = map (fn x => true) params (* Everything escapes, currently *)
        val fLabel = Temp.newLabel (S.name name)
        val parent = (getLevel extra)
        val level = Tr.newLevel {parent = parent, name = fLabel, formals = formals}
      in
        (level,fLabel)
      end

  and enterHeaders(venv,[],[]) = venv
    | enterHeaders(venv,(level,label)::headers, {name,params,resultTy,body}::fDecls) =
        let
            fun paramType { name = _, escape = _
                  , ty = ty} = ty
            val formals = map paramType params
            val venv' = S.enter(venv,name,(E.FunEntry {
                            formals = formals
                            , result = resultTy
                            , label = label
                            , level = level
                }))
        in
            enterHeaders(venv', headers, fDecls)
        end
    | enterHeaders(_) = raise Bug "The number of headers doesnt match the number of function declarations"




and transDecs (venv, decls, extra, explist) =
  case decls of
    [] => ({venv=venv}, explist)
    | (h::t) =>   let 
                    val ({venv=venv'}, explist') = transDec(venv,h,explist,extra)
                  in
                    transDecs(venv',t,extra,explist')
                  end
        (*({venv = venv}, [])*) (* TODO *)                

fun transProg absyn : Tr.frag list  =
    let
        val {exp=exp,ty} = transExp ( E.baseVenv
                                    , { break=NONE
                                      , level=E.initLevel}) absyn
    in
        if ty=Ty.UNIT
        then Tr.procEntryExit {level = E.initLevel, body = exp }
        else Tr.funEntryExit {level = E.initLevel, body = exp};
        Tr.getResult ()
    end
        
end (* IRgen *)
