structure DrawAst = 
struct

structure A = Ast
structure T = DrawTrees
               
fun strBinop A.Plus  = "+"
  | strBinop A.Minus = "-"
  | strBinop A.Times = "*"
  | strBinop A.Div   = "/"
   
fun dotAexp (A.Id s) = T.singletonTree s
  | dotAexp (A.Number i) = T.singletonTree (Int.toString i)
  | dotAexp (A.Op (binop, e1, e2)) = 
    T.connectTwoChildren (T.new_node(strBinop binop)) (dotAexp e1) (dotAexp e2)

fun dotCmd (A.Assign (s, e)) =
    T.connectTwoChildren (T.new_node(":=")) (T.singletonTree s) (dotAexp e)

  | dotCmd (A.If (e, c1, SOME c2)) =
    T.connectThreeChildren (T.new_node "if then else") (dotAexp e) (dotCmd c1) (dotCmd c2)

  | dotCmd (A.If (e, c1, NONE )) =
    T.connectTwoChildren (T.new_node "if then") (dotAexp e) (dotCmd c1) 

  | dotCmd (A.Cmds cmds) =
    T.connectToXChildren (T.new_node "cmds") (map dotCmd cmds)

  

end
