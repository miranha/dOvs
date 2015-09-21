structure Examples =
struct

(* simple example values *)
val e1 = Ast.Id "x"
val e2 = Ast.Op (Ast.Plus, (Ast.Id "x"), Ast.Number 2) 

(* aux function to produce a tree for our datatype *)
val showTree = DrawTrees.showTreePDF "/tmp/aexp" (DrawAst.dotCmd)

val parseAndShow = showTree o Frontend.parse

end           
