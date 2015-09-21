structure Ast =
struct

type pos = int

type id = string
datatype aexp = Id of id
         | Number of int
         | Op of binop * aexp * aexp 

and binop = Plus | Minus | Times | Div

and cmd = Assign of id * aexp
        | If of aexp * cmd * cmd option
        | Cmds of cmd list
end

