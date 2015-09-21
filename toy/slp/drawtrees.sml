(* 

drawtrees.sml

An SML module for generating dot files to display tree datatypes. Intended
usage: generation of trees to use in lectures in Compilation 2014 course at
Aarhus University.

Author: Aslan Askarov, aslan@cs.au.dk
License: CRAPL

CHANGELOG:

- September 10, 2014. Initial version as a module, based on a previous non-modular code

*)

structure DrawTrees:  
          sig
              type node
              type tree
              val new_node             : string -> node
                      
              val singletonTree        : string -> tree

              (* this should really be renamed to something like oneParentOneChildTree *)
              val simpleTree           : string -> string -> tree

              val connectOneChild      : string -> string -> tree
                                                                                                                                  
              (* this is the powerhorse function *)
              val connectToXChildren   : node -> tree list -> tree

              (* synonym to X children *)
              val connectTwoChildren   : node -> tree -> tree -> tree

              (* another synonym to X children *)
              val connectThreeChildren : node -> tree -> tree -> tree -> tree

              (* writes dot file to a file *)
              val writeTree :  string -> tree -> unit
                                       
              (* writes a dot file; generates the PDF, and displays it to the user *)
              val showTreePDF : string -> ('a -> tree) -> 'a -> OS.Process.status
          end
=
struct

type node = (int * string)
type tree = (int * string list)



val node_counter = ref 0

fun new_node str =
    let
        val n = !node_counter;
        val N = Int.toString (n);
    in
        node_counter := !node_counter + 1;
        (n, "node" ^ N ^ " [label=\"" ^ str ^ "\"]")
    end

fun nodify N = "node"^ Int.toString (N)
                                    
val separator = ";"
val connector = "->"
val newline_t = "\n\t"

fun connect a b  = (nodify a) ^ connector ^ (nodify b)

fun singletonTree a = 
    let val (n1,s1) = new_node a
    in (n1, [s1])
    end

fun simpleConnect a b =
    let val (n1, s1) = new_node a
        val (n2, s2) = new_node b
        val s3 = connect n1 n2
     in (n1, [s1,s2,s3])
    end

val simpleTree = simpleConnect

val connectOneChild = simpleConnect

fun connectToXChildren' (n0, s0) (children: (int * string list) list ) = 
    let 
        val children_decs   = foldl (op @) [] (map #2 children)

        val connect_strings = map ((fn n_i => connect n0 n_i) o #1) children
        val children_connectors = foldr (op ::) [] connect_strings
    in 
        (n0, [s0] @ children_decs @ children_connectors)
    end 

fun connectToXChildren p ls = connectToXChildren' p (List.rev ls)

fun connectTwoChildren   p a b = connectToXChildren p [a,b]

fun connectThreeChildren p a b c = connectToXChildren p [a,b,c]    

fun graphIt (_, ss) = 
    let 
        val preamble = "digraph G { graph [fontname = \"Helvetica\"]; node  [fontname = \"Helvetica\"];edge  [fontname = \"Helvetica\"];\n"
        val last_line = "}\n"
        val ss_semi_formatted = map (fn s => "\t" ^ s ^ ";\n") ss 
    in 
        String.concat ([preamble]@ss_semi_formatted@[last_line])
    end

fun writeTree filename tree = 
    let
        val outs = TextIO.openOut filename
        val str  = graphIt tree                           
    in
        TextIO.output(outs, str);
        TextIO.closeOut outs
    end


(* we keep a counter to produce unique dot files and the corresponding pdfs      *)

(* a better implementation would use a combination of a time stamp and a counter *)

val tree_counter = ref 0

(* note that open command is assuming OSX here *)

fun showTreePDF file_template f e = 
    let val template' = file_template ^ "-" ^ (Int.toString (!tree_counter))

        val new_fn      = template' ^ ".dot"
        val new_pdf_out = template' ^ ".pdf" 

        val dot_cmd = "dot -Tpdf  " ^ new_fn ^ " -o " ^ new_pdf_out ^ "; open " ^ new_pdf_out

        val mkT = (writeTree new_fn o f)
    in
        tree_counter := !tree_counter + 1;
        mkT e;
        OS.Process.system(dot_cmd)
    end
        
end
