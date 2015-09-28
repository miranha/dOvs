structure PrintTypes: PRINTTYPES =
struct

structure T = Types

val nameUnfoldMax = 10

fun asStringI unfolds seen (T.RECORD (fields, uniq)) =
    let fun fieldString (s, ty) =
            (Symbol.name s) ^ ": " ^
            (asStringI (unfolds + 1) seen ty)
        fun concatFields (s, (r,fieldsCount)) =
            if fieldsCount > 0
            then (r ^ ", " ^ (fieldString s), fieldsCount + 1)
            else (r ^ (fieldString s), fieldsCount + 1)
        val (fieldsString,unfolds) = List.foldl concatFields ("",0) fields
    in  "RECORD {" ^ fieldsString ^ "}"
    end
  | asStringI unfolds seen T.NIL = "NIL"
  | asStringI unfolds seen T.INT = "INT"
  | asStringI unfolds seen T.STRING = "STRING"
  | asStringI unfolds seen (T.ARRAY (ty1, uniq)) = 
    "ARRAY of " ^ (asStringI (unfolds + 1) seen ty1)
  | asStringI unfolds seen (T.NAME (s, tyor)) =
    let val hasBeenSeen = List.exists (fn s' => s' = s) seen in
        if hasBeenSeen
        then "NAME " ^ (Symbol.name s)
        else (if unfolds <= nameUnfoldMax then
                  let val tyAsStringI =
                          case !tyor of
                              SOME ty => asStringI (unfolds + 1) (s::seen) ty
                            | NONE => "(NONE)"
                  in  "NAME " ^ (Symbol.name s) ^ " = " ^ tyAsStringI
                  end
              else "NAME " ^ (Symbol.name s))
    end
  | asStringI unfolds seen T.UNIT = "UNIT"
  | asStringI unfolds seen T.ERROR = "ERROR"

fun asString ty = asStringI 0 [] ty

fun print (outstream, ty) =
    let fun say s =  TextIO.output (outstream, s)
        fun sayln s = (say s; say "\n")
    in 
        sayln (asString ty)
    end

end (* PrintTypes *)

