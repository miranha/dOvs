signature ENV =
sig

  type access

  datatype enventry = VarEntry of {access: Translate.access,ty: Types.ty}
                    | FunEntry of {(*level: Translate.level, label: Temp.label,*)formals: Types.ty list, result: Types.ty}

  val baseTenv: Types.ty Symbol.table
  val baseVenv: enventry Symbol.table

end
