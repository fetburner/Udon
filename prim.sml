structure Prim = struct
  (* primitives *)
  datatype t = 
      PLUS
    | MINUS
    | TIMES
    | LE

  (* pretty-printer *)
  fun toString PLUS = "+"
    | toString MINUS = "-"
    | toString TIMES = "*"
    | toString LE = "<="

  fun priority (PLUS | MINUS) = SOME (6, Assoc.LEFT_ASSOC)
    | priority TIMES = SOME (7, Assoc.LEFT_ASSOC)
    | priority LE = SOME (4, Assoc.LEFT_ASSOC)

  fun typeOf (PLUS | MINUS | TIMES) =
        Type.FUN ([Type.INT, Type.INT], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.INT, Type.INT], Type.BOOL)
end
