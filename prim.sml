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

  fun typeOf (PLUS | MINUS | TIMES) =
        Type.FUN ([Type.INT, Type.INT], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.INT, Type.INT], Type.INT)
end
