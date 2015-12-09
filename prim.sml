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

  fun priority (PLUS | MINUS) = (6, Infixer.Assoc.LEFT)
    | priority TIMES = (7, Infixer.Assoc.LEFT)
    | priority LE = (4, Infixer.Assoc.LEFT)

  fun typeOf (PLUS | MINUS | TIMES) =
        Type.FUN ([Type.INT, Type.INT], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.INT, Type.INT], Type.BOOL)

  val primitives =
    map (fn p => (toString p, p))
    [ PLUS,
      MINUS,
      TIMES,
      LE ]
end
