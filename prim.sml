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

  fun dom (PLUS | MINUS | TIMES | LE) = [Type.INT, Type.INT]

  fun cod (PLUS | MINUS | TIMES) = Type.INT
    | cod LE = Type.BOOL

  val primitives =
    map (fn p => (toString p, p))
    [ PLUS,
      MINUS,
      TIMES,
      LE ]
end
