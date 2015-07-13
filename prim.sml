structure Prim = struct
  (* primitives *)
  datatype t =
      PLUS
    | MINUS
    | TIMES
    | LE
    (* (v_1, ... , v_n) *)
    | TUPLE
    (* #n x *)
    | TUPLE_GET of int

  (* pretty-printer *)
  fun toString PLUS = "+"
    | toString MINUS = "-"
    | toString TIMES = "*"
    | toString LE = "<="
    | toString TUPLE = "make_tuple"
    | toString (TUPLE_GET n) = "#" ^ Int.toString n

  fun priority (PLUS | MINUS) = SOME (6, Infixer.Assoc.LEFT)
    | priority TIMES = SOME (7, Infixer.Assoc.LEFT)
    | priority LE = SOME (4, Infixer.Assoc.LEFT)
    | priority TUPLE = NONE
    | priority (TUPLE_GET n) = NONE

  fun typeOf (PLUS | MINUS | TIMES) =
        Type.FUN ([Type.INT, Type.INT], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.INT, Type.INT], Type.BOOL)

  val primitives =
    map (fn p => (Id.gensym (toString p), p))
    [ PLUS,
      MINUS,
      TIMES,
      LE ]
end
