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

  fun priority (PLUS | MINUS) = SOME (6, Assoc.LEFT_ASSOC)
    | priority TIMES = SOME (7, Assoc.LEFT_ASSOC)
    | priority LE = SOME (4, Assoc.LEFT_ASSOC)
    | priority TUPLE = NONE
    | priority (TUPLE_GET n) = NONE

  fun typeOf (PLUS | MINUS | TIMES) =
        Type.FUN ([Type.TUPLE [Type.INT, Type.INT]], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.TUPLE [Type.INT, Type.INT]], Type.BOOL)

  val primitives =
    map (fn p => (Id.gensym (toString p), p))
    [PLUS,
    MINUS,
    TIMES,
    LE]

  val infixInfoBindings =
    map (fn (id, p) => (id, priority p)) primitives

  val typeInfoBindings =
    map (fn (id, p) => (id, typeOf p)) primitives
end
