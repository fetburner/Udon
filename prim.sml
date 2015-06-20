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
        Type.FUN ([Type.TUPLE [Type.INT, Type.INT]], Type.INT)
    | typeOf LE =
        Type.FUN ([Type.TUPLE [Type.INT, Type.INT]], Type.BOOL)

  local
    val primitives =
      map (fn p => (Id.gensym (toString p), p))
      [PLUS,
      MINUS,
      TIMES,
      LE]
  in
    val infixInfoBindings =
      map (fn (id, p) => (id, priority p)) primitives

    val typeInfoBindings =
      map (fn (id, p) => (id, typeOf p)) primitives
  end
end
