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
end
