structure Type = struct
  datatype t =
      VAR of t option ref
    | INT
    | BOOL
    | FUN of t list * t
    | TUPLE of t list

  fun toString (VAR (ref (SOME t))) = toString t
    | toString (VAR (ref NONE)) = "'a"
    | toString INT = "int"
    | toString BOOL = "bool"
    | toString (FUN (ts, t)) = "(" ^ seqToString ts ^ " -> " ^ toString t ^ ")"
    | toString (TUPLE ts) = "(" ^ seqToString ts ^ ")"
  and seqToString ts = PP.seqToString (toString, "unit", " * ", "", "") ts

  fun genvar () = VAR (ref NONE)
end

