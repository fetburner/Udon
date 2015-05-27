structure Const = struct
  (* constant expressions *)
  datatype t = INT of int | BOOL of bool

  fun toString (INT n) = Int.toString n
    | toString (BOOL b) = Bool.toString b

  fun typeOf (INT _) = Type.INT
    | typeOf (BOOL _) = Type.BOOL
end
