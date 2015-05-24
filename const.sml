structure Const = struct
  (* constant expressions *)
  datatype t = INT of int | BOOL of bool

  fun toString (INT n) = Int.toString n
    | toString (BOOL b) = Bool.toString b
end
