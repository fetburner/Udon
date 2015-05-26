(* utilities for pretty-printing *)
structure PP = struct
  (* auxiliary function for pretty-printing sequence *)
  (* e.g. seqToString (Int.toString, "()", ", ", "(", ")") [1, 1, 4, 5, 1, 4] *)
  fun seqToString (toString, empty, separator, lparen, rparen) = fn
      [] => empty
    | [x] => toString x
    | x :: xs =>
        lparen
        ^ List.foldl (fn (x, s) =>
            s ^ separator ^ toString x) (toString x) xs
        ^ rparen
end
