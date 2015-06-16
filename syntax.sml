structure Syntax = struct

  (* abstract syntax tree of expression *)
  datatype exp =
    (* constant *)
      CONST of Const.t
    (* variable *)
    | VAR of Id.t
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn (x_1, ... , x_n) => M *)
    | ABS of Id.t list * exp
    (* M (N_1, ... , N_n) *)
    | APP of exp * exp list
    (* let d in N end *)
    | LET of dec list * exp
    (* op (+) (M_1, ... , M_n) *)
    | PRIM of Prim.t * exp list
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of Id.t * exp
    (* val rec f = fn (x_1, ... , x_n) => M *)
    | VALREC of Id.t * Id.t list * exp

  (* pretty-printer *)
  (* as you can see, this implementation is conservative *)
  fun expToString (CONST c) = Const.toString c
    | expToString (VAR x) = Id.toString x
    | expToString (IF (m, n1, n2)) =
      "(if "
      ^ expToString m
      ^ " then "
      ^ expToString n1
      ^ " else "
      ^ expToString n2
      ^ ")"
    | expToString (ABS (xs, m)) =
      "(fn "
      ^ Id.seqToString xs
      ^ " => "
      ^ expToString m
      ^ ")"
    | expToString (APP (m, ns)) =
      "("
      ^ expToString m
      ^ " "
      ^ expSeqToString ns
      ^ ")"
    | expToString (LET (d, m)) =
      "let "
      ^ decToString d
      ^ " in "
      ^ expToString m
      ^ " end"
    | expToString (PRIM (p, ms)) =
      "(op"
      ^ Prim.toString p
      ^ " "
      ^ expSeqToString ms
      ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
      "val "
      ^ Id.toString x
      ^ " = "
      ^ expToString m
    | VALREC (f, xs, m) =>
      "val rec "
      ^ Id.toString f
      ^ " = fn "
      ^ Id.seqToString xs
      ^ " => "
      ^ expToString m, "", "; ", "", "") dec
end
