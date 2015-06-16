structure Syntax = struct

  (* abstract syntax tree of expression *)
  datatype exp =
    (* constant *)
      CONST of Const.t
    (* variable *)
    | VAR of Id.t
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn x => M *)
    | ABS of Id.t * exp
    (* M N *)
    | APP of exp * exp
    (* let d in N end *)
    | LET of dec list * exp
    (* (M_1, ... , M_n) *)
    | TUPLE of exp list
    (* case M of (x_1, ... , x_n) => N *)
    | CASE of exp * Id.t list * exp
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of Id.t * exp
    (* val rec f = fn x => M *)
    | VALREC of Id.t * Id.t * exp

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
    | expToString (ABS (x, m)) =
      "(fn "
      ^ Id.toString x
      ^ " => "
      ^ expToString m
      ^ ")"
    | expToString (APP (m, n)) =
      "("
      ^ expToString m
      ^ " "
      ^ expToString n
      ^ ")"
    | expToString (LET (d, m)) =
      "let "
      ^ decToString d
      ^ " in "
      ^ expToString m
      ^ " end"
    | expToString (TUPLE ms) =
        expSeqToString ms
    | expToString (CASE (m, xs, n)) =
        "(case "
        ^ expToString m
        ^ " of "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString n
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
      "val "
      ^ Id.toString x
      ^ " = "
      ^ expToString m
    | VALREC (f, x, m) =>
      "val rec "
      ^ Id.toString f
      ^ " = fn "
      ^ Id.toString x
      ^ " => "
      ^ expToString m, "", "; ", "", "") dec
end
