structure Syntax = struct

  (* abstract syntax tree of expression *)
  datatype exp =
    (* constant *)
      CONST of Const.t
    (* variable *)
    | VAR of string
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn x => M *)
    | ABS of string * exp
    (* M N *)
    | APP of exp * exp
    (* let d in N end *)
    | LET of dec list * exp
    (* (M_1, ... , M_n) *)
    | TUPLE of exp list
    (* case M of (x_1, ... , x_n) => N *)
    | CASE of exp * string list * exp
    (* op (+) (M_1, ..., M_n) *)
    | PRIM of Prim.t * exp list
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of string * exp
    (* val rec f = M *)
    | VALREC of string * exp

  (* pretty-printer *)
  (* as you can see, this implementation is conservative *)
  fun seqToString l = PP.seqToString (String.toString, "()", ", ", "(", ")") l
  fun expToString (CONST c) = Const.toString c
    | expToString (VAR x) = x
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
        ^ x
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
        ^ seqToString xs
        ^ " => "
        ^ expToString n
        ^ ")"
    | expToString (PRIM (p, ms)) =
        "(op "
        ^ Prim.toString p
        ^ " "
        ^ expSeqToString ms
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
        "val "
        ^ x
        ^ " = "
        ^ expToString m
    | VALREC (f, m) =>
        "val rec "
        ^ f
        ^ " = "
        ^ expToString m, "", "; ", "", "") dec
end
