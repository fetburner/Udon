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
    (* let val x = M in N end *)
    | LET_VAL of Id.t * exp * exp
    (* let val rec f = fn (x_1, ... , x_n) => M in N *)
    | LET_VALREC of Id.t * Id.t list * exp * exp
    (* op (+) (M_1, ... , M_n) *)
    | PRIM of Prim.t * exp list

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
    | expToString (LET_VAL (x, m, n)) =
        "let val "
        ^ Id.toString x
        ^ " = "
        ^ expToString m
        ^ " in "
        ^ expToString n
        ^ " end"
    | expToString (LET_VALREC (f, xs, m, n)) =
        "let val rec "
        ^ f
        ^ " = fn "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString m
        ^ " in "
        ^ expToString n
        ^ " end"
    | expToString (PRIM (p, ms)) =
        "(op"
        ^ Prim.toString p
        ^ " "
        ^ expSeqToString ms
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq

end
