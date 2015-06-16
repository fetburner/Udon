structure ConcreteSyntax = struct
  (* abstract syntax tree of expression *)
  datatype exp =
    (* constant *)
      CONST of Const.t
    (* variable *)
    | VAR of string
    (* non-infixed operator *)
    | OP of string
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn (x_1, ... , x_n) => M *)
    | ABS of string list * exp
    (* M (N_1, ... , N_n) *)
    | APP of exp * exp list
    (* let d in N end *)
    | LET of dec list * exp
    (* sequence of expression *)
    | SEQ of exp list
    (* ( M ) *)
    | PAREN of exp
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of string * exp
    (* val rec f = fn (x_1, ... , x_n) => M *)
    | VALREC of string * string list * exp
    (* infix d vid_1 ... vid_n *)
    (* infixr d vid_1 ... vid_n *)
    | INFIX of Assoc.assoc * int * string list
    (* nonfix vid_1 ... vid_n *)
    | NONFIX of string list

  (* pretty-printer *)
  (* as you can see, this implementation is conservative *)
  local
    fun seqToString l = PP.seqToString (String.toString, "()", ", ", "(", ")") l
    fun vidSeqToString l = PP.seqToString (String.toString, "", " ", "", "") l
  in
  fun expToString (CONST c) = Const.toString c
    | expToString (VAR x) = x
    | expToString (OP x) = "op " ^ x
    | expToString (IF (m, n1, n2)) =
      "if "
      ^ expToString m
      ^ " then "
      ^ expToString n1
      ^ " else "
      ^ expToString n2
      ^ ""
    | expToString (ABS (xs, m)) =
      "fn "
      ^ seqToString xs
      ^ " => "
      ^ expToString m
    | expToString (APP (m, ns)) =
      expToString m
      ^ " "
      ^ expSeqToString ns
    | expToString (LET (d, m)) =
      "let "
      ^ decToString d
      ^ " in "
      ^ expToString m
      ^ " end"
    | expToString (SEQ ms) =
      PP.seqToString (expToString, "", " ", "", "") ms
    | expToString (PAREN m) =
      "("
      ^ expToString m
      ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
      "val "
      ^ x
      ^ " = "
      ^ expToString m
    | VALREC (f, xs, m) =>
      "val rec "
      ^ f
      ^ " = fn "
      ^ seqToString xs
      ^ " => "
      ^ expToString m
    | INFIX (Assoc.LEFT_ASSOC, d, vids) =>
        "infixl "
        ^ Int.toString d
        ^ vidSeqToString vids
    | INFIX (Assoc.RIGHT_ASSOC, d, vids) =>
        "infixr "
        ^ Int.toString d
        ^ vidSeqToString vids
    | NONFIX vids =>
        "nonfix "
        ^ vidSeqToString vids, "", "; ", "", "") dec
  end
end
