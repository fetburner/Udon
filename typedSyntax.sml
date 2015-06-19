structure TypedSyntax = struct
  (* typed value identifier *)
  type id = Id.t * Type.t

  fun idToString (x, t) = Id.toString x ^ " : " ^ Type.toString t
  val idSeqToString = PP.seqToString (idToString, "()", ", ", "(", ")")
  (* return type of typed value identifier *)
  val idTypeOf : id -> Type.t = #2
  (* return types of typed value identifiers *)
  val idSeqTypeOf = map idTypeOf

  (* e : T *)
  datatype exp = E of exp_body * Type.t
  and exp_body =
    (* constant *)
      CONST of Const.t
    (* variable *)
    | VAR of Id.t
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn (x_1 : T_1, ... , x_n : T_n) => M *)
    | ABS of id list * exp
    (* M (N_1, ... , N_n) *)
    | APP of exp * exp list
    (* let d in N end *)
    | LET of dec list * exp
    (* op (+) (M_1, ... , M_n) *)
    | PRIM of Prim.t * exp list
    (* (M_1, ... , M_n) *)
    | TUPLE of exp list
    (* case M of (x_1, ... , x_n) => N *)
    | CASE of exp * id list * exp
  and dec =
    (* val x : T = M *)
      VAL of id * exp
    (* val rec f : T_1 = fn (x_1 : T_21, ... , x_2n : T_n) => M *)
    | VALREC of id * id list * exp

  fun expToString (E (e, t)) =
    "(" ^ expBodyToString e ^ " : " ^ Type.toString t ^ ")"
  and expBodyToString (CONST c) = Const.toString c
    | expBodyToString (VAR x) = Id.toString x
    | expBodyToString (IF (m, n1, n2)) =
        "(if "
        ^ expToString m
        ^ " then "
        ^ expToString n1
        ^ " else "
        ^ expToString n2
        ^ ")"
    | expBodyToString (ABS (xs, m)) =
        "(fn "
        ^ idSeqToString xs
        ^ " => " 
        ^ expToString m
        ^ ")"
    | expBodyToString (APP (m, ns)) =
        "("
        ^ expToString m
        ^ " "
        ^ expSeqToString ns
        ^ ")"
    | expBodyToString (LET (d, m)) =
        "let "
        ^ decToString d
        ^ " in "
        ^ expToString m
        ^ " end"
    | expBodyToString (PRIM (p, ms)) =
        "(op"
        ^ Prim.toString p
        ^ " "
        ^ expSeqToString ms
        ^ ")"
    | expBodyToString (TUPLE ms) =
        expSeqToString ms
    | expBodyToString (CASE (m, xs, n)) =
        "(case "
        ^ expToString m
        ^ " of "
        ^ idSeqToString xs
        ^ " => "
        ^ expToString n
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
      "val "
      ^ idToString x
      ^ " = "
      ^ expToString m
    | VALREC (f, xs, m) =>
      "val rec "
      ^ idToString f
      ^ " = fn "
      ^ idSeqToString xs
      ^ " => "
      ^ expToString m, "", "; ", "", "") dec

  (* return type of typed expression *)
  fun expTypeOf (E (_, t)) = t
  (* return types of typed expressions *)
  val expSeqTypeOf = map expTypeOf
end
