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
    (* let val x : T = M in N end *)
    | LET_VAL of id * exp * exp
    (* let val rec f : T_1 = fn (x_1 : T_21, ... , x_2n : T_n) => M in N *)
    | LET_VALREC of id * id list * exp * exp
    (* op (+) (M_1, ... , M_n) *)
    | PRIM of Prim.t * exp list

  fun expToString (E (e, t)) = expBodyToString e ^ " : " ^ Type.toString t
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
    | expBodyToString (LET_VAL (x, m, n)) =
        "let val "
        ^ idToString x
        ^ " = "
        ^ expToString m
        ^ " in "
        ^ expToString n
        ^ " end"
    | expBodyToString (LET_VALREC (f, xs, m, n)) =
        "let val rec "
        ^ idToString f
        ^ " = fn "
        ^ idSeqToString xs
        ^ " => "
        ^ expToString m
        ^ " in "
        ^ expToString n
        ^ " end"
    | expBodyToString (PRIM (p, ms)) =
        "(op"
        ^ Prim.toString p
        ^ " "
        ^ expSeqToString ms
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq

  (* return type of typed expression *)
  fun expTypeOf (E (_, t)) = t
  (* return types of typed expressions *)
  val expSeqTypeOf = map expTypeOf
end
