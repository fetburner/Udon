structure TypedSyntax = struct
  (* typed value identifier *)
  type id = Id.t * Type.t

  fun idToString (x, t) = Id.toString x ^ " : " ^ Type.toString t
  val idSeqToString = PP.seqToString (idToString, "()", ", ", "(", ")")
  (* return type of typed value identifier *)
  val idTypeOf : id -> Type.t = #2
  (* return types of typed value identifiers *)
  val idSeqTypeOf = map idTypeOf

  (* polymorphic typed value identifier *)
  type polyId = Id.t * Type.scheme

  fun polyIdToString (x, t) = Id.toString x ^ " : " ^ Type.schemeToString t
  (* return type scheme of polymorphic typed value identifier *)
  val polyIdTypeOf : polyId -> Type.scheme = #2

  fun idToPolyId (x, t) = (x, Type.toTypeScheme t)

  datatype exp_body =
    (* constant *)
      CONST of Const.t
    (* x [T_1] ... [T_n] *)
    | VAR of Id.t * Type.t list
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn (x_1 : T_1, ... , x_n : T_n) => M *)
    | ABS of id list * exp
    (* M (N_1, ... , N_n) *)
    | APP of exp * exp list
    (* let d in N end *)
    | LET of dec list * exp
    (* (M_1, ... , M_n) *)
    | TUPLE of exp list
    (* case M of (x_1, ... , x_n) => N *)
    | CASE of exp * id list * exp
    (* op (+) (M_1, ..., M_n) *)
    | PRIM of Prim.t * exp list
  and dec =
    (* val x : /\X_1 ... X_n. T = M *)
      VAL of polyId * exp
    (* val rec f : /\X_1 ... X_n. T = M *)
    | VALREC of polyId * exp
  (* e : T *)
  withtype exp = exp_body * Type.t

  fun expToString (e, t) =
    "(" ^ expBodyToString e ^ " : " ^ Type.toString t ^ ")"
  and expBodyToString (CONST c) = Const.toString c
    | expBodyToString (VAR (x, ts)) =
        "("
        ^ Id.toString x
        ^ PP.seqToString (fn t =>
            " [" ^ Type.toString t ^ "]", "", "", "", "") ts
        ^ ")"
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
    | expBodyToString (PRIM (p, ms)) =
        "(op "
        ^ Prim.toString p
        ^ " "
        ^ expSeqToString ms
        ^ ")"
  and expSeqToString seq = PP.seqToString (expToString, "()", ", ", "(", ")") seq
  and decToString dec = PP.seqToString (fn
      VAL (x, m) =>
        "val "
        ^ polyIdToString x
        ^ " = "
        ^ expToString m
    | VALREC (f, m) =>
        "val rec "
        ^ polyIdToString f
        ^ " = "
        ^ expToString m, "", "; ", "", "") dec

  (* return type of typed expression *)
  val expTypeOf : exp -> Type.t = #2
  (* return types of typed expressions *)
  val expSeqTypeOf = map expTypeOf
end
