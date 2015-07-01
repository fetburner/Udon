structure Cps = struct
  (* v *)
  datatype value =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
  (* t *)
  and term
    (* v *)
      VAL of value
    (* fn (x_1, ... x_n) k => e *)
    | ABS of Id.t list * Id.t * exp
    (* ( + ) (v_1, ... , v_n) *)
    | PRIM of Prim.t * value list
  (* e *)
  and exp =
    (* x (v_1, ... , v_n) C *)
      APP of Id.t * value list * cont
    (* k v *)
    | APP_TAIL of Id.t * value
    (* let val x = t in e end *)
    | LET of (Id.t * term) * exp
    (* let val rec f = t in e end *)
    | LET_REC of (Id.t * term) * exp
    (* let k = cont in exp *)
    | LET_CONT of (Id.t * cont) * exp
    (* if v then e1 else e2 *)
    | IF of value * exp * exp
  (* C *)
  and cont =
      (* k *)
      CVAR of Id.t
      (* fn x => e *)
    | CABS of Id.t * exp

  fun valueToString (CONST c) = Const.toString c
    | valueToString (VAR id) = Id.toString id

  fun vsToString seq = PP.seqToString (valueToString, "()", ", ", "(", ")") seq

  fun expToString (APP (v, vs, cont)) =
    PP.seqToString (valueToString, "", " ", "", "") (v :: vs) ^ " " ^ contToString cont
    | expToString (APP_TAIL (c, v)) =
      contToString c ^ " " ^ valueToString v
    | expToString (LET ((id, abs), exp)) =
      "LET " ^ Id.toString id ^ " = " ^ absToString abs ^ " in " ^ expToString exp
    | expToString (LET_REC ((id, abs), exp)) =
      "LET* " ^ Id.toString id ^ " = " ^ absToString abs ^ " in " ^ expToString exp
    | expToString (IF (v, e1, e2)) =
      ("IF " ^ valueToString v ^ " then " ^ expToString e1 ^ " else " ^ expToString e2)
  and absToString (ABS (id, ids, exp)) =
      "ABS" ^ PP.seqToString (Id.toString, "()", ",", "(", ")") (id :: ids) ^ " => " ^ expToString exp
    (* | absToString (ABS_TAIL (id, exp)) = *)
    (*   "(ABS* " ^ Id.toString id ^ " => " ^ expToString exp ^ ")" *)
    | absToString (TUPLE vs) = vsToString vs
    | absToString (GET (v, i)) = "#" ^ Int.toString i ^ " " ^ valueToString v
  and contToString (CVAR id) = "*" ^ Id.toString id
    | contToString (CABS (id, exp)) = "(CABS " ^ Id.toString id ^ " => " ^ expToString exp ^ ")"

end
