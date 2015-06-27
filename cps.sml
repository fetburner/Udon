structure Cps = struct
  datatype value =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
  and exp =
    (* v k v* *)
      APP of value * value list * cont
    (* k v *)
    | APP_TAIL of cont * value
    (* let val f = abs in e end *)
    | LET of (Id.t * abs) * exp
    (* let val rec f = abs in e end *)
    | LET_REC of (Id.t * abs) * exp
    (* if v then e1 else e2 *)
    | IF of value * exp * exp
  and abs = (* XXX : should change the name *)
    (* fn k x => v *)
      ABS of Id.t * Id.t list * exp
    (* (* fn x => v *) *)
    (* | ABS_TAIL of Id.t * exp *)
    (* (v, v, v, ...) *)
    | TUPLE of value list
    (* #n v *)
    | GET of value * int (* value, but tuple only *)
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
