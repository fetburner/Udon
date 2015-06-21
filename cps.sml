structure Cps = struct
  datatype value =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
  and exp =
    (* v1 v2 k *)
      APP of value * value * cont
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
      ABS of Id.t * Id.t * exp
    (* fn x => v *)
    | ABS_TAIL of Id.t * exp
    (* (v, v, v, ...) *)
    | TUPLE of value list
    (* #n v *)                     
    | GET of value * int (* abs, but tuple only *)
  and cont =
      (* k *)
      CVAR of Id.t
      (* fn x => e *)
    | CABS of Id.t * exp
    | HALT

  fun valueToString (CONST c) = Const.toString c
    | valueToString (VAR id) = Id.toString id

  local 
    fun vsToString' [v] = valueToString v
      | vsToString' (v :: vs) = valueToString v ^ ", " ^ vsToString' vs 
  in
  fun vsToString ids =
    "(" ^ vsToString' ids ^ ")"
  end
  
  fun expToString (APP (v1, v2, cont)) =
    "(" ^ valueToString v1 ^ " " ^ valueToString v2 ^ " " ^ contToString cont ^ ")"
    | expToString (APP_TAIL (c, v)) =
      "*(" ^ contToString c ^ " " ^ valueToString v ^ ")"
    | expToString (LET ((id, abs), exp)) =
      "LET " ^ Id.toString id ^ " = " ^ absToString abs ^ " in " ^ expToString exp
    | expToString (LET_REC ((id, abs), exp)) =
      "LET* " ^ Id.toString id ^ " = " ^ absToString abs ^ " in " ^ expToString exp
    | expToString (IF (v, e1, e2)) =
      ("IF " ^ valueToString v ^ " then " ^ expToString e1 ^ " else " ^ expToString e2)
  and absToString (ABS (id1, id2, exp)) =
      "(ABS (" ^ Id.toString id1 ^ ", " ^ Id.toString id2 ^ ") => " ^ expToString exp ^ ")"
    | absToString (ABS_TAIL (id, exp)) =
      "(ABS* " ^ Id.toString id ^ " => " ^ expToString exp ^ ")"
    | absToString (TUPLE vs) = vsToString vs
    | absToString (GET (v, i)) = "#" ^ Int.toString i ^ " " ^ valueToString v
  and contToString (CVAR id) = "*" ^ Id.toString id
    | contToString (CABS (id, exp)) = "(CABS " ^ Id.toString id ^ " => " ^ expToString exp ^ ")"
    | contToString HALT = "HALT"

end
