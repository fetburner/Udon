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
  and abs =
    (* fn k x => v *)
      ABS of Id.t * Id.t * value
    (* fn x => v *)
    | ABS_TAIL of Id.t * value
  and cont =
      (* k *)
      CVAR of Id.t
      (* fn x => e *)
    | CABS of Id.t * exp
end
