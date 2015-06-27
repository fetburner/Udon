structure Js = struct

  datatype exp =
      INT  of int
    | BOOL of bool
    | VAR  of Id.t
    | LIST of exp list
    | ABS  of Id.t list * prog
    | APP  of exp * exp list
    | GET  of exp * int

  and prog =
      VARDECL of Id.t * exp * prog
    | IF      of exp * prog * prog
    | RET     of exp
    | HALT

  val result = {s = "result", id = 0}

  fun translValue (Cps.CONST (Const.INT i)) = INT i
    | translValue (Cps.CONST (Const.BOOL b)) = BOOL b
    | translValue (Cps.VAR id) = VAR id

  fun values vs = List.map translValue vs

  fun translAbs (Cps.ABS (id, ids, cps)) = ABS (ids, transl cps)
    | translAbs (Cps.TUPLE vs) = LIST (values vs)
    | translAbs (Cps.GET (v, ind)) = GET (translValue v, ind - 1)

  and transl cps =
    case cps of
       Cps.APP (v, vs, Cps.CVAR k) =>
       if #s k = "HALT"
       then VARDECL (result, APP (translValue v, values vs), HALT)
       else RET (APP (translValue v, values vs))
     | Cps.APP (v, vs, Cps.CABS (id, cps)) => VARDECL (id, APP (translValue v, values vs), transl cps)
     | Cps.APP_TAIL (Cps.CVAR id, value) =>
       if #s id = "HALT"
       then VARDECL (result, translValue value, HALT)
       else RET (translValue value)
     | Cps.APP_TAIL (Cps.CABS (id, cps), value) =>
       let val f = ABS ([id], transl cps)
           val arg = translValue value
       in
         RET (APP (f, [arg]))
       end
     | Cps.LET ((id, abs), cps) =>
       VARDECL (id, translAbs abs, transl cps)
     | Cps.LET_REC ((id, abs), cps) =>
       VARDECL (id, translAbs abs, transl cps)
     | Cps.IF (v, cps1, cps2) =>
       IF (translValue v, transl cps1, transl cps2)

  fun progToString prog =
    case prog of
        VARDECL (id, exp, prog) => "var " ^ Id.toString id ^ " = " ^ expToString exp ^ ";\n" ^ progToString prog
      | IF (exp, p1, p2) =>
        "if (" ^ expToString exp ^ ")\n" ^ "{ " ^ progToString p1 ^ " }\n"  ^ "else { " ^ progToString p2 ^ " };"
      | RET exp => "return (" ^ expToString exp ^ ");"
      | HALT => "console.log(result_0);"
  and expToString (INT i) = Int.toString i
    | expToString (BOOL b) = Bool.toString b
    | expToString (VAR id) =
      (case #s id of
          "+" => "plus"
        | "-" => "minus"
        | "*" => "times"
        | "<=" => "le"
        | _ => Id.toString id)
    | expToString (LIST exps) = PP.seqToString (expToString, "", ", ", "[", "]") exps
    | expToString (ABS (ids, prog)) =
      "(function (" ^ PP.seqToString (Id.toString, "", ", ", "", "") ids ^ ") { " ^ progToString prog ^ " })"
    | expToString (APP (func, args)) =
      expToString func ^ " (" ^ PP.seqToString (expToString, "", ", ", "", "") args ^ ")"
    | expToString (GET (e, ind)) = expToString e ^ ".[" ^ Int.toString ind ^ "]"
end
