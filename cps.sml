structure Cps = struct
  (* t *)
  datatype term =
    (* c *)
      CONST of Const.t
    (* l *)
    | LABEL of Id.t
    (* l_{x} *)
    | ARG of { func : Id.t, arg : Id.t }
    (* ( + ) (t_1, ... , t_n) *)
    | PRIM of { prim : Prim.t, args : term list }

  (* e *)
  datatype exp =
    (* t (t_1, ... , t_n) *)
      APP of { func : term, args : term list }
    (* if t then t () else t () *)
    | IF of { cond : term, then_ : term, else_ : term }

  type prog = { args : Id.t list, body : exp } IdMap.map

  fun termToString (CONST c) = Const.toString c
    | termToString (LABEL x) = Id.toString x
    | termToString (ARG { func = f, arg = x }) =
        Id.toString f ^ "_{" ^ Id.toString x ^ "}"
    | termToString (PRIM { prim = p, args = ts }) =
        "("
        ^ Prim.toString p
        ^ " "
        ^ termSeqToString ts
        ^ ")"
  and termSeqToString seq =
    PP.seqToString (termToString, "()", ", ", "(", ")") seq

  fun expToString (APP { func = t, args = ts }) =
        "("
        ^ termToString t
        ^ " "
        ^ termSeqToString ts
        ^ ")"
    | expToString (IF { cond = t, then_ = x, else_ = y }) =
        "(if "
        ^ termToString t
        ^ " then "
        ^ termToString x
        ^ " () else "
        ^ termToString y
        ^ " ())"

  fun progToString prog =
    PP.seqToString
      (fn (f, { args = xs, body = m }) =>
        Id.toString f
        ^ " = fn "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString m, "", "\n", "", "") (IdMap.listItemsi prog)
end
