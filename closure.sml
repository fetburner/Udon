structure Closure = struct
  (* t *)
  datatype term =
    (* c *)
      CONST of Const.t
    (* x *)
    | VAR of Id.t
    (* ( + ) (x_1, ... , x_n) *)
    | PRIM of Prim.t * Id.t list
    (* (x_1, ..., x_n) *)
    | TUPLE of Id.t list
    (* #n x *)
    | PROJ of int * Id.t
    (* <E, f> *)
    | CLOSURE of Id.t list * Id.t
  (* e *)
  and exp =
    (* x (E, y_1, ..., y_n) *)
      APP of Id.t * Id.t list * Id.t list
    (* c (x_1, ..., x_n) *)
    | APP_CLOS of Id.t * Id.t list
    (* let val x = t in e end *)
    | LET of binding * exp
    (* if x then e1 else e2 *)
    | IF of Id.t * exp * exp
  withtype binding = Id.t * term

  type fundec = Id.t * (Id.t list * Id.t list * exp)
  (* p *)
  (* let val rec f = fn (E, x_11, ..., x_n1) => e and ... in e end *)
  type program = fundec list * exp


  fun termToString (CONST c) = Const.toString c
    | termToString (VAR x) = Id.toString x
    | termToString (PRIM (p, xs)) =
        Prim.toString p
        ^ " "
        ^ Id.seqToString xs
    | termToString (TUPLE xs) =
        Id.seqToString xs
    | termToString (PROJ (n, x)) =
        "#"
        ^ Int.toString n
        ^ " "
        ^ Id.toString x
    | termToString (CLOSURE (e, f)) =
        "<"
        ^ Id.seqToString e
        ^ ", "
        ^ Id.toString f
        ^ ">"

  fun expToString (APP (x, e, ys)) =
        Id.toString x
        ^ " "
        ^ Id.seqToString e
        ^ " "
        ^ Id.seqToString ys
    | expToString (APP_CLOS (c, xs)) =
        Id.toString c
        ^ " "
        ^ Id.seqToString xs
    | expToString (LET (binding, e)) =
        "let val rec "
        ^ bindingToString binding
        ^ " in "
        ^ expToString e
        ^ " end"
    | expToString (IF (x, e1, e2)) =
        "if "
        ^ Id.toString x
        ^ " then "
        ^ expToString e1
        ^ " else "
        ^ expToString e2

  and bindingToString (x, t) = Id.toString x ^ " = " ^ termToString t

  fun programToString (fundecs, m) =
    "let val rec "
    ^ PP.seqToString (fn (f, (e, xs, m)) =>
        Id.toString f
        ^ " = fn "
        ^ Id.seqToString e
        ^ " "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString m, "", " and ", "", "") fundecs
    ^ " in "
    ^ expToString m
    ^ " end"


  fun expClosureConv env toplevel (Cps.APP (x, ys)) =
        (toplevel, 
          let val (args, m) =
            case Env.find (env, x) of
                  NONE => (IdSet.fromList ys, APP_CLOS (x, ys))
                | SOME (x', e) =>
                    (IdSet.fromList (e @ ys), APP (x', e, ys)) in
            foldr (fn (x, m') =>
              case Env.find (env, x) of
                   NONE => m'
                 | SOME (x', e) => LET ((x, CLOSURE (e, x')), m'))
              m (IdSet.listItems args)
          end)
    | expClosureConv env toplevel (Cps.LET_REC (bindings as [(x, Cps.ABS (ys, m))], n)) =
        let
          val x' = Id.gensym "x"
          val e = IdSet.listItems (Cps.bindingsFreeVar bindings)
          val env' = Env.insert (env, x, (x', e))
          val (toplevel', m') = expClosureConv env' toplevel m
          val (toplevel'', n') = expClosureConv env' toplevel' n
        in
          ((x', (e, ys, m')) :: toplevel'', n')
        end
    | expClosureConv env toplevel (Cps.LET_REC ([(x, t)], m)) =
        let val (toplevel', m') = expClosureConv env toplevel m in
          (toplevel', LET ((x,
            case t of
                 Cps.CONST c => CONST c
               | Cps.VAR y => VAR y
               | Cps.PRIM (p, ys) => PRIM (p, ys)
               | Cps.TUPLE ys => TUPLE ys
               | Cps.PROJ (n, y) => PROJ (n, y)), m'))
        end
    | expClosureConv env toplevel (Cps.IF (x, m, n)) =
        let
          val (toplevel', m') = expClosureConv env toplevel m
          val (toplevel'', n') = expClosureConv env toplevel' n
        in
          (toplevel'', IF (x, m', n'))
        end

  val closureConv = expClosureConv Env.empty []
end
