structure Cse = struct
  open Cps

  fun termCSE env (t as CONST _) = t
    | termCSE env (t as VAR _) = t
    | termCSE env (ABS (xs, e)) =
        ABS (xs, expCSE env e)
    | termCSE env (t as PRIM _) = t

  and expCSE env (e as APP _) = e
    | expCSE env (LET_REC (bindings, e)) =
        let
          val bindings' = List.map (fn (x, t) =>
            let val t' = termCSE env t in
              case List.find ((fn t => t = t') o #2) env of
                   NONE => (x, t')
                 | SOME (x', _) => (x, VAR x')
            end) bindings
          val e' = expCSE (bindings' @ env) e
        in
          LET_REC (bindings', e')
        end
    | expCSE env (IF (x, e1, e2)) =
        let
          val e1' = expCSE env e1
          val e2' = expCSE env e2
        in
          if e1' = e2' then e1'
          else IF (x, e1', e2')
        end
end
