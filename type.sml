structure Type = struct
  datatype t =
    (* type variable *)
      VAR of tvar ref
    (* quantified type variable *)
    | META of Id.t
    | INT
    | BOOL
    | FUN of t list * t
    | TUPLE of t list
  and tvar = UNBOUND of Id.t * int | LINK of t

  fun toString (VAR (ref (LINK t))) = toString t
    | toString (VAR (ref (UNBOUND (x, l)))) =
        "'_" ^ Id.toString x ^ "_l" ^ Int.toString l
    | toString (META x) = "'" ^ Id.toString x
    | toString INT = "int"
    | toString BOOL = "bool"
    | toString (FUN (ts, t)) = "(" ^ seqToString ts ^ " -> " ^ toString t ^ ")"
    | toString (TUPLE ts) = "(" ^ seqToString ts ^ ")"
  and seqToString ts = PP.seqToString (toString, "unit", " * ", "", "") ts

  (* generate type variable in current level *)
  fun genvar l = VAR (ref (UNBOUND (Id.gensym "a", l)))

  (* instantiate quantified type variable in current level *)
  fun inst l t =
    let
      val env = ref Env.empty
      fun instBody (t as (VAR (ref (UNBOUND _)))) = t
        | instBody (VAR (ref (LINK t))) = instBody t
        | instBody (META x) =
            (case Env.find (!env, x) of
                  SOME t => t
                | NONE =>
                    let
                      val t = genvar l
                    in
                      env := Env.insert (!env, x, t);
                      t
                    end)
        | instBody (t as INT) = t
        | instBody (t as BOOL) = t
        | instBody (FUN (t1s, t2)) = FUN (map instBody t1s, instBody t2)
        | instBody (TUPLE ts) = TUPLE (map instBody ts)
    in
      instBody t
    end

  (* generalize type variable in current level *)
  fun generalize l (VAR (ref (LINK t))) = generalize l t
    | generalize l (t as (VAR (ref (UNBOUND (x, l'))))) =
        if l < l' then META x else t
    | generalize l (t as (META _)) = t
    | generalize l (t as INT) = t
    | generalize l (t as BOOL) = t
    | generalize l (FUN (t1s, t2)) =
        FUN (map (generalize l) t1s, generalize l t2)
    | generalize l (TUPLE ts) = TUPLE (map (generalize l) ts)

  (* exception that arises when type checker fail to unify types *)
  exception Unify of t * t

  local
    (* occur check *)
    fun occur r1 (FUN (t21s, t22)) =
          List.exists (occur r1) t21s orelse occur r1 t22
      | occur r1 (VAR (r2 as (ref (UNBOUND _)))) = r1 = r2
      | occur r1 (VAR (r2 as (ref (LINK t2)))) =
          r1 = r2 orelse occur r1 t2
      | occur r1 (META _) = false
      | occur r1 (TUPLE ts) = List.exists (occur r1) ts
      | occur r1 INT = false
      | occur r1 BOOL = false
  in
    (* unifier *)
    fun unify (INT, INT) = ()
      | unify (BOOL, BOOL) = ()
      | unify (FUN (t11s, t12), FUN (t21s, t22)) =
          (ListPair.appEq unify (t11s, t21s);
           unify (t12, t22))
      | unify (TUPLE t1s, TUPLE t2s) =
           ListPair.appEq unify (t1s, t2s)
      | unify (VAR (ref (LINK t1)), t2) = unify (t1, t2)
      | unify (t1, VAR (ref (LINK t2))) = unify (t1, t2)
      | unify (t1 as (VAR (r1 as (ref (UNBOUND (_, l1))))),
               t2 as (VAR (r2 as (ref (UNBOUND (_, l2)))))) =
          if r1 = r2 then ()
          else if occur r1 t2 then raise (Unify (t1, t2))
          else if l1 < l2 then r2 := LINK t1
          else r1 := LINK t2
      | unify (t1 as (VAR (r1 as (ref (UNBOUND _)))), t2) =
          if occur r1 t2 then raise (Unify (t1, t2))
          else r1 := LINK t2
      | unify (t1, t2 as (VAR (r2 as (ref (UNBOUND _))))) =
          if occur r2 t1 then raise (Unify (t1, t2))
          else r2 := LINK t1
      | unify (t1, t2) = raise (Unify (t1, t2))
  end
end

