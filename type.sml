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

  (* type scheme *)
  type scheme = Id.t list * t

  fun toTypeScheme t = ([], t)

  fun toString (VAR (ref (LINK t))) = toString t
    | toString (VAR (ref (UNBOUND (x, l)))) =
        "'_" ^ Id.toString x ^ "_l" ^ Int.toString l
    | toString (META x) = "'" ^ Id.toString x
    | toString INT = "int"
    | toString BOOL = "bool"
    | toString (FUN (ts, t)) = "(" ^ seqToString ts ^ " -> " ^ toString t ^ ")"
    | toString (TUPLE ts) = "(" ^ seqToString ts ^ ")"
  and seqToString ts = PP.seqToString (toString, "unit", " * ", "", "") ts

  fun schemeToString ([], t) = toString t
    | schemeToString (xs, t) =
        "forall "
        ^ PP.seqToString (fn x => Id.toString x, "", " ", "", "") xs
        ^ ", "
        ^ toString t

  (* generate type variable in current level *)
  fun genvar l = VAR (ref (UNBOUND (Id.gensym "a", l)))

  fun subst env (t as (VAR (ref (UNBOUND _)))) = t
    | subst env (VAR (ref (LINK t))) = subst env t
    | subst env (t as META x) =
        (case Env.find (env, x) of
              SOME t' => t'
            | NONE => t)
    | subst env (t as INT) = t
    | subst env (t as BOOL) = t
    | subst env (FUN (t1s, t2)) = FUN (map (subst env) t1s, subst env t2)
    | subst env (TUPLE ts) = TUPLE (map (subst env) ts)

  (* instantiate type scheme in current level *)
  fun inst l (xs, t) =
    let val xs' = map (fn x => (x, genvar l)) xs in
      (subst (Env.fromList xs') t, map #2 xs')
    end

  (* generalize type variable in current level *)
  fun generalize l t =
    let
      val bounds = ref IdSet.empty
      fun generalizeBody (VAR (ref (LINK t))) =
            generalizeBody t
        | generalizeBody (t as (VAR (ref (UNBOUND (x, l'))))) =
            if l < l' then
              (bounds := IdSet.add (!bounds, x);
               META x)
            else t
        | generalizeBody (t as (META _)) = t
        | generalizeBody (t as INT) = t
        | generalizeBody (t as BOOL) = t
        | generalizeBody (FUN (t1s, t2)) =
            FUN (map generalizeBody t1s, generalizeBody t2)
        | generalizeBody (TUPLE ts) = TUPLE (map generalizeBody ts)
      val t' = generalizeBody t
    in
      (IdSet.listItems (!bounds), t')
    end

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

