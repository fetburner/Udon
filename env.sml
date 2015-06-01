structure Env : ENV = struct
  type 'a t = (Id.t * 'a) list

  val empty = []
  fun insert (env, x, v) = (x, v) :: env
  fun insertList (env, bindings) =
    foldl (fn ((x, v), env) =>
      insert (env, x, v)) env bindings
  fun find (env, x) =
    Option.map #2 (List.find (fn (y, _) => x = y) env)
  fun findName (env: Type.t t, x: string) =
    (List.find (fn ({s, id}, _) => x = s) env)
end
