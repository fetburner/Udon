structure Id : ID = struct
  (* value identifier *)
  type t = {s: string, id: int}

  fun toString {s, id} = s ^ "_" ^ Int.toString id
  (* fun fromString s = SOME s *)

  val seqToString = PP.seqToString (toString, "()", ", ", "(", ")")

  local
    val seed = ref 1
  in
    fun genId () =
      (seed := !seed + 1;
       !seed)
  end

  fun gensym s = {s = s, id = genId ()}

  fun compare ({s = s1, id = id1}, {s = s2, id = id2}) =
    case String.compare (s1, s2) of
         EQUAL => Int.compare (id1, id2)
       | ord => ord

end
