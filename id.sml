structure Id : ID = struct
  (* value identifier *)
  type t = string * int

  fun toString (s, id) = s ^ "_" ^ Int.toString id
  (* fun fromString s = SOME s *)

  val seqToString = PP.seqToString (toString, "()", ", ", "(", ")")

  local
    val seed = ref 0
  in
    fun genId () =
      (seed := !seed + 1;
       !seed)
  end

  fun gensym s = (s, genId ())
end
