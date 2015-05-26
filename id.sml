structure Id : ID = struct
  (* value identifier *)
  type t = string

  fun toString x = x
  fun fromString s = SOME s

  val seqToString = PP.seqToString (toString, "()", ", ", "(", ")") 

  local
    val seed = ref 0
  in
    fun gensym () =
      (seed := !seed + 1;
       Int.toString (!seed))
  end
end
