structure Id = struct
  (* value identifier *)
  type t = string

  fun toString x = x

  local
    val seed = ref 0
  in
    fun gensym () =
      (seed := !seed + 1;
       Int.toString (!seed))
  end
end
