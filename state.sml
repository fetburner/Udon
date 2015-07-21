structure State : STATE = struct
  type ('s, 'a) state = 's -> 'a * 's
  fun return x s = (x, s)

  infix >>= >>
  fun f >>= g = fn s =>
    let val (a, s') = f s in
      g a s'
    end

  fun f >> g = f >>= (fn _ => g)

  fun runState a s = a s

  fun get s = (s, s)

  fun put s _ = ((), s)

  fun modify f s = ((), f s)
end
