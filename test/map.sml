let
  val make = fn n => fn m => (n, n + 1, m, m + 1)
in
  let
    val map = fn f => fn t => case t of (a, b, c, d) => (f a, f b, f c, f d)
  in
    map (fn x => x * x) (map (fn x => x + 1) (make 1 3))
  end
end;
