let
  val pair = fn x => fn y => (x, y)
in
  let
    val pair1 = pair 1
  in
    (pair1 2, pair1 3)
  end
end;
