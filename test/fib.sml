let
  val rec fact = fn n => if n <= 1 then 1 else n * fact (n - 1)
in
  fact 10
end;
