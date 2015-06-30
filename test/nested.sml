let
  val calc =
   fn x => fn y =>
      if x + y <= 0
      then
        let val double = fn sum => sum + x + y in double (x + y) end
      else
        let val rev = fn sum => 0 - sum  in rev (x + y) end
in
  (calc (0 - 2) (0 - 3), calc 3 4)
end;
