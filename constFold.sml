structure ConstFold = struct
  open Cps

  (* constant foldings *)
  fun constFoldPrim Prim.PLUS [ CONST (Const.INT m), CONST (Const.INT n) ] =
        CONST (Const.INT (m + n))
    | constFoldPrim Prim.PLUS [ t, CONST (Const.INT 0) ] = t
    | constFoldPrim Prim.PLUS [ CONST (Const.INT 0), t ] = t
    | constFoldPrim Prim.MINUS [ CONST (Const.INT m), CONST (Const.INT n) ] =
        CONST (Const.INT (m - n))
    | constFoldPrim Prim.MINUS [ t, CONST (Const.INT 0) ] = t
    | constFoldPrim Prim.TIMES [ CONST (Const.INT m), CONST (Const.INT n) ] =
        CONST (Const.INT (m * n))
    | constFoldPrim Prim.TIMES [ _, CONST (Const.INT 0) ] = CONST (Const.INT 0)
    | constFoldPrim Prim.TIMES [ CONST (Const.INT 0), _ ] = CONST (Const.INT 0)
    | constFoldPrim Prim.TIMES [ t, CONST (Const.INT 1) ] = t
    | constFoldPrim Prim.TIMES [ CONST (Const.INT 1), t ] = t
    | constFoldPrim Prim.LE [ CONST (Const.INT m), CONST (Const.INT n) ] =
        CONST (Const.BOOL (m < n))
    | constFoldPrim (Prim.TUPLE_GET n) [ PRIM { prim = Prim.TUPLE, args = ts } ] =
        List.nth (ts, n - 1)
    | constFoldPrim p ts = PRIM { prim = p, args = ts }

  fun constFoldTerm (c as CONST _) = c
    | constFoldTerm (c as LABEL _) = c
    | constFoldTerm (c as ARG _) = c
    | constFoldTerm (PRIM { prim = p, args = ts }) =
        constFoldPrim p (map constFoldTerm ts)

  fun constFoldExp (APP { func = t, args = ts }) =
        APP { func = constFoldTerm t, args = map constFoldTerm ts }
    | constFoldExp (IF { cond = t1, then_ = t2, else_ = t3 }) =
        IF
          { cond = constFoldTerm t1,
            then_ = constFoldTerm t2,
            else_ = constFoldTerm t3 }

  and constFoldProg p =
    IdMap.map (fn { args = xs, body = m } =>
      { args = xs, body = constFoldExp m }) p

  val constFold = constFoldProg
end
