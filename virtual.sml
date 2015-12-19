structure Virtual = struct
  datatype inst =
    (* x := c *)
      CONST of Id.t * Const.t
    (* x := y *)
    | VAR of Id.t * Id.t
    (* x := l *)
    | LABEL of Id.t * Id.t
    (* x := ( + ) (y_1, ..., y_n) *)
    | PRIM of Id.t * Prim.t * Id.t list
    (* x := (y_1, ..., y_n) *)
    | TUPLE of Id.t * Id.t list
    (* x := #n y *)
    | PROJ of Id.t * int * Id.t
    (* goto l *)
    | GOTO of Id.t
    (* goto [x] *)
    | GOTO_VAR of Id.t
    (* if x then goto l1 else goto l2 *)
    | IF of Id.t * Id.t * Id.t

  type program = (Id.t * inst list) list

  fun instToString (CONST (x, c)) =
        Id.toString x
        ^ " := "
        ^ Const.toString c
    | instToString (VAR (x, y)) =
        Id.toString x
        ^ " := "
        ^ Id.toString y
    | instToString (LABEL (x, l)) =
        Id.toString x
        ^ " := "
        ^ Id.toString l
    | instToString (PRIM (x, p, ys)) =
        Id.toString x
        ^ " := "
        ^ Prim.toString p
        ^ " "
        ^ Id.seqToString ys
    | instToString (TUPLE (x, ys)) =
        Id.toString x
        ^ " := "
        ^ Id.seqToString ys
    | instToString (PROJ (x, n, y)) =
        Id.toString x
        ^ " := #"
        ^ Int.toString n
        ^ " "
        ^ Id.toString y
    | instToString (GOTO l) =
        "goto " ^ Id.toString l
    | instToString (GOTO_VAR x) =
        "goto [" ^ Id.toString x ^ "]"
    | instToString (IF (x, l1, l2)) =
        "if "
        ^ Id.toString x
        ^ " then goto "
        ^ Id.toString l1
        ^ " else goto "
        ^ Id.toString l2

  val programToString =
    foldr op^ "" o map (fn (l, instrs) =>
      Id.toString l
      ^ ":\n"
      ^ foldr op^ "" (map (fn i => "  " ^ instToString i ^ "\n") instrs)
      ^ "\n")

  local
    datatype 'a idseq' = CONS of 'a * 'a idseq
    withtype 'a idseq = 'a idseq' option ref
    val stackBody = ref NONE
    fun stackAux n (r as ref NONE) =
          (r := SOME (CONS (Id.gensym "stack", ref NONE)); stackAux n r)
      | stackAux 0 (ref (SOME (CONS (x, _)))) = x
      | stackAux n (ref (SOME (CONS (_, r)))) = stackAux (n - 1) r
  in
    fun stack n = stackAux n stackBody
  end

  fun expCodeGeneration mapping toplevel (Closure.APP (x, e, ys)) =
        let val SOME (direct, fv, args) = Env.find (mapping, x) in
          (toplevel, List.mapPartial (fn (x, y) =>
            if x = y then NONE
            else SOME (VAR (x, y))) (ListPair.zip (fv, e))
          @ List.mapPartial (fn (x, y) =>
            if x = y then NONE
            else SOME (VAR (x, y))) (ListPair.zip (args, ys))
          @ [ GOTO direct ])
        end
    | expCodeGeneration mapping toplevel (Closure.APP_CLOS (c, xs)) =
        let val f = Id.gensym "f" in
          (toplevel, #1
            (foldl (fn (x, (instrs, n)) =>
              if stack n = x then (instrs, n + 1)
              else (VAR (stack n, x) :: instrs, n + 1))
              ([ PROJ (stack 0, 0, c), PROJ (f, 1, c), GOTO_VAR f ], 1) xs))
        end
    | expCodeGeneration mapping toplevel (Closure.LET ((x, t), m)) =
        let val (toplevel', m') = expCodeGeneration mapping toplevel m in
          (toplevel', case t of
               Closure.CONST c => CONST (x, c) :: m'
             | Closure.VAR y => VAR (x, y) :: m'
             | Closure.PRIM (p, ys) => PRIM (x, p, ys) :: m'
             | Closure.TUPLE ys => TUPLE (x, ys) :: m'
             | Closure.PROJ (n, y) => PROJ (x, n, y) :: m'
             | Closure.CLOSURE (e, f) =>
                 let
                   val e' = Id.gensym "e"
                   val f' = Id.gensym "f"
                 in
                   TUPLE (e', e)
                   :: LABEL (f', f)
                   :: TUPLE (x, [e', f'])
                   :: m'
                 end)
        end
    | expCodeGeneration mapping toplevel (Closure.IF (x, m, n)) =
        let
          val l1 = Id.gensym "then"
          val l2 = Id.gensym "else"
          val (toplevel', m') = expCodeGeneration mapping toplevel m
          val (toplevel'', n') = expCodeGeneration mapping toplevel' n
        in
          ((l1, m') :: (l2, n') :: toplevel'', [IF (x, l1, l2)])
        end

  fun codeGeneration (fundecs, m) =
    let
      val (mapping, toplevel) = ListPair.unzip (map (fn (f, (e, xs, _)) =>
          let val l = Id.gensym "direct" in
            ((f, (l, e, xs)),
             (f, #1 (foldl (fn (x, (codes, n)) =>
               if stack n = x then (codes, n + 1)
               else (VAR (x, stack n) :: codes, n + 1))
               (#1 (foldl (fn (x, (codes, n)) =>
                 (PROJ (x, n, stack 0) :: codes, n + 1)) ([ GOTO f ], 0) e), 1) xs)))
          end) fundecs)
      val mapping = Env.fromList mapping
      val (toplevel', m') = expCodeGeneration mapping toplevel m
    in
      (Id.gensym "main", m') :: foldl (fn ((f, (_, _, m)), toplevel) =>
        let val (toplevel', m') = expCodeGeneration mapping toplevel m in
          let val SOME (l, _, _) = Env.find (mapping, f) in
            (l, m') :: toplevel'
          end
        end) toplevel' fundecs
    end
end
