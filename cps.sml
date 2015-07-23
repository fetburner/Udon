structure Cps = struct
  (* t *)
  datatype term =
    (* c *)
      CONST of Const.t
    (* l *)
    | LABEL of Id.t
    (* l_{x} *)
    | ARG of { func : Id.t, arg : Id.t }
    (* ( + ) (t_1, ... , t_n) *)
    | PRIM of { prim : Prim.t, args : term list }

  (* e *)
  datatype exp =
    (* t (t_1, ... , t_n) *)
      APP of { func : term, args : term list }
    (* if t then t () else t () *)
    | IF of { cond : term, then_ : term, else_ : term }

  type prog = { args : Id.t list, body : exp } IdMap.map

  fun termToString (CONST c) = Const.toString c
    | termToString (LABEL x) = Id.toString x
    | termToString (ARG { func = f, arg = x }) =
        Id.toString f ^ "_{" ^ Id.toString x ^ "}"
    | termToString (PRIM { prim = p, args = ts }) =
        "("
        ^ Prim.toString p
        ^ " "
        ^ termSeqToString ts
        ^ ")"
  and termSeqToString seq =
    PP.seqToString (termToString, "()", ", ", "(", ")") seq

  fun expToString (APP { func = t, args = ts }) =
        "("
        ^ termToString t
        ^ " "
        ^ termSeqToString ts
        ^ ")"
    | expToString (IF { cond = t, then_ = x, else_ = y }) =
        "(if "
        ^ termToString t
        ^ " then "
        ^ termToString x
        ^ " () else "
        ^ termToString y
        ^ " ())"

  fun progToString prog =
    PP.seqToString
      (fn (f, { args = xs, body = m }) =>
        Id.toString f
        ^ " = fn "
        ^ Id.seqToString xs
        ^ " => "
        ^ expToString m, "", "\n", "", "") (IdMap.listItemsi prog)

  fun mapTerm m t =
    case m t of
         SOME t' => t'
       | NONE =>
           (case t of
                 CONST _ => t
               | LABEL _ => t
               | ARG { func = f, arg = x } =>
                   let val LABEL f' = mapTerm m (LABEL f) in
                     ARG { func = f, arg = x }
                   end
               | PRIM { prim = p, args = ts } =>
                   PRIM { prim = p, args = map (mapTerm m) ts })

  fun mapExp m (APP { func = f, args = ts }) =
        APP { func = mapTerm m f, args = map (mapTerm m) ts }
    | mapExp m (IF { cond = c, then_ = t, else_ = e }) =
        IF { cond = mapTerm m c, then_ = mapTerm m t, else_ = mapTerm m e }

  local
    open State
    infix >>= >>

    fun insert x args m =
      modify (fn p => IdMap.insert (p, x, { args = args, body = m }))
    fun lookup x = get >>= (fn p => return (IdMap.lookup (p, x)))
  in
    fun scopeTerm acc (CONST _) p = acc
      | scopeTerm acc (LABEL l) p =
          if isSome (IdMap.find (acc, l)) then acc
          else scopeProg acc l p
      | scopeTerm acc (ARG _) p = acc
      | scopeTerm acc (PRIM { prim = _, args = ts }) p =
          foldl (fn (t, acc) => scopeTerm acc t p) acc ts

    and scopeExp acc (APP { func = f, args = ts }) p =
          foldl (fn (t, acc) => scopeTerm acc t p) (scopeTerm acc f p) ts
      | scopeExp acc (IF { cond = c, then_ = t, else_ = e }) p =
          scopeTerm (scopeTerm (scopeTerm acc c p) t p) e p

    and scopeProg acc l (p : prog) =
      scopeExp (IdMap.insert (acc, l, Id.gensym "l"))
        (#body (IdMap.lookup (p, l))) p

    fun scope l = get >>= (fn p => return (scopeProg IdMap.empty l p))

    fun mangle entry t m =
      scope entry >>= (fn ls =>
        let
          val ls' = #1 (IdMap.remove (ls, entry))
          fun m' (t as LABEL l) =
                (case IdMap.find (ls', l) of
                      SOME l' => SOME (LABEL l')
                    | NONE => m t)
            | m' t = m t
          val entry' = Id.gensym "l_entry"
        in
          foldl op >> (return ())
            (map (fn (l, l') =>
              lookup l >>= (fn { args = args, body = body } =>
                insert l' args (mapExp m body))) (IdMap.listItemsi ls))
          >> lookup entry
          >>= (fn { args = _, body = body } =>
            insert entry' t (mapExp m' body))
          >> return entry'
        end)
  end

end
