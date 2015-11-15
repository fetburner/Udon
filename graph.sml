functor Graph (VSet : ORD_SET) = struct
  type vertex = VSet.item
  type vertexSet = VSet.set
  type edgeSet = vertex -> vertexSet
  type graph = vertexSet * edgeSet

  fun visit es (v, (vs, l)) =
    if VSet.member (vs, v) then
      let val (vs', l') =
        VSet.foldl (visit es) (VSet.delete (vs, v), l) (es v) in
        (vs', v :: l')
      end
    else (vs, l)

  fun sort (vs, es) = #2 (VSet.foldl (visit es) (vs, []) vs)

  fun scc (vs, es) =
    let
      infix |>
      fun x |> f = f x
    in
      sort (vs, es)
      |> rev
      |> foldl (fn (v, (vs, l)) =>
          case visit es (v, (vs, [])) of
               (vs', []) => (vs', l)
             | (vs', cs) => (vs', cs :: l)) (vs, [])
      |> #2
    end
end
