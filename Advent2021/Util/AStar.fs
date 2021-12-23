module AStar

open FSharpx.Collections

let aStar paths heur dest start =
  let rec step q vis =
    let (_, d, s, prev), q = PriorityQueue.pop q
    if Map.containsKey s vis then
      assert (
        let (d', _) = Map.find s vis
        d >= d')
      step q vis
    else
      let vis = Map.add s (d, prev) vis
      if s = dest then
        d, s |> List.unfold (
        (fun s -> Map.find s vis)
        >> snd
        >> Option.map (fun s -> s, s))
      else
        let q =
          paths s
          |> Seq.filter (fun (_, s') -> not (Map.containsKey s' vis))
          |> Seq.map (fun (c, s') ->
            let nd = d + c
            nd + (heur s' dest), nd, s', Some s)
          |> Seq.fold (fun q e -> PriorityQueue.insert e q) q
        step q vis
  step (Heap.ofSeq false [heur start dest, 0, start, None]) Map.empty
