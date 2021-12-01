module Day01

let part1 ds =
  Seq.zip ds (Seq.skip 1 ds)
    |> Seq.filter (fun (d0, d1) -> d1 > d0)
    |> Seq.length

let part2 ds =
  Seq.zip3 ds (Seq.skip 1 ds) (Seq.skip 2 ds)
    |> Seq.map (fun (d0, d1, d2) -> d0 + d1 + d2)
    |> part1