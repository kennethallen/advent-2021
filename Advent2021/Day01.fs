module Day01

let part1 ds =
  ds
  |> Seq.pairwise
  |> Seq.filter (fun (d0, d1) -> d1 > d0)
  |> Seq.length

let part2 (ds: seq<int>) =
  ds
  |> Seq.windowed 3
  |> Seq.map Array.sum
  |> part1