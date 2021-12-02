module Day01

let part1 ls = ds |> Seq.map int |> part1Impl
let part1Impl ds =
  ds
  |> Seq.pairwise
  |> Seq.filter (fun (d0, d1) -> d1 > d0)
  |> Seq.length

let part2 ds =
  ds
  |> Seq.map int
  |> Seq.windowed 3
  |> Seq.map Array.sum
  |> part1Impl
