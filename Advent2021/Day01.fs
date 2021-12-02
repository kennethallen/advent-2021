module Day01

let part1Impl ds =
  ds
  |> Seq.pairwise
  |> Seq.filter (fun (d0, d1) -> d1 > d0)
  |> Seq.length
let part1 (ls: seq<string>) = ls |> Seq.map int |> part1Impl

let part2 (ls: seq<string>) =
  ls
  |> Seq.map int
  |> Seq.windowed 3
  |> Seq.map Array.sum
  |> part1Impl
