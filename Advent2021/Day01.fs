module Day01

let part1Impl =
  Seq.pairwise
  >> Seq.filter (fun (d0, d1) -> d1 > d0)
  >> Seq.length
let part1: (seq<string> -> int) =
  Seq.map int<string>
  >> part1Impl

let part2: (seq<string> -> int) =
  Seq.map int
  >> Seq.windowed 3
  >> Seq.map Array.sum
  >> part1Impl
