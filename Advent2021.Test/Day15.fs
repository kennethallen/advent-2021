module Day15

open Xunit
open System.IO
open Day15

let in0 = [
  "1163751742"
  "1381373672"
  "2136511328"
  "3694931569"
  "7463417111"
  "1319128137"
  "1359912421"
  "3125421639"
  "1293138521"
  "2311944581"]
let in1 = File.ReadLines "Input/15.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(40, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(523, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(315, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(2876, part2 in1)
