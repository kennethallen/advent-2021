module Day09

open Xunit
open System.IO
open Day09

let in0 = [
  "2199943210";
  "3987894921";
  "9856789892";
  "8767896789";
  "9899965678"]
let in1 = File.ReadLines "Input/09.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(15, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(594, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(1134, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(858494, part2 in1)
