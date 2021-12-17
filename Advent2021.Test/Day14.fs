module Day14

open Xunit
open System.IO
open Day14

let in0 = [
  "NNCB"
  ""
  "CH -> B"
  "HH -> N"
  "CB -> H"
  "NH -> C"
  "HB -> C"
  "HC -> B"
  "HN -> C"
  "NN -> C"
  "BH -> H"
  "NC -> B"
  "NB -> B"
  "BN -> B"
  "BB -> N"
  "BC -> B"
  "CC -> N"
  "CN -> C"]
let in1 = File.ReadLines "Input/14.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(1588UL, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(2937UL, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(2188189693529UL, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(3390034818249UL, part2 in1)
