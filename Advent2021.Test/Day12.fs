module Day12

open Xunit
open System.IO
open Day12

let in0 = ["start-A";"start-b";"A-c";"A-b";"b-d";"A-end";"b-end"]
let in1 = [
  "dc-end"
  "HN-start"
  "start-kj"
  "dc-start"
  "dc-HN"
  "LN-dc"
  "HN-end"
  "kj-sa"
  "kj-HN"
  "kj-dc"]
let in2 = [
  "fs-end"
  "he-DX"
  "fs-he"
  "start-DX"
  "pj-DX"
  "end-zg"
  "zg-sl"
  "zg-pj"
  "pj-he"
  "RW-he"
  "fs-DX"
  "pj-RW"
  "zg-RW"
  "start-pj"
  "he-WI"
  "zg-he"
  "pj-fs"
  "start-RW"]
let in3 = File.ReadLines "Input/12.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(10, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(19, part1 in1)
[<Fact>]
let part1_in2 () = Assert.Equal(226, part1 in2)
[<Fact>]
let part1_in3 () = Assert.Equal(4495, part1 in3)

[<Fact>]
let part2_in0 () = Assert.Equal(36, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(103, part2 in1)
[<Fact>]
let part2_in2 () = Assert.Equal(3509, part2 in2)
[<Fact>]
let part2_in3 () = Assert.Equal(131254, part2 in3)
