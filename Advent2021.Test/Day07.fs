module Day07

open Xunit
open System.IO
open Day07

let in0 = ["16,1,2,0,4,2,7,1,2,14"]
let in1 = File.ReadLines "Input/07.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(37, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(344735, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(168, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(96798233, part2 in1)
