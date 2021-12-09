module Day06

open Xunit
open System.IO
open Day06

let in0 = ["3,4,3,1,2"]
let in1 = File.ReadLines "Input/06.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(5934UL, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(362346UL, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(26984457539UL, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(1639643057051UL, part2 in1)
