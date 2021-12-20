module Day19

open Xunit
open System.IO
open Day19

let in0 = File.ReadLines "Input/19a.txt" |> Seq.cache
let in1 = File.ReadLines "Input/19.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(79, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(303, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(3621, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(9621, part2 in1)
