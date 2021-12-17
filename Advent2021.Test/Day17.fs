module Day17

open Xunit
open System.IO
open Day17

let in0 = ["target area: x=20..30, y=-10..-5"]
let in1 = File.ReadLines "Input/17.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(45, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(4560, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(112, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(3344, part2 in1)
