module Day02

open Xunit
open System.IO
open Day02

let in0 = ["forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"]
let in1 = File.ReadLines "Input/02.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(150, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(1714950, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(900, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(1281977850, part2 in1)
