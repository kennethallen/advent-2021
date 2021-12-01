module Day01

open Xunit
open System.IO
open Day01

let in0 = [199; 200; 208; 210; 200; 207; 240; 269; 260; 263]
let in1 = seq { yield! File.ReadLines "Input/01.txt" |> Seq.map int }

[<Fact>]
let part1_in0 () = Assert.Equal(7, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(1602, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(5, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(1633, part2 in1)
