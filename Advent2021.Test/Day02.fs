module Day02

open Xunit
open System.IO
open Day02

let in0 = ["forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"]
let in1 = File.ReadAllLines "Input/02.txt"

[<Fact>]
let part1_in0 () = Assert.Equal(150, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(0, part1 in1)

/*[<Fact>]
let part2_in0 () = Assert.Equal(5, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(1633, part2 in1)*/
