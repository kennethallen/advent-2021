module Day05

open Xunit
open System.IO
open Day05

let in0 = [
  "0,9 -> 5,9"
  "8,0 -> 0,8"
  "9,4 -> 3,4"
  "2,2 -> 2,1"
  "7,0 -> 7,4"
  "6,4 -> 2,0"
  "0,9 -> 2,9"
  "3,4 -> 1,4"
  "0,0 -> 8,8"
  "5,5 -> 8,2"]
let in1 = File.ReadAllLines "Input/05.txt"

[<Fact>]
let part1_in0 () = Assert.Equal(5, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(6710, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(12, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(20121, part2 in1)
