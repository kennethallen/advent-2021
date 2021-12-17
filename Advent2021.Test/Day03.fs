module Day03

open Xunit
open System.IO
open Day03

let in0 = [
  "00100"
  "11110"; 
  "10110"
  "10111"
  "10101"
  "01111"
  "00111"
  "11100"
  "10000"
  "11001"
  "00010"
  "01010"]
let in1 = File.ReadLines "Input/03.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(198, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(3549854, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(230, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(3765399, part2 in1)
