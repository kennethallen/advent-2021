module Day21

open Xunit
open System.IO
open Day21

let in0 = [
  "Player 1 starting position: 4"
  "Player 2 starting position: 8"]
let in1 = File.ReadLines "Input/21.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(739785, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(797160, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(444356092776315UL, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(27464148626406UL, part2 in1)
