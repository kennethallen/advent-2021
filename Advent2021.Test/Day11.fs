module Day11

open Xunit
open System.IO
open Day11

let in0 = [
  "5483143223";
  "2745854711";
  "5264556173";
  "6141336146";
  "6357385478";
  "4167524645";
  "2176841721";
  "6882881134";
  "4846848554";
  "5283751526"]
let in1 = File.ReadLines "Input/11.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(1656, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(1732, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(195, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(290, part2 in1)
