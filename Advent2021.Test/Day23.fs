module Day23

open Xunit
open System.IO
open Day23

let in0 = [
  "#############"
  "#...........#"
  "###B#C#B#D###"
  "  #A#D#C#A#"
  "  #########"]
let in1 = File.ReadLines "Input/23.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(12521L, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(590784L, part1 in1)
