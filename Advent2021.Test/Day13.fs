module Day13

open Xunit
open System.IO
open Day13

let in0 = [
  "6,10";
  "0,14";
  "9,10";
  "0,3";
  "10,4";
  "4,11";
  "6,0";
  "6,12";
  "4,1";
  "0,13";
  "10,12";
  "3,4";
  "3,0";
  "8,4";
  "1,10";
  "2,14";
  "8,10";
  "9,0";
  "";
  "fold along y=7";
  "fold along x=5"]
let in1 = File.ReadLines "Input/13.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(17, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(678, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(
  "#####\n" +
  "#...#\n" +
  "#...#\n" +
  "#...#\n" +
  "#####",
  part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(
  "####..##..####.#..#.#....#..#.####.####\n" +
  "#....#..#.#....#..#.#....#..#....#.#...\n" +
  "###..#....###..####.#....####...#..###.\n" +
  "#....#....#....#..#.#....#..#..#...#...\n" +
  "#....#..#.#....#..#.#....#..#.#....#...\n" +
  "####..##..#....#..#.####.#..#.####.#...",
  part2 in1)
