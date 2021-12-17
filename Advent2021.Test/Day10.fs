module Day10

open Xunit
open System.IO
open Day10

let in0 = [
  "[({(<(())[]>[[{[]{<()<>>"
  "[(()[<>])]({[<{<<[]>>("
  "{([(<{}[<>[]}>{[]{[(<()>"
  "(((({<>}<{<{<>}{[]{[]{}"
  "[[<[([]))<([[{}[[()]]]"
  "[{[{({}]{}}([{[{{{}}([]"
  "{<[[]]>}<{[{[{[]{()[[[]"
  "[<(<(<(<{}))><([]([]()"
  "<{([([[(<>()){}]>(<<{{"
  "<{([{{}}[<[[[<>{}]]]>[]]"]
let in1 = File.ReadLines "Input/10.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(26397, part1 in0)
[<Fact>]
let part1_in1 () = Assert.Equal(369105, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(288957UL, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(3999363569UL, part2 in1)
