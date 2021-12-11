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
let step1 () =
  let s0 = [|
    [|1;1;1;1;1|];
    [|1;9;9;9;1|];
    [|1;9;1;9;1|];
    [|1;9;9;9;1|];
    [|1;1;1;1;1|]|]
  let s1 = [|
    [|3;4;5;4;3|];
    [|4;0;0;0;4|];
    [|5;0;0;0;5|];
    [|4;0;0;0;4|];
    [|3;4;5;4;3|]|]
  let s2 = Array.map (Array.map ((+) 1)) s1
  Assert.Equal((s1, 9), step s0)
  Assert.Equal((s2, 0), step s1)

[<Fact>]
let step2 () =
  let map0 = parse in0
  let map1, flares1 = Day11.step map0
  Assert.Equal<int array>(map1, parse [|"6594254334";"3856965822";"6375667284";"7252447257";"7468496589";"5278635756";"3287952832";"7993992245";"5957959665";"6394862637"|])
  Assert.Equal(0, flares1)
  let map2, flares2 = Day11.step map1
  Assert.Equal<int array>(map2, parse [|"8807476555";"5089087054";"8597889608";"8485769600";"8700908800";"6600088989";"6800005943";"0000007456";"9000000876";"8700006848"|])
  Assert.Equal(35, flares2)
  let map3, flares3 = Day11.step map2
  Assert.Equal<int array>(map3, parse [|"0050900866";"8500800575";"9900000039";"9700000041";"9935080063";"7712300000";"7911250009";"2211130000";"0421125000";"0021119000"|])
  Assert.Equal(45, flares3)
  let map4, flares4 = Day11.step map3
  Assert.Equal<int array>(map4, parse [|"2263031977";"0923031697";"0032221150";"0041111163";"0076191174";"0053411122";"0042361120";"5532241122";"1532247211";"1132230211"|])
  Assert.Equal(16, flares4)

[<Fact>]
let part2_in0 () = Assert.Equal(195, part2 in0)
[<Fact>]
let part2_in1 () = Assert.Equal(290, part2 in1)
