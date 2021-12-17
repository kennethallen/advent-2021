module Day16

open Xunit
open System.IO
open Day16

let in1 = File.ReadLines "Input/16.txt" |> Seq.cache

[<Fact>]
let part1_in0 () = Assert.Equal(16UL, part1 ["8A004A801A8002F478"])
[<Fact>]
let part1_in1 () = Assert.Equal(12UL, part1 ["620080001611562C8802118E34"])
[<Fact>]
let part1_in2 () = Assert.Equal(23UL, part1 ["C0015000016115A2E0802F182340"])
[<Fact>]
let part1_in3 () = Assert.Equal(31UL, part1 ["A0016C880162017C3686B18A3D4780"])
[<Fact>]
let part1_in4 () = Assert.Equal(1007UL, part1 in1)

[<Fact>]
let part2_in0 () = Assert.Equal(3UL, part2 ["C200B40A82"])
[<Fact>]
let part2_in1 () = Assert.Equal(54UL, part2 ["04005AC33890"])
[<Fact>]
let part2_in2 () = Assert.Equal(7UL, part2 ["880086C3E88112"])
[<Fact>]
let part2_in3 () = Assert.Equal(9UL, part2 ["CE00C43D881120"])
[<Fact>]
let part2_in4 () = Assert.Equal(1UL, part2 ["D8005AC2A8F0"])
[<Fact>]
let part2_in5 () = Assert.Equal(0UL, part2 ["F600BC2D8F"])
[<Fact>]
let part2_in6 () = Assert.Equal(0UL, part2 ["9C005AC2F8F0"])
[<Fact>]
let part2_in7 () = Assert.Equal(1UL, part2 ["9C0141080250320F1802104A08"])
[<Fact>]
let part2_in8 () = Assert.Equal(834151779165UL, part2 in1)
