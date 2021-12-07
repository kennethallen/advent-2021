module Day07

open System

let calc costFn (ls: seq<string>) =
  let cs = ls |> Seq.collect (fun l -> l.Split(",")) |> Seq.map int |> List.ofSeq
  seq { List.min cs .. List.max cs }
  |> Seq.map (fun x ->
    cs |> Seq.sumBy (fun c -> Math.Abs(c - x) |> costFn))
  |> Seq.min

let part1: (seq<string> -> int) = calc id
let part2: (seq<string> -> int) = calc (fun d -> d * (d+1) / 2)
