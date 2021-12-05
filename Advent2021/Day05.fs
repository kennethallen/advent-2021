module Day05

open System
open System.Text.RegularExpressions

let regex = Regex "^(\d+),(\d+) -> (\d+),(\d+)$"
let parseLine s =
  let [_; x0; y0; x1; y1] = (regex.Match s).Groups |> List.ofSeq
  ((int x0.Value, int y0.Value), (int x1.Value, int y1.Value))

let enumSeg diags ((x0: int, y0: int), (x1, y1)) =
  let xMin = Math.Min(x0, x1)
  let xMax = Math.Max(x0, x1)
  let yMin = Math.Min(y0, y1)
  let yMax = Math.Max(y0, y1)
  let count = Math.Max(xMax - xMin, yMax - yMin)
  let enumPoints f = seq { 0 .. count } |> Seq.map f
  if x0 = x1 then
    enumPoints (fun i -> (x0, yMin + i))
  elif y0 = y1 then
    enumPoints (fun i -> (xMin + i, y0))
  elif diags then
    if x1 - x0 = y1 - y0 then
      enumPoints (fun i -> (xMin + i, yMin + i))
    else
      enumPoints (fun i -> (xMin + i, yMax - i))
  else
    Seq.empty

let countPoint map point =
  map
  |> Map.change point (fun n ->
    Some (defaultArg n 0 |> (+) 1))

let run diags =
  Seq.map parseLine
  >> Seq.collect (enumSeg diags)
  >> Seq.fold countPoint Map.empty
  >> Map.values
  >> Seq.filter ((<=) 2)
  >> Seq.length

let part1: (seq<string> -> int) = run false
let part2: (seq<string> -> int) = run true