module Day05

open System
open System.Text.RegularExpressions

let regex = Regex "^(\d+),(\d+) -> (\d+),(\d+)$"
let parseLine s =
  let [_; x0; y0; x1; y1] = (regex.Match s).Groups |> List.ofSeq
  ((int x0.Value, int y0.Value), (int x1.Value, int y1.Value))

let onLineSimple ((x0: int, y0: int), (x1, y1)) (x, y) =
  if x0 = x1 then
    x = x0 && y <= Math.Max(y0, y1) && y >= Math.Min(y0, y1)
  elif y0 = y1 then
    y = y0 && x <= Math.Max(x0, x1) && x >= Math.Min(x0, x1)
  else
    false
let onLineFull ((x0: int, y0: int), (x1, y1)) (x, y) =
  x <= Math.Max(x0, x1) && x >= Math.Min(x0, x1)
  && y <= Math.Max(y0, y1) && y >= Math.Min(y0, y1)
  && (x - x0) * (y1 - y0) = (y - y0) * (x1 - x0)

let calcBounds lines =
  let (coords0, coords1) = lines |> List.unzip
  let (xs0, ys0) = coords0 |> List.unzip
  let (xs1, ys1) = coords1 |> List.unzip
  let xs = List.append xs0 xs1
  let ys = List.append ys0 ys1
  (xs |> List.min, xs |> List.max, ys |> List.min, ys |> List.max)
let allCoords (xMin, xMax, yMin, yMax) =
  seq { xMin .. xMax }
  |> Seq.collect (fun x ->
    seq { yMin .. yMax }
    |> Seq.map (fun y -> (x, y)))

let run lineCoordTest ls =
  let lines = ls |> Seq.map parseLine |> List.ofSeq
  let wins =
    lines
    |> calcBounds
    |> allCoords
    |> Seq.filter (fun coord ->
      lines
      |> Seq.filter (fun line -> lineCoordTest line coord)
      |> Seq.length
      |> (<=) 2)
  wins |> Seq.length

let part1: (seq<string> -> int) = run onLineSimple
let part2: (seq<string> -> int) = run onLineFull