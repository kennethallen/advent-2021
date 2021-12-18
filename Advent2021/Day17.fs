module Day17
#nowarn "25"

open System
open System.Text.RegularExpressions

let regex = Regex @"^target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)$"
let parse l =
  let [xMin; xMax; yMin; yMax] =
    (regex.Match l).Groups
    |> Seq.tail
    |> Seq.map (fun g -> int g.Value)
    |> List.ofSeq
  (xMin, xMax), (yMin, yMax)

let testHits ((xMin, xMax), (yMin, yMax)) (vx, vy) =
  let rec step vx vy x y =
    if xMin <= x && x <= xMax && yMin <= y && y <= yMax then
      true
    elif (x < xMin && vx <= 0)
        || (x > xMax && vx >= 0)
        || (y < yMin && vy <= 0) then
      false
    else
      let dirX = Math.Max(Math.Min(vx, 1), -1)
      step (vx-dirX) (vy-1) (x+vx) (y+vy)
  step vx vy 0 0

let enumHits ((xMin, xMax), (yMin, yMax)) =
  Seq.allPairs
    { Math.Min(xMin, 0) .. Math.Max(0, xMax) }
    { Math.Min(yMin, 0) .. 100 } // TODO: Calc better bounds
  |> Seq.filter (testHits ((xMin, xMax), (yMin, yMax)))

let peakY (_, vy) =
  vy * (vy+1) / 2

let part1 : string seq -> int =
  Seq.head
  >> parse
  >> enumHits
  >> Seq.map peakY
  >> Seq.max

let part2 : string seq -> int =
  Seq.head
  >> parse
  >> enumHits
  >> Seq.length
