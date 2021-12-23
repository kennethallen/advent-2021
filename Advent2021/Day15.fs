module Day15

open System
open FSharpx.Collections

let private parse : string seq -> int array array =
  Seq.map (
    Seq.map (Array.singleton >> String >> int)
    >> Array.ofSeq)
  >> Array.ofSeq

let private heur (ax, ay) (bx, by) = abs (ax-bx) + abs (ay-by)

let private paths map (px, py) =
  seq { px+1,py; px-1,py; px,py+1; px,py-1 }
  |> Seq.choose (fun (x, y) ->
    map
    |> Array.tryItem x
    |> Option.bind (Array.tryItem y)
    |> Option.map (fun c -> c, (x, y)))

let part1 ls =
  let map = parse ls
  let maxX, maxY = Array.squareDims map
  AStar.aStar (paths map) heur (maxX-1, maxY-1) (0, 0)
  |> fst
  
let private tile xTiles yTiles map =
  { 0 .. xTiles-1 }
  |> Seq.collect (fun tileX ->
    map
    |> Seq.map (fun row ->
      { 0 .. yTiles-1 }
      |> Seq.collect (fun tileY ->
        row
        |> Seq.map (fun n ->
          match (n+tileX+tileY) % 9 with
          | 0 -> 9
          | n -> n))
      |> Array.ofSeq))
  |> Array.ofSeq

let part2 ls =
  let map = parse ls |> tile 5 5
  let maxX, maxY = Array.squareDims map
  AStar.aStar (paths map) heur (maxX-1, maxY-1) (0, 0)
  |> fst
