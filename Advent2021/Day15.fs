module Day15

open System
open FSharpx.Collections

let private parse : string seq -> int array array =
  Seq.map (
    Seq.map (Array.singleton >> String >> int)
    >> Array.ofSeq)
  >> Array.ofSeq

let private dims = Day11.dims

let private heur (ax: int, ay: int) (bx, by) = Math.Abs(ax - bx) + Math.Abs(ay - by)

let private aStar map start dest =
  let maxX, maxY = dims map
  let rec step queue visited =
    let (_, d, (px, py)), queue = PriorityQueue.pop queue
    if Set.contains (px, py) visited then
      step queue visited
    elif (px, py) = dest then
      d
    else
      let queue =
        seq { px+1,py; px-1,py; px,py+1; px,py-1 }
        |> Seq.filter (fun (x, y) ->
          0 <= x && x < maxX && 0 <= y && y < maxY)
        |> Seq.filter (fun p -> not (Set.contains p visited))
        |> Seq.map (fun (x, y) ->
          let nd = d + map[x][y]
          nd + (heur (x, y) dest), nd, (x, y))
        |> Seq.fold (fun q e -> PriorityQueue.insert e q) queue
      step queue (Set.add (px, py) visited)
  step (Heap.ofSeq false [heur start dest, 0, start]) (Set.empty)

let part1 ls =
  let map = parse ls
  let maxX, maxY = dims map
  aStar map (0, 0) (maxX-1, maxY-1)
  
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
  let maxX, maxY = dims map
  aStar map (0, 0) (maxX-1, maxY-1)
    
