module Day11

open System

let parse : string seq -> int array array =
  Seq.map (
    Seq.map (Array.singleton >> String >> int)
    >> Array.ofSeq)
  >> Array.ofSeq

let allCoords map =
  let x, y = Array.squareDims map
  Seq.allPairs { 0 .. x-1 } { 0 .. y-1 }

let adjacent (x0, y0) (x1, y1) =
  abs (x0-x1) <= 1 && abs (y0-y1) <= 1

let resetFlares =
  Array.map (Array.map (fun n -> if n > 9 then 0 else n))
let bloomFlares fs =
  Array.mapi (fun x -> Array.mapi (fun y ->
    fs
    |> Seq.filter (adjacent (x, y))
    |> Seq.length
    |> (+)))

let rec doFlares flared map =
  let newFlares =
    allCoords map
    |> Seq.filter (fun (x, y) -> map[x][y] > 9)
    |> Seq.filter (fun p -> not (Set.contains p flared))
    |> Set.ofSeq
  if Set.isEmpty newFlares then
    Set.count flared, resetFlares map
  else
    doFlares (Set.union flared newFlares) (bloomFlares newFlares map)

let step map =
  doFlares Set.empty (Array.map (Array.map ((+) 1)) map)

let part1 : string seq -> int =
  parse
  >> Seq.unfold (step >> Some)
  >> Seq.truncate 100
  >> Seq.sum

let part2 ls =
  let init = parse ls
  let x, y = Array.squareDims init
  init
  |> Seq.unfold (step >> Some)
  |> Seq.findIndex ((=) (x * y))
  |> (+) 1