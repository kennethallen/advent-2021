﻿module Day11

open System

let parse : string seq -> int array array =
  Seq.map (
    Seq.map (Array.singleton >> String >> int)
    >> Array.ofSeq)
  >> Array.ofSeq

let dims map =
  Array.length map,
  Array.tryHead map
  |> Option.map Array.length
  |> Option.defaultValue 0

let allCoords map =
  let x, y = dims map
  Seq.allPairs { 0 .. x-1 } { 0 .. y-1 }

let adjacent (x0 : int, y0 : int) (x1, y1) =
  Math.Abs(x0 - x1) <= 1 && Math.Abs(y0 - y1) <= 1

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
  let x, y = dims init
  init
  |> Seq.unfold (step >> Some)
  |> Seq.findIndex ((=) (x * y))
  |> (+) 1