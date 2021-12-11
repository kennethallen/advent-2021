module Day11

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

let rec stepInner flared map =
  let newFlares =
    allCoords map
    |> Seq.filter (fun (x, y) -> map[x][y] > 9)
    |> Seq.filter (fun p ->
      not (Set.contains p flared))
    |> Set.ofSeq
  if Set.isEmpty newFlares then
    resetFlares map, Set.count flared
  else
    let map' =
      map
      |> Array.mapi (fun x -> Array.mapi (fun y n ->
        newFlares
        |> Seq.filter (adjacent (x, y))
        |> Seq.length
        |> (+) n))
    stepInner (Set.union flared newFlares) map'

let step map =
  stepInner Set.empty (Array.map (Array.map ((+) 1)) map)

let part1 ls =
  let init = parse ls
  { 1 .. 100 }
  |> Seq.fold (fun (map, flares) _ ->
    let map', flares' = step map
    map', flares + flares') (init, 0)
  |> snd

let part2 ls =
  let init = parse ls
  let x, y = dims init
  let octopoda = x * y
  init
  |> Seq.unfold (fun map ->
    let map', flares = step map
    Some (flares, map'))
  |> Seq.indexed
  |> Seq.pick (fun (i, flares) ->
    if flares = octopoda then Some (i+1) else None)