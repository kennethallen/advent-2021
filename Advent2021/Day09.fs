module Day09

open System

let parse : string seq -> int array array =
  Seq.map (
    Seq.map (Array.singleton >> String >> int)
    >> Array.ofSeq)
  >> Array.ofSeq

let tryGet map (x, y) =
  Array.tryItem x map
  |> Option.bind (Array.tryItem y)

let part1 ls =
  let map = parse ls
  map
  |> Seq.indexed
  |> Seq.collect (fun (x, row) ->
    row
    |> Seq.indexed
    |> Seq.filter (fun (y, h) ->
      [x-1,y; x+1,y; x,y-1; x,y+1]
      |> Seq.choose (tryGet map)
      |> Seq.forall ((<) h))
    |> Seq.map snd)
  |> Seq.map ((+) 1)
  |> Seq.sum

let rec dfs map visited size =
  function
  | []    -> visited, size
  | p::ps ->
    match Set.contains p visited, tryGet map p with
    | true, _                -> dfs map visited size ps
    | false, (None | Some 9) -> dfs map (Set.add p visited) size ps
    | false, Some _          ->
      let x, y = p
      dfs map (Set.add p visited) (size+1) (List.append [x-1,y; x+1,y; x,y-1; x,y+1] ps)

let exploreForBasin map (visited, sizes) p =
  let visited', size = dfs map visited 0 [p]
  visited', if size = 0 then sizes else size::sizes

let part2 ls =
  let map = parse ls
  Seq.allPairs { 0 .. map.Length } { 0 .. map[0].Length }
  |> Seq.fold (exploreForBasin map) (Set.empty, [])
  |> snd
  |> Seq.sortDescending
  |> Seq.truncate 3
  |> Seq.fold (*) 1
