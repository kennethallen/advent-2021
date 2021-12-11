module Day04
#nowarn "25"

open System

let parse (ls : string seq) =
  let ns = (Seq.head ls).Split "," |> Seq.map int |> List.ofSeq
  let bs =
    ls
    |> Seq.tail
    |> Seq.chunkBySize 6
    |> Seq.map (fun chunk ->
      Seq.tail chunk
      |> Seq.map (fun row ->
        row.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int)
      |> Array.ofSeq)
    |> List.ofSeq
  ns, bs

let markAt b (x, y) =
  b |> Array.updateAt x (b[x] |> Array.updateAt y None)

let rowWin x =
  Array.item x >> Array.forall Option.isNone
let colWin y =
  Array.forall (Array.item y >> Option.isNone)

let score n =
  Seq.collect (Seq.choose id)
  >> Seq.sum
  >> (*) n

let playToEnd nsAll init =
  let rec f b i ns =
    let n::ns' = ns
    let matches =
      b |> Seq.indexed |> Seq.collect (fun (x, r) ->
        r |> Seq.indexed |> Seq.choose (fun (y, v) ->
          if v = Some n then Some (x, y) else None))
    if Seq.isEmpty matches then
      f b (i+1) ns'
    else
      let b' = Seq.fold markAt b matches
      matches
      |> Seq.tryPick (fun (x, y) ->
        if rowWin x b' || colWin y b' then
          Some (i, score n b')
        else
          None)
      |> Option.defaultWith (fun () ->
        f b' (i+1) ns')
  f (init |> Array.map (Array.map Some)) 0 nsAll
 
let playedBoards ls =
  let ns, bs = parse ls
  bs |> Seq.map (playToEnd ns)

let part1 : string seq -> int =
  playedBoards
  >> Seq.minBy fst
  >> snd

let part2 : string seq -> int =
  playedBoards
  >> Seq.maxBy fst
  >> snd
