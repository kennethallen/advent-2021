module Day04

open System

let parse (ls: seq<string>) =
  let ns = (Seq.head ls).Split "," |> Seq.map int
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
  (ns, bs)

let play b ns =
  let marked =
    b |> Array.map (fun r ->
      r |> Array.map (fun e -> Seq.contains e ns))
  let rowWin = marked |> Array.exists (Array.forall id)
  let colWin =
    seq { 0 .. (Array.length b) - 1 }
    |> Seq.exists (fun i ->
      marked |> Array.forall (fun r -> r[i]))
  //let diagWin back = seq { 0 .. dim } |> Seq.forall (fun i ->
  //  marked[i][if back then dim-1-i else i])
  if rowWin || colWin then
    Some(
      (b, marked)
      ||> Seq.map2 (fun br mr ->
        (br, mr)
        ||> Seq.map2 (fun be me -> if me then 0 else be)
        |> Seq.sum)
      |> Seq.sum)
  else
    None
 
let part1 ls =
  let (ns, bs) = parse ls
  seq { 0 .. Seq.length ls }
  |> Seq.pick (fun i ->
    let ns' = ns |> Seq.take i
    let wins = bs |> Seq.choose (fun b -> play b ns')
    if Seq.isEmpty wins then
      None
    else
      Some ((Seq.last ns') * (Seq.max wins)))

let part2 ls =
  let (ns, bs) = parse ls
  let subNs i = ns |> Seq.take i
  let rec f i bs' =
    let ns' = subNs i
    match List.ofSeq bs' with
    | [only] ->
      match play only ns' with
      | Some(n) -> n * Seq.last ns'
      | None    -> f (i+1) bs'
    | _      ->
      bs'
      |> Seq.filter (fun b -> play b ns' |> Option.isNone)
      |> f (i+1)
  f 0 bs
