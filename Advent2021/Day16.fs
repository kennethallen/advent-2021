module Day16
#nowarn "25"
#nowarn "104"

open System

type Op =
  | Sum         = 0
  | Product     = 1
  | Min         = 2
  | Max         = 3
  | GreaterThan = 5
  | LessThan    = 6
  | Equals      = 7

type Packet =
  | Literal  of uint64 * uint64
  | Operator of uint64 * Op * Packet list

let parseHex : string seq -> bool list =
  Seq.collect (
    Seq.collect (fun c ->
      let n = Convert.ToInt32(Array.singleton c |> String, 16)
      { 3 .. -1 .. 0 }
      |> Seq.map (fun m -> n &&& (1 <<< m) <> 0)))
  >> List.ofSeq

let readNum =
  Seq.fold (fun s b -> (s <<< 1) + if b then 1UL else 0UL) 0UL

let split i bs =
  Seq.take i bs, List.skip i bs

let readChunkedNum =
  let rec f a (c::bs) =
    let ns, bs = split 4 bs
    let a = (a <<< 4) + readNum ns
    if c then
      f a bs
    else
      a, bs
  f 0UL

let rec readPacket bs =
  let readSubpackets (lt::bs) =
    if lt then
      let l, bs = split 11 bs
      let l = readNum l
      [1UL .. l] |> List.mapFold (fun bs _ -> readPacket bs) bs
    else
      let l, bs = split 15 bs
      let l = readNum l
      let ps, bs = split (int l) bs
      let ps = List.unfold (fun ps ->
        if Seq.isEmpty ps then
          None
        else
          Some (readPacket ps)) (List.ofSeq ps)
      ps, bs

  let vers, bs = split 3 bs
  let vers = readNum vers
  let typeId, bs = split 3 bs
  match readNum typeId with
  | 4UL    ->
    let n, bs = readChunkedNum bs
    Literal (vers, n), bs
  | typeId ->
    let ps, bs = readSubpackets bs
    Operator (vers, enum (int typeId), ps), bs

let rec sumVersions = function
  | Literal (vers, _)      -> vers
  | Operator (vers, _, ps) ->
    vers + (ps |> Seq.map sumVersions |> Seq.sum)

let part1 : string seq -> uint64 =
  parseHex >> readPacket >> fst >> sumVersions

let testPair f [p0; p1] = if f p0 p1 then 1UL else 0UL

let rec eval = function
  | Literal (_, n)       -> n
  | Operator (_, op, ps) ->
    ps
    |> List.map eval
    |> match op with
       | Op.Sum         -> List.sum
       | Op.Product     -> List.fold (*) 1UL
       | Op.Min         -> List.min
       | Op.Max         -> List.max
       | Op.GreaterThan -> testPair (>)
       | Op.LessThan    -> testPair (<)
       | Op.Equals      -> testPair (=)

let part2 : string seq -> uint64 =
  parseHex >> readPacket >> fst >> eval
