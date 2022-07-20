module Day19
#nowarn "25"

let rec parse ls =
  let ls = Seq.tail ls
  let i = Seq.tryFindIndex (fun (l : string) -> l.Length = 0) ls
  let s =
    match i with
    | Some i -> Seq.take i ls
    | None -> ls
    |> Seq.map (fun s ->
      let [|x; y; z|] : int array = s.Split(",") |> Array.map int
      x, y, z)
    |> List.ofSeq
  let rest =
    match i with
    | Some i -> parse (Seq.skip (i+1) ls)
    | None -> []
  s::rest

let facings =
  let evenRefs = [
    id
    fun (x, y, z) -> -x, -y, z
    fun (x, y, z) -> -x, y, -z
    fun (x, y, z) -> x, -y, -z
  ]
  let evenNegs = [
    id
    fun (x, y, z) -> z, x, y
    fun (x, y, z) -> y, z, x
  ]
  let oddRefs = [
    fun (x, y, z) -> -x, -y, -z
    fun (x, y, z) -> -x, y, z
    fun (x, y, z) -> x, -y, z
    fun (x, y, z) -> x, y, -z
  ]
  let oddNegs = [
    fun (x, y, z) -> x, z, y
    fun (x, y, z) -> z, y, x
    fun (x, y, z) -> y, x, z
  ]
  Seq.append
    (evenRefs |> Seq.collect (fun r -> evenNegs |> Seq.map ((>>) r)))
    (oddRefs |> Seq.collect (fun r -> oddNegs |> Seq.map ((>>) r)))
  |> List.ofSeq
  
let dist (x0, y0, z0) (x1, y1, z1) = abs (x0-x1) + abs (y0-y1) + abs (z0-z1)
let (+) (x0, y0, z0) (x1, y1, z1) = x0+x1, y0+y1, z0+z1
let (-) (x0, y0, z0) (x1, y1, z1) = x0-x1, y0-y1, z0-z1

let tryOverlap s sRoot =
  facings
  |> Seq.collect (fun f ->
    s
    |> Seq.map f
    |> Seq.allPairs sRoot
    |> Seq.map (fun (p, pRoot) -> p - pRoot)
    |> Seq.countBy id
    |> Seq.filter (snd >> (<=) 12)
    |> Seq.map (fun (offset, _) -> f >> (+) offset))
  |> Seq.tryExactlyOne

let overlapAll (s::ss) =
  let rec inner rootFns lastSuccs =
    function
    | [] -> rootFns
    | lastFails ->
      let folder (rootFns, succs, fails) s =
        lastSuccs
        |> Seq.tryPick (fun sRoot ->
          tryOverlap s sRoot
          |> Option.map (fun f -> f >> Map.find sRoot rootFns))
        |> function
          | None -> rootFns, succs, s::fails
          | Some f ->
            Map.add s f rootFns, s::succs, fails
      let rootFns, prospects, rest = List.fold folder (rootFns, [], []) lastFails
      inner rootFns prospects rest

  inner (Map.ofList [s, id]) [s] ss

let part1 : string seq -> int =
  parse
  >> overlapAll
  >> Map.toSeq
  >> Seq.collect (fun (s, f) -> Seq.map f s)
  >> Seq.distinct
  >> Seq.length

let part2 : string seq -> int =
  let rec pairs =
    function
    | [] -> Seq.empty
    | e::es ->
      Seq.append
        (Seq.map (fun e1 -> e, e1) es)
        (pairs es)
  parse
  >> overlapAll
  >> Map.toList
  >> List.map (fun (_, f) -> f (0, 0, 0))
  >> pairs
  >> Seq.map (fun (p0, p1) -> dist p0 p1)
  >> Seq.max
