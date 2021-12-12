module Day12
#nowarn "25"

open System

let isLarge (c : string) =
  Char.IsUpper c[0]

let withDiEdge c0 c1 =
  Map.change c0 (fun s -> defaultArg s Set.empty |> Set.add c1 |> Some)

let withEdge c0 c1 =
  withDiEdge c0 c1 >> withDiEdge c1 c0
    
let build : string seq -> Map<string, Set<string>> =
  Seq.map (fun s ->
    let [|s0; s1|] = s.Split("-")
    s0, s1)
  >> Seq.fold (fun m (c0, c1) -> withEdge c0 c1 m) Map.empty

let countPaths idb m =
  let rec f sv db n =
    if n = "end" then 1
    else
      let sv' =
        if isLarge n then sv
        else              Set.add n sv
      let dys, dns =
        Map.find n m
        |> Set.partition (fun d -> Set.contains d sv)
      let ddbs =
        if db then
          dys |> Seq.except ["start"] |> Seq.map (f sv' false)
        else Seq.empty
      Seq.append ddbs (dns |> Seq.map (f sv' db))
      |> Seq.sum
  f Set.empty idb "start"

let part1 : string seq -> int = build >> countPaths false
let part2 : string seq -> int = build >> countPaths true
