module Day14
#nowarn "25"

open System.Text.RegularExpressions

let private regex = Regex "^(.)(.) -> (.)$"
let private parse ls =
  let temp = Seq.head ls |> List.ofSeq
  let rules =
    ls
    |> Seq.skip 2
    |> Seq.map (fun s ->
      let [a; b; c] =
        (regex.Match s).Groups
        |> Seq.tail
        |> Seq.map (fun g -> g.Value[0])
        |> List.ofSeq
      (a, b), c)
    |> Map.ofSeq
  temp, rules

let private countAdd (n : uint64) k =
  Map.change k (Option.map ((+) n) >> Option.orElse (Some n))

let private mergeCountMaps m0 m1 =
  Map.fold (fun m k v -> countAdd v k m) m0 m1

let rec private countTail rules memo steps e0 e1 =
  match steps with
  | 0 -> Map.ofList [e1, 1UL], memo
  | n ->
    let en = Map.find (e0, e1) rules
    let memoDescend memo ea eb =
      match Map.tryFind (ea, eb, n-1) memo with
      | Some x -> x, memo
      | None   ->
        let x, memo = countTail rules memo (n-1) ea eb
        x, Map.add (ea, eb, n-1) x memo
    let m0, memo = memoDescend memo e0 en
    let m1, memo = memoDescend memo en e1
    mergeCountMaps m0 m1, memo

let private count rules steps es =
  es
  |> Seq.pairwise
  |> Seq.fold (fun (c0, memo) (e0, e1) ->
    let c1, memo = countTail rules memo steps e0 e1
    mergeCountMaps c0 c1, memo) (Map.empty, Map.empty)
  |> fst
  |> countAdd 1UL (Seq.head es)

let private run n ls =
  let temp, rules = parse ls
  let counts = count rules n temp |> Map.values
  (Seq.max counts) - (Seq.min counts)

let part1 : string seq -> uint64 = run 10
let part2 : string seq -> uint64 = run 40
