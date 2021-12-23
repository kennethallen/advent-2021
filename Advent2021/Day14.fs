module Day14
#nowarn "25"

open System.Text.RegularExpressions

let private regex = Regex @"^(.)(.) -> (.)$"
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

let rec private countTail rules memo e0 e1 =
  function
  | 0 -> Map.ofList [e1, 1UL], memo
  | steps ->
    match Map.tryFind (e0, e1, steps) memo with
    | Some m -> m, memo
    | None   ->
      let en = Map.find (e0, e1) rules
      let m0, memo = countTail rules memo e0 en (steps-1)
      let m1, memo = countTail rules memo en e1 (steps-1)
      let m = Counter.merge m0 m1
      m, Map.add (e0, e1, steps) m memo

let private count rules steps es =
  es
  |> Seq.pairwise
  |> Seq.fold (fun (c0, memo) (e0, e1) ->
    let c1, memo = countTail rules memo e0 e1 steps
    Counter.merge c0 c1, memo) (Map.empty, Map.empty)
  |> fst
  |> Counter.add (Seq.head es)

let private run n ls =
  let temp, rules = parse ls
  let counts = count rules n temp |> Map.values
  Seq.max counts - Seq.min counts

let part1 : string seq -> uint64 = run 10
let part2 : string seq -> uint64 = run 40
