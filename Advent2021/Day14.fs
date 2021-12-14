module Day14
#nowarn "25"

open System.Text.RegularExpressions

let regex = Regex "^(.)(.) -> (.)$"
let parse ls =
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

let rec step rules es =
  es
  |> Seq.pairwise
  |> Seq.collect (fun (e0, e1) ->
    [Map.find (e0, e1) rules; e1])
  |> Seq.append (Seq.truncate 1 es)

let run n ls =
  let temp, rules = parse ls
  let counts =
    Seq.unfold (step rules >> (fun x -> Some (x, x))) temp
    |> Seq.item (n-1)
    |> Seq.fold (fun m e ->
      Map.change e (Option.map ((+) 1UL) >> Option.orElse (Some 1UL)) m)
      Map.empty
    |> Map.values
  (Seq.max counts) - (Seq.min counts)

let part1 : string seq -> uint64 = run 10
let part2 : string seq -> uint64 = run 40
