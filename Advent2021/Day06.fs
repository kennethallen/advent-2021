module Day06

let mergeCountMap (m: Map<'a, uint64>) (k, (a: uint64)) =
  Map.change k (fun n -> Some (defaultArg n 0UL |> (+) a)) m
let countMapOfSeq = Seq.fold mergeCountMap Map.empty

let sim days (ls: seq<string>) =
  let init =
    (Seq.head ls).Split ","
    |> Seq.map int
    |> Seq.map (fun n -> (n, 1UL))
    |> countMapOfSeq
  let step m =
    m
    |> Map.toSeq
    |> Seq.collect (fun (f, n) ->
      match f with
      | 0 -> [6, n; 8, n]
      | f -> [f-1, n])
    |> countMapOfSeq
  Seq.init days (fun _ -> step)
  |> Seq.fold (|>) init
  |> Map.values
  |> Seq.sum

let part1: (seq<string> -> uint64) = sim 80
let part2: (seq<string> -> uint64) = sim 256
