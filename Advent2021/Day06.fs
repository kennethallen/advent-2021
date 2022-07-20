module Day06

let sim days (ls : string seq) =
  let init =
    (Seq.head ls).Split ","
    |> Seq.map int
    |> Counter.ofSeq
  let step =
    Map.toSeq
    >> Seq.collect (fun (f, n) ->
      match f with
      | 0 -> [6, n; 8, n]
      | f -> [f-1, n])
    >> Counter.ofCountSeq
  init
  |> Seq.unfold (step >> (fun m -> Some (m, m)))
  |> Seq.item (days-1)
  |> Map.values
  |> Seq.sum

let part1 : string seq -> uint64 = sim 80
let part2 : string seq -> uint64 = sim 256
