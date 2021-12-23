module Day07

let calc costFn (ls : string seq) =
  let cs = ls |> Seq.collect (fun l -> l.Split(",")) |> Seq.map int |> List.ofSeq
  { List.min cs .. List.max cs }
  |> Seq.map (fun x ->
    cs |> Seq.sumBy ((+) -x >> abs >> costFn))
  |> Seq.min

let part1 : string seq -> int = calc id
let part2 : string seq -> int = calc (fun d -> d * (d+1) / 2)
