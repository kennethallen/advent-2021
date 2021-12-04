module Day03

open System

let parseBin s = Convert.ToInt32(s, 2)

let bitCounts (ls: seq<string>) =
  let f = Seq.map2 (fun count char -> count + ([|char|] |> String |> parseBin))
  let init = Seq.head ls |> Seq.map (fun _ -> 0)
  Seq.fold f init ls

let majBits ls =
  bitCounts ls
  |> Seq.map (fun n -> 2*n >= Seq.length ls)

let part1 ls =
  let f =
    Seq.map (fun b -> if b then '1' else '0')
    >> Array.ofSeq
    >> String
    >> parseBin
  let majs = majBits ls
  (majs |> f) * (majs |> Seq.map not |> f)

let part2 ls =
  let rec f (rems: seq<string>) i maj =
    match rems |> List.ofSeq with
    | [only] -> parseBin only
    | _      ->
      let targetBit = majBits rems |> Seq.item i |> (=) maj
      let pred (l: string) = (l.[i] = '1') = targetBit
      f (rems |> Seq.filter pred) (i+1) maj
  (f ls 0 true) * (f ls 1 false)