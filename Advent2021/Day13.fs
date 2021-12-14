module Day13
#nowarn "25"

open System
open System.Text.RegularExpressions

let regex = Regex "^fold along (x|y)=(\d+)$"
let parse ls =
  let split = Seq.findIndex String.IsNullOrEmpty ls
  let ps =
    ls
    |> Seq.take split
    |> Seq.map (fun s ->
      let [|x; y|] = s.Split(",")
      int x, int y)
    |> List.ofSeq
  let fs =
    ls
    |> Seq.skip (split+1)
    |> Seq.map (fun s ->
      let [a; n] =
        (regex.Match s).Groups
        |> Seq.tail
        |> Seq.map (fun g -> g.Value)
        |> List.ofSeq
      a[0], int n)
    |> List.ofSeq
  ps, fs

let folded (x, y) (a, n) =
  match a with
  | 'x' when x > n -> 2*n-x, y
  | 'y' when y > n -> x, 2*n-y
  | _              -> x, y

let part1 ls =
  let ps, fs = parse ls
  let f = Seq.head fs
  ps
  |> Seq.map (fun p -> folded p f)
  |> Seq.distinct
  |> Seq.length

let part2 ls =
  let ps, fs = parse ls
  let ps =
    ps
    |> Seq.map (fun p ->
      fs |> Seq.fold folded p)
    |> Set.ofSeq
  let maxX = ps |> Seq.map fst |> Seq.max
  let maxY = ps |> Seq.map snd |> Seq.max
  { 0 .. maxY }
  |> Seq.map (fun y ->
    { 0 .. maxX }
    |> Seq.map (fun x ->
      if Set.contains (x, y) ps then '#' else '.')
    |> Array.ofSeq
    |> String)
  |> String.concat "\n"
