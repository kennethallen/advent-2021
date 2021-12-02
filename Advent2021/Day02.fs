module Day02

open System.Text.RegularExpressions

type Dir = Forward | Down | Up
let parseDir s = match s with
                 | "forward" -> Forward
                 | "down"    -> Down
                 | "up"      -> Up

let regex = Regex "^(forward|down|up) (\d+)$"
let parseCommand s =
  let m = (regex.Match s).Groups |> List.ofSeq
  let [_; di; n] = (regex.Match s).Groups |> List.ofSeq
  (parseDir di.Value, int n.Value)

let part1 cs =
  let f (x, de) (di, n) =
    match di with
    | Forward -> (x+n, de)
    | Down    -> (x, de+n)
    | Up      -> (x, de-n)
  let (x, de) = cs
             |> Seq.map parseCommand
             |> Seq.fold f (0, 0)
  x * de

let part2 cs =
 let f (x, de, a) (di, n) =
   match di with
   | Forward -> (x+n, de + a*n, a)
   | Down    -> (x, de, a+n)
   | Up      -> (x, de, a-n)
 let (x, de, _) = cs
               |> Seq.map parseCommand
               |> Seq.fold f (0, 0, 0)
 x * de
