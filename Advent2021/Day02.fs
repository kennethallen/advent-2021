module Day02

open System
open System.Text.RegularExpressions

type Dir = forward | down | up

let regex = Regex "^(forward|down|up) (\d+)$"
let parse s =
  let [d; n] = regex.Match s
  (Enum.Parse<Dir> d, int n)

let part1 cs =
  let (x, y) = cs
    |> Seq.map parse
    |> Seq.fold (fun (x, y) (d, n) ->
      match d with
        | Dir.forward -> (x+n, y)
        | Dir.down    -> (x, y-n)
        | Dir.up      -> (x, y+n)
    ) (0, 0)
  x * -y
