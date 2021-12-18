module Day18

type SFNum =
  | Pair of SFNum * SFNum
  | Num of int

let rec parse (s: string) =
  let rec findComma (s: string) i d =
    match s[i], d with
    | '[', d -> findComma s (i + 1) (d + 1)
    | ']', d -> findComma s (i + 1) (d - 1)
    | ',', 0 -> i
    | _, d -> findComma s (i + 1) d

  match s[0] with
  | '[' ->
    let split = findComma s 1 0
    Pair(parse s[1 .. split - 1], parse s[split + 1 .. s.Length - 2])
  | _ -> Num(int s)

let rec explodingLeft l =
  function
  | Num x -> Num(x + l)
  | Pair (n0, n1) -> Pair(n0, explodingLeft l n1)

let rec explodingRight r =
  function
  | Num x -> Num(x + r)
  | Pair (n0, n1) -> Pair(explodingRight r n0, n1)

let doExplode n =
  let rec f =
    function
    | Num x, _ -> Ok(Num x)
    | Pair (Num x0, Num x1), d when d >= 4 -> Error(Num 0, Some x0, Some x1)
    | Pair (n0, n1), d ->
      match f (n0, d + 1) with
      | Ok n0 ->
        match f (n1, d + 1) with
        | Ok n1 -> Ok(Pair(n0, n1))
        | Error (n1, None, r) -> Error(Pair(n0, n1), None, r)
        | Error (n1, Some l, r) -> Error(Pair(explodingLeft l n0, n1), None, r)
      | Error (n0, l, None) -> Error(Pair(n0, n1), l, None)
      | Error (n0, l, Some r) -> Error(Pair(n0, explodingRight r n1), l, None)

  f (n, 0) |> Result.mapError (fun (n, _, _) -> n)

let rec doSplit =
  function
  | Pair (n0, n1) ->
    match doSplit n0 with
    | Ok n0 ->
      match doSplit n1 with
      | Ok n1 -> Ok(Pair(n0, n1))
      | Error n1 -> Error(Pair(n0, n1))
    | Error n0 -> Error(Pair(n0, n1))
  | Num x when x < 10 -> Ok(Num x)
  | Num x -> Error(Pair(Num(x / 2), Num((x + 1) / 2)))

let rec reduce n =
  match doExplode n |> Result.bind doSplit with
  | Ok n -> n
  | Error n -> reduce n

let rec magnitude =
  function
  | Num x -> x
  | Pair (n0, n1) -> 3 * (magnitude n0) + 2 * (magnitude n1)

let (+) a b = reduce (Pair(a, b))

let part1: string seq -> int = Seq.map parse >> Seq.reduce (+) >> magnitude

let part2 ls =
  let ns = Seq.map parse ls |> Seq.cache

  ns
  |> Seq.collect (fun n ->
    ns
    |> Seq.except (Seq.singleton n)
    |> Seq.map ((+) n))
  |> Seq.map magnitude
  |> Seq.max
