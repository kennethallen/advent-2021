module Day08

let solve (l : string) =
  let [|s0; s1|] = l.Split(" | ")
  let refs = s0.Split(" ")
  let refSized n = refs |> Seq.filter (fun r -> r.Length = n) |> Seq.exactlyOne
  let refOne = refSized 2 
  let refFour = refSized 4
  let countShared r s =
    r |> Seq.filter (fun c -> Seq.contains c s) |> Seq.length
  s1.Split(" ")
  |> Seq.map (fun s ->
    match s.Length with
    | 2                                -> 1
    | 3                                -> 7
    | 4                                -> 4
    | 5 when countShared refOne s = 2  -> 3
    | 5 when countShared refFour s = 2 -> 2
    | 5                                -> 5
    | 6 when countShared refFour s = 4 -> 9
    | 6 when countShared refOne s = 2  -> 0
    | 6                                -> 6
    | 7                                -> 8)
  |> List.ofSeq

let digitsToNum =
  let rec f =
    function
    | []    -> 0
    | d::ds -> d + 10*(f ds)
  List.rev >> f

let part1 : string seq -> int =
  Seq.collect solve
  >> Seq.filter (fun d -> Array.contains d [|1; 4; 7; 8|])
  >> Seq.length
let part2 : string seq -> int =
  Seq.map solve
  >> Seq.map digitsToNum
  >> Seq.sum
