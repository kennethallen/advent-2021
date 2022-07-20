module Day23
#nowarn "25"

type private Pos =
  | Hall of int
  | Room of int * int

type private Pod = int * Pos

type private State = Pod array

let private charToId c = int c - int 'A'

let private parse : string seq -> State =
  Seq.skip 2
  >> Seq.take 2
  >> Seq.mapi (fun i r -> i, r)
  >> Seq.collect (fun (i, r) ->
    seq { 0 .. 3 }
    |> Seq.map (fun n -> charToId r[3 + 2*n], Pos.Room (i, n)))
  >> Seq.toArray

let private heur _ _ = 0

let private energyCost = function
| 0 -> 1
| 1 -> 10
| 2 -> 100
| 3 -> 1000

let private paths state : (int * State) seq =
  state
  |> Seq.collect (fun (id, pos) ->
    match pos with
    | Room (y, x) ->
      if x = id && (y = 1 || true) then
        []
      else
        []
    | Hall i -> []
  )

let part1 : string seq -> int =
  parse
  >> AStar.aStar paths heur [||]
  >> fst
