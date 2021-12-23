module Day21
#nowarn "25"

open System.Text.RegularExpressions

type Die<'a> = {
  ThriceFn: 'a -> (int * uint64) list * 'a
  State: 'a
  RollCount: int
}

module Die =
  let rollThrice die =
    let roll, state = die.ThriceFn die.State
    roll, {die with State=state; RollCount=die.RollCount+3}

  let countUp nSides first = {
    ThriceFn = fun n ->
      let step n =
        if n = nSides then nSides, 1 else n, n+1
      let r0, n = step n
      let r1, n = step n
      let r2, n = step n
      [r0+r1+r2, 1UL], n
    State = first
    RollCount = 0}

  let quantum123 = {
    ThriceFn = fun () ->
      let rolls = [
        3, 1UL
        4, 3UL
        5, 6UL
        6, 7UL
        7, 6UL
        8, 3UL
        9, 1UL
      ]
      rolls, ()
    State = ()
    RollCount = 0
  }

type Game<'a> = {
  WinScore: int
  Wins: uint64 array
  LosingScores: Map<int, uint64>
  Die: Die<'a>
  LiveStates: Map<(int * int) array, uint64>
  NextPlayer: int
}

module Game =
  let create die winScore startPoss = {
    WinScore = winScore
    Wins = Array.replicate (Array.length startPoss) 0UL
    LosingScores = Map.empty
    Die = die
    LiveStates = Map.ofList [Array.map (fun pos -> pos, 0) startPoss, 1UL]
    NextPlayer = 0
  }

let regex = Regex "^Player (\d+) starting position: (\d+)$"
let parse: string seq -> int array =
  Seq.map (fun l ->
    let [id; pos] =
      (regex.Match l).Groups
      |> Seq.tail
      |> Seq.map (fun g -> int g.Value)
      |> List.ofSeq
    id, pos)
  >> Seq.sortBy (fst)
  >> Seq.map snd
  >> Array.ofSeq

let stepPlayer pIdx game =
  let rolls, die = Die.rollThrice game.Die
  let states =
    game.LiveStates
    |> Map.toSeq
    |> Seq.collect (fun (state, n0: uint64) ->
      rolls
      |> Seq.map (fun (roll, n1: uint64) -> 
        let pos, score = Array.item pIdx state
        let pos =
          match (pos + roll) % 10 with
          | 0 -> 10
          | n -> n
        Array.updateAt pIdx (pos, score+pos) state, n0*n1))
    |> Counter.ofCountSeq

  let isWin = Array.item pIdx >> snd >> (<=) game.WinScore
  let wins =
    states
    |> Map.toSeq
    |> Seq.filter (fst >> isWin)

  {game with
    Die = die
    LiveStates =
      states |> Map.filter (fun state _ -> not (isWin state))
    Wins =
      game.Wins |> Array.updateAt pIdx (Seq.sumBy snd wins + Array.item pIdx game.Wins)
    LosingScores =
      wins
      |> Seq.collect (fun (state, n) ->
        state
        |> Seq.removeAt pIdx
        |> Seq.map (fun (_, score) -> score, n))
      |> Counter.ofCountSeq
      |> Counter.merge game.LosingScores}

let rec play game =
  if Map.isEmpty game.LiveStates then
    game
  else
    play {
      stepPlayer game.NextPlayer game with
        NextPlayer = (game.NextPlayer+1) % (Array.length game.Wins)}

let part1 ls =
  let game = play (
    Game.create
      (Die.countUp 100 1)
      1000
      (parse ls))
  game.Die.RollCount * (game.LosingScores |> Map.toSeq |> Seq.exactlyOne |> fst)
  
let part2 ls =
  let game = play (
    Game.create
      Die.quantum123
      21
      (parse ls))
  Array.max game.Wins
