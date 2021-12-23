module Day22
#nowarn "25"

open System.Text.RegularExpressions

type private RPrism = {
  Los: int64 array
  His: int64 array
}

module private RPrism =
  let empty p = Array.exists2 (>) p.Los p.His

  let tryNonempty = Some >> Option.filter (empty >> not)

  let tryClip m p =
    tryNonempty {
      Los = Array.map2 max p.Los m.Los
      His = Array.map2 min p.His m.His
    }

  let card p =
    Seq.map2 (-) p.His p.Los
    |> Seq.map ((+) 1L)
    |> Seq.reduce (*)

  let trySlice dim lo hi p =
    if hi < p.Los[dim] || lo > p.His[dim] then
      None
    else
      Some
        {p with
          Los = Array.updateAt dim lo p.Los
          His = Array.updateAt dim hi p.His}

type private Zone = {
  State: bool
  Idx: int
  Prism: RPrism
}

module private Zone =
  let card z =
    if z.State then
      RPrism.card z.Prism
    else
      0

  let trySlice dim lo hi z =
    RPrism.trySlice dim lo hi z.Prism
    |> Option.map (fun p -> {z with Prism=p})

  let tryClip m z =
    RPrism.tryClip m z.Prism
    |> Option.map (fun p -> {z with Prism=p})

let private regex = Regex @"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$"
let private parse: string seq -> Zone list =
  Seq.mapi (fun i l ->
    let state::coords =
      (regex.Match l).Groups
      |> Seq.tail
      |> Seq.map (fun g -> g.Value)
      |> List.ofSeq
    let coords = coords |> List.map int64
    let prism = {
      Los = [|0..2..4|] |> Array.map (fun i -> List.item i coords)
      His = [|1..2..5|] |> Array.map (fun i -> List.item i coords)
    }
    {State=state = "on"; Idx=i; Prism=prism})
  >> List.ofSeq

let private sliceUp dim zs =
  Seq.append
    (Seq.map (fun z -> z.Prism.Los[dim]) zs)
    (Seq.map (fun z -> z.Prism.His[dim]+1L) zs)
  |> Seq.distinct
  |> Seq.sort
  |> Seq.pairwise
  |> Seq.map (fun (lo, hi) ->
    zs |> List.choose (Zone.trySlice dim lo (hi-1L)))
  |> List.ofSeq

let private card zs =
  let rec f dims zs =
    match dims with
    | [] ->
      assert (zs |> List.pairwise |> List.forall (fun (z0, z1) -> z0.Prism = z1.Prism))
      match zs with
      | [] -> 0L
      | zs -> Zone.card (zs |> List.maxBy (fun z -> z.Idx))
    | dim::dims ->
      sliceUp dim zs
      |> List.sumBy (f dims)
  f [0..2] zs
    
let part1: string seq -> int64 =
  let bounds = {Los=Array.replicate 3 -50; His=Array.replicate 3 50}
  parse
  >> List.choose (Zone.tryClip bounds)
  >> card

let part2: string seq -> int64 = parse >> card
