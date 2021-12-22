module Day22

open System
open System.Text.RegularExpressions

type RPrism = {
  x0: int64
  x1: int64
  y0: int64
  y1: int64
  z0: int64
  z1: int64
}

module RPrism =
  let empty p =
    p.x0 > p.x1 || p.y0 > p.y1 || p.z0 > p.z1

  let tryMergeNeighbors p0 p1 =
    if p0.x0 = p1.x0 && p0.x1 = p1.x1 then
      if p0.y0 = p1.y0 && p0.y1 = p1.y1 then
        if p1.z0 = p0.z1+1L then
          Some {p0 with z1=p1.z1}
        elif p0.z0 = p1.z1+1L then
          Some {p1 with z1=p0.z1}
        else
          None
      elif p0.z0 = p1.z0 && p0.z1 = p1.z1 then
        if p1.y0 = p0.y1+1L then
          Some {p0 with y1=p1.y1}
        elif p0.y0 = p1.y1+1L then
          Some {p1 with y1=p0.y1}
        else
          None
      else None
    elif p0.y0 = p1.y0 && p0.y1 = p1.y1 && p0.z0 = p1.z0 && p0.z1 = p1.z1 then
      if p1.x0 = p0.x1+1L then
        Some {p0 with x1=p1.x1}
      elif p0.x0 = p1.x1+1L then
        Some {p1 with x1=p0.x1}
      else
        None
    else None

  let consolidate ps =
    let rec f ps =
      function
      | [] -> ps
      | next::rest ->
        match List.tryPick (fun p -> tryMergeNeighbors next p |> Option.map (fun c -> c, p)) ps with
        | None -> f (next::ps) rest
        | Some (c, p) -> f (List.except [p] ps) (c::ps)
    f [] ps

  let carveOut m p =
    if m.x0 > p.x1 && p.x0 > m.x1 && m.y0 > p.y1 && p.y0 > m.y1 && m.z0 > p.z1 && p.z0 > m.z1 then
      [p]
    else
      seq {p.z0,m.z0-1L; m.z0,m.z1; m.z1+1L,p.z1}
      |> Seq.allPairs (seq {p.y0,m.y0-1L; m.y0,m.y1; m.y1+1L,p.y1})
      |> Seq.allPairs (seq {p.x0,m.x0-1L; m.x0,m.x1; m.x1+1L,p.x1})
      |> Seq.removeAt 4 // Center section, hole left by mask
      |> Seq.map (fun ((x0, x1), ((y0, y1), (z0, z1))) -> {x0=x0; x1=x1; y0=y0; y1=y1; z0=z0; z1=z1})
      |> Seq.filter (empty >> not)
      |> List.ofSeq
      |> consolidate
      
  let clip m p =
    let p = {
      x0 = Math.Max(p.x0, m.x0)
      x1 = Math.Min(p.x1, m.x1)
      y0 = Math.Max(p.y0, m.y0)
      y1 = Math.Min(p.y1, m.y1)
      z0 = Math.Max(p.z0, m.z0)
      z1 = Math.Min(p.z1, m.z1)
    }
    if empty p then None else Some p

  let card p =
    (p.x1+1L-p.x0) * (p.y1+1L-p.y0) * (p.z1+1L-p.z0)

let regex = Regex @"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$"
let parse: string seq -> (bool * RPrism) seq =
  Seq.map (fun l ->
    let state::coords =
      (regex.Match l).Groups
      |> Seq.tail
      |> Seq.map (fun g -> g.Value)
      |> List.ofSeq
    let [x0; x1; y0; y1; z0; z1] = coords |> List.map int64
    state = "on", {x0=x0; x1=x1; y0=y0; y1=y1; z0=z0; z1=z1})

let step zones (state, newPrism) =
  let zones = zones |> List.collect (RPrism.carveOut newPrism)
  if state then newPrism::zones else zones
  
let part1: string seq -> int64 =
  let bounds = {x0= -50L; x1=50L; y0= -50L; y1=50L; z0= -50L; z1=50L}
  parse
  >> Seq.fold step []
  >> Seq.choose (RPrism.clip bounds)
  >> Seq.map RPrism.card
  >> Seq.sum
