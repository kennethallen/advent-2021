module Day22

open System
open System.Text.RegularExpressions

type Node<'a> =
  | State of bool
  | Children of 'a list

type Octree = {
  X: int64
  Y: int64
  Z: int64
  Dim: int64
  Node: Node<Octree>
}

type RPrism = {
  X0: int64
  X1: int64
  Y0: int64
  Y1: int64
  Z0: int64
  Z1: int64
}

module Octree =
  let makeChildren o =
    match o.Node with
    | Children cs -> cs
    | State s ->
      let halfDim = o.Dim >>> 1
      [o.Z; o.Z + halfDim]
      |> Seq.allPairs [o.Y; o.Y + halfDim]
      |> Seq.allPairs [o.X; o.X + halfDim]
      |> Seq.map (fun (x, (y, z)) -> {X=x; Y=y; Z=z; Dim=halfDim; Node=State s})
      |> List.ofSeq

  let withinPrism p (o: Octree) =
    p.X0 <= o.X && o.X+o.Dim <= p.X1
    && p.Y0 <= o.Y && o.Y+o.Dim <= p.Y1
    && p.Z0 <= o.Z && o.Z+o.Dim <= p.Z1

  let disjointFromPrism p (o: Octree) =
    p.X1 < o.X || o.X+o.Dim < p.X0
    || p.Y1 < o.Y || o.Y+o.Dim < p.Y0
    || p.Z1 < o.Z || o.Z+o.Dim < p.Z0

  let rec apply s p o =
    if disjointFromPrism p o then
      o
    elif withinPrism p o then
      {o with Node=State s}
    else
      {o with Node=Children (makeChildren o |> List.map (apply s p))}

  let rec clip p o =
    if disjointFromPrism p o then
      {o with Node=State false}
    elif withinPrism p o then
      o
    else
      {o with Node=Children (makeChildren o |> List.map (clip p))}

  let rec card o =
    match o.Node with
    | State true -> o.Dim * o.Dim * o.Dim
    | State false -> 0L
    | Children cs -> cs |> List.sumBy card

let regex = Regex @"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$"
let parse: string seq -> (bool * RPrism) seq =
  Seq.map (fun l ->
    let state::coords =
      (regex.Match l).Groups
      |> Seq.tail
      |> Seq.map (fun g -> g.Value)
      |> List.ofSeq
    let [x0; x1; y0; y1; z0; z1] = coords |> List.map int64
    state = "on", {X0=x0; X1=x1; Y0=y0; Y1=y1; Z0=z0; Z1=z1})

let step octree (state, prism) = Octree.apply state prism octree

let po2Ceil n =
  let rec f p = if p >= n then p else f (p <<< 1)
  f 1L
  
let part1 ls =
  let bounds = {X0= -50L; X1=50L; Y0= -50L; Y1=50L; Z0= -50L; Z1=50L}
  let zones = parse ls
  let maxCoord =
    zones
    |> Seq.collect (fun (_, p) -> [p.X0; p.X1; p.Y0; p.Y1; p.Z0; p.X1])
    |> Seq.map Math.Abs
    |> Seq.max
    |> (+) 1L
    |> po2Ceil
  let octree = {
    X = -maxCoord
    Y = -maxCoord
    Z = -maxCoord
    Dim = 2L*maxCoord
    Node = State false}
  zones
  |> Seq.fold step octree
  |> Octree.clip bounds
  |> Octree.card
