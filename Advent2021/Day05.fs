module Day05
#nowarn "25"

open System.Text.RegularExpressions

type LineType = Horiz | Vert | ForwardDiag | BackDiag

type LineSeg = (int*int) * (int*int) * LineType

let regex = Regex @"^(\d+),(\d+) -> (\d+),(\d+)$"
let parseLine s =
  let [|x0; y0; x1; y1|] =
    (regex.Match s).Groups
    |> Seq.tail
    |> Seq.map (fun g -> int g.Value)
    |> Array.ofSeq
  let lType =
    if y0 = y1 then Horiz
    elif x0 = x1 then Vert
    elif y0-x0 = y1-x1 then ForwardDiag
    else BackDiag
  if x0 <= x1 then
    (x0, y0), (x1, y1), lType
  else
    (x1, y1), (x0, y0), lType

let hasCollinear ((ax0, ay0), (ax1, ay1), at) x y =
  assert ((ax0 = ax1 && ax0 = x) || (ay1-ay0)*(x-ax0) = (y-ay0)*(ax1-ax0))
  let betweenInc a b c = a <= b && b <= c
  match at with
  | Vert -> betweenInc (min ay0 ay1) y (max ay0 ay1)
  | _    -> betweenInc ax0 x ax1

let intersectTypesOrdered a b =
  let ((ax0, ay0), (ax1, ay1), at), ((bx0, by0), (bx1, by1), bt) = a, b
  let considerPoint x y =
    if hasCollinear a x y && hasCollinear b x y then
      Seq.singleton (x, y)
    else
      Seq.empty
  match at, bt with
  | Horiz, Horiz when ay0 = by0 ->
    { max ax0 bx0 .. min ax1 bx1 } |> Seq.map (fun x -> x, ay0)
  | Vert, Vert when ax0 = bx0 ->
    { max (min ay0 ay1) (min by0 by1) .. min (max ay0 ay1) (max by0 by1) } |> Seq.map (fun y -> ax0, y)
  | Horiz, Vert -> considerPoint bx0 ay0
  | ForwardDiag, ForwardDiag when ay0-ax0 = by0-bx0 ->
    { max ax0 bx0 .. min ax1 bx1 } |> Seq.map (fun x -> x, x + (ay0-ax0))
  | Horiz, ForwardDiag -> considerPoint (ay0 - (by0-bx0)) ay0
  | Vert, ForwardDiag -> considerPoint ax0 (ax0 + (by0-bx0))
  | BackDiag, BackDiag when ay0+ax0 = by0+bx0 ->
    { max ax0 bx0 .. min ax1 bx1 } |> Seq.map (fun x -> x, (ay0+ax0) - x)
  | Horiz, BackDiag -> considerPoint ((by0+bx0) - ay0) ay0
  | Vert, BackDiag -> considerPoint ax0 ((by0+bx0) - ax0)
  | ForwardDiag, BackDiag ->
    let aIntercept = ay0-ax0
    let doubX = (by0+bx0) - aIntercept
    if doubX % 2 = 0 then
      let x = doubX/2
      considerPoint x (x + aIntercept)
    else
      Seq.empty
  | _, _ -> Seq.empty

let intersect l0 l1 =
  let (_, _, t0), (_, _, t1) = l0, l1
  if t0 <= t1 then intersectTypesOrdered l0 l1
  else             intersectTypesOrdered l1 l0

let rec intersectAll = function
  | []      -> Seq.empty
  | l :: ls -> Seq.append (ls |> Seq.collect (intersect l)) (intersectAll ls)

let run diags =
  Seq.map parseLine
  >> Seq.filter (fun (_, _, t) -> diags || t = Horiz || t = Vert)
  >> List.ofSeq
  >> intersectAll
  >> Seq.distinct
  >> Seq.length

let part1 : string seq -> int = run false
let part2 : string seq -> int = run true
