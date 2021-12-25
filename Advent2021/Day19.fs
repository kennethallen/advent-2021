module Day19
#nowarn "25"

let rec private parse ls =
  if Seq.isEmpty ls then
    []
  else
    let ls = Seq.tail ls
    let i =
      ls
      |> Seq.tryFindIndex (fun (l: string) -> l.Length = 0)
      |> Option.defaultWith (fun () -> Seq.length ls)
    let v =
      ls
      |> Seq.take i
      |> Seq.map (fun (s: string) ->
        let [|x; y; z|]: int array = s.Split(",") |> Array.map int
        x, y, z)
      |> List.ofSeq
    v :: parse (Seq.skip (i+1) ls)

let private facings =
  let evenRefs = [
    id
    fun (x, y, z) -> -x, -y, z
    fun (x, y, z) -> -x, y, -z
    fun (x, y, z) -> x, -y, -z
  ]
  let evenNegs = [
    id
    fun (x, y, z) -> z, x, y
    fun (x, y, z) -> y, z, x
  ]
  let oddRefs = [
    fun (x, y, z) -> -x, -y, -z
    fun (x, y, z) -> -x, y, z
    fun (x, y, z) -> x, -y, z
    fun (x, y, z) -> x, y, -z
  ]
  let oddNegs = [
    fun (x, y, z) -> x, z, y
    fun (x, y, z) -> z, y, x
    fun (x, y, z) -> y, x, z
  ]
  Seq.append
    (evenRefs |> Seq.collect (fun r -> evenNegs |> Seq.map ((>>) r)))
    (oddRefs |> Seq.collect (fun r -> oddNegs |> Seq.map ((>>) r)))
  |> List.ofSeq
  
let private dist (x0, y0, z0) (x1, y1, z1) = abs (x0-x1) + abs (y0-y1) + abs (z0-z1)
let private (+<) (x0, y0, z0) (x1, y1, z1) = x0+x1, y0+y1, z0+z1
let private (-<) (x0, y0, z0) (x1, y1, z1) = x0-x1, y0-y1, z0-z1

let private tryOverlap s sRoot =
  facings
  |> Seq.collect (fun f ->
    s
    |> Seq.map f
    |> Seq.allPairs sRoot
    |> Seq.map (fun (p, pRoot) -> p -< pRoot)
    |> Seq.countBy id
    |> Seq.filter (snd >> (<=) 12)
    |> Seq.map (fun (offset, _) -> f >> (+<) offset))
  |> Seq.tryExactlyOne

let private overlapAll (s::ss) =
  let rec inner rootFns lastSuccs =
    function
    | [] -> rootFns
    | lastFails ->
      let folder (rootFns, succs, fails) s =
        lastSuccs
        |> Seq.tryPick (fun sRoot ->
          tryOverlap s sRoot
          |> Option.map (fun f -> f >> Map.find sRoot rootFns))
        |> function
          | None -> rootFns, succs, s::fails
          | Some f ->
            Map.add s f rootFns, s::succs, fails
      let rootFns, prospects, rest = List.fold folder (rootFns, [], []) lastFails
      inner rootFns prospects rest

  inner (Map.ofList [s, id]) [s] ss
  
open MathNet.Numerics.LinearAlgebra

type private Scanner = Vector<int> list

let rec private parse1 ls: Scanner list =
  if Seq.isEmpty ls then
    []
  else
    let ls = Seq.tail ls
    let n =
      ls
      |> Seq.tryFindIndex (fun (l: string) -> l.Length = 0)
      |> Option.defaultWith (fun () -> Seq.length ls)
    let v =
      ls
      |> Seq.take n
      |> Seq.map (fun (s: string) -> s.Split(",") |> Array.map int |> vector)
      |> List.ofSeq
    v :: parse1 (Seq.skip (n+1) ls)
  
let private idMatComponents = [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]]
  
let private facings1 =
  let evenNegs = [
    idMatComponents
    [[-1; 0; 0]; [0; -1; 0]; [0; 0;  1]]
    [[-1; 0; 0]; [0;  1; 0]; [0; 0; -1]]
    [[ 1; 0; 0]; [0; -1; 0]; [0; 0; -1]]
  ]
  let evenSwaps = [
    idMatComponents
    [[0; 0; 1]; [1; 0; 0]; [0; 1; 0]]
    [[0; 1; 0]; [0; 0; 1]; [1; 0; 0]]
  ]
  let oddNegs = [
    [[-1; 0; 0]; [0;  1; 0]; [0; 0;  1]]
    [[ 1; 0; 0]; [0; -1; 0]; [0; 0;  1]]
    [[ 1; 0; 0]; [0;  1; 0]; [0; 0; -1]]
    [[-1; 0; 0]; [0; -1; 0]; [0; 0; -1]]
  ]
  let oddSwaps = [
    [[0; 1; 0]; [1; 0; 0]; [0; 0; 1]]
    [[0; 0; 1]; [0; 1; 0]; [1; 0; 0]]
    [[1; 0; 0]; [0; 0; 1]; [0; 1; 0]]
  ]
  Seq.append
    (Seq.allPairs evenNegs evenSwaps)
    (Seq.allPairs oddNegs oddSwaps)
  |> Seq.map (fun (m0, m1) -> matrix m0 * matrix m1)
  |> List.ofSeq

type private MVFunc = {
  Mat: Matrix<int>
  Vec: Vector<int>
}

module private MVFunc =
  let (<<) mv0 mv1 = {
    Mat = mv0.Mat * mv1.Mat
    Vec = mv0.Mat * mv1.Vec + mv0.Vec
  }

  let apply mv v = mv.Mat * v + mv.Vec

  let id = {
    Mat = matrix idMatComponents
    Vec = vector [0; 0; 0]
  }

let private tryOverlap1 (s: Scanner) (sRoot: Scanner) =
  facings1
  |> Seq.collect (fun fac ->
    s
    |> Seq.map ((*) fac)
    |> Seq.allPairs sRoot
    |> Seq.map (Vector.(-))
    |> Seq.countBy id
    |> Seq.filter (snd >> (<=) 12)
    |> Seq.map (fun (offset, _) -> {Mat=fac; Vec=offset}))
  |> Seq.tryExactlyOne

let private overlapAll1 =
  let rec inner (rooted, unrooted) =
    match unrooted with
    | [] -> rooted
    | _ ->
      let folder (rooted, unrooted) s =
        let picker (sRoot, mvRoot) = tryOverlap1 s sRoot |> Option.map (MVFunc.(<<) mvRoot)
        match rooted |> Seq.tryPick picker with
        | None -> rooted, s::unrooted
        | Some mv -> (s, mv)::rooted, unrooted
      inner (List.fold folder ([], []) unrooted)

  function
  | s::ss -> inner ([s, MVFunc.id], ss)
  | [] -> []

let part1: string seq -> int =
  //parse
  //>> overlapAll
  //>> Map.toSeq
  //>> Seq.collect (fun (s, f) -> Seq.map f s)
  //>> Seq.distinct
  //>> Seq.length
  parse1
  >> overlapAll1
  >> Seq.collect (fun (s, mv) -> s |> Seq.map (MVFunc.apply mv))
  >> Seq.distinct
  >> Seq.length

let part2: string seq -> int =
  let rec pairs =
    function
    | [] -> Seq.empty
    | e::es ->
      Seq.append
        (Seq.map (fun e1 -> e, e1) es)
        (pairs es)
  //parse
  //>> overlapAll
  //>> Map.toList
  //>> List.map (fun (_, f) -> f (0, 0, 0))
  //>> pairs
  //>> Seq.map (fun (p0, p1) -> dist p0 p1)
  //>> Seq.max
  parse1
  >> overlapAll1
  >> List.map (fun (_, mv) -> mv.Vec)
  >> pairs
  >> Seq.map (fun (p0, p1) -> Vector.Abs (p0-p1) |> Vector.sum)
  >> Seq.max
