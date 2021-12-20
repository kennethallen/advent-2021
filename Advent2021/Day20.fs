module Day20

let parse ls =
  let parsePixels s = Seq.map ((=) '#') s |> Array.ofSeq

  let alg = Seq.head ls |> parsePixels
  let imgData =
    ls
    |> Seq.skip 2
    |> Seq.map parsePixels
    |> Array.ofSeq
  alg, (imgData, false)

let algIndex (imgData, bg) x y =
  Seq.allPairs { x-1 .. x+1 } { y-1 .. y+1 }
  |> Seq.map (fun (x, y) ->
    Array.tryItem x imgData
    |> Option.bind (Array.tryItem y)
    |> Option.defaultValue bg)
  |> Seq.fold (fun n b -> 2*n + if b then 1 else 0) 0

let step alg (imgData, bg) =
  let imgData =
    [|-1 .. Array.length imgData|]
    |> Array.map (fun x ->
      [|-1 .. Array.length (Array.head imgData)|]
      |> Array.map (fun y ->
        Array.item (algIndex (imgData, bg) x y) alg))
  imgData, bg <> Array.head alg

let enhance alg passes img =
  Seq.replicate passes (step alg)
  |> Seq.fold (|>) img

let run n ls =
  let alg, img = parse ls
  img
  |> enhance alg n
  |> fst
  |> Seq.collect id
  |> Seq.filter id
  |> Seq.length

let part1: string seq -> int = run 2
let part2: string seq -> int = run 50
