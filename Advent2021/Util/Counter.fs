module Counter

let add k =
  Map.change k (Option.map ((+) 1UL) >> Option.orElse (Some 1UL))

let ofSeq s =
  Seq.fold (fun c k -> add k c) Map.empty s

let addCount k (n: uint64) =
  Map.change k (Option.map ((+) n) >> Option.orElse (Some n))

let ofCountSeq s =
  Seq.fold (fun c (k, n) -> addCount k n c) Map.empty s

let merge c0 c1 =
  Map.fold (fun c k n -> addCount k n c) c0 c1
