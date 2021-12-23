module Array

let squareDims map =
  Array.length map,
  Array.tryHead map
  |> Option.map Array.length
  |> Option.defaultValue 0
