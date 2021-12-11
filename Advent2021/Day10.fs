module Day10
#nowarn "25"

let rec check stack i line =
  let tryPop c =
    match stack with
    | head::tail when head = c -> check tail (i+1) line
    | _                        -> Error c
  let push c = check (c::stack) (i+1) line
  match Seq.tryItem i line with
  | Some '(' -> push '('
  | Some '[' -> push '['
  | Some '{' -> push '{'
  | Some '<' -> push '<'
  | Some ')' -> tryPop '('
  | Some ']' -> tryPop '['
  | Some '}' -> tryPop '{'
  | Some '>' -> tryPop '<'
  | None     -> Ok stack

let part1 : string seq -> int =
  Seq.map (check [] 0)
  >> Seq.choose (
    function
    | Error '(' -> Some 3
    | Error '[' -> Some 57
    | Error '{' -> Some 1197
    | Error '<' -> Some 25137
    | Ok _      -> None)
  >> Seq.sum

let scoreIncomplete =
  Seq.fold (fun score c ->
    score*5UL + 
      match c with
      | '(' -> 1UL
      | '[' -> 2UL
      | '{' -> 3UL
      | '<' -> 4UL) 0UL

let middle ns = Seq.item (Seq.length ns / 2) ns

let part2 : string seq -> uint64 =
  Seq.map (check [] 0)
  >> Seq.choose (
    function
    | Ok stack when not (List.isEmpty stack) ->
      Some (scoreIncomplete stack)
    | Error _ -> None)
  >> Seq.sort
  >> middle
