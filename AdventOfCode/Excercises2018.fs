#if INTERACTIVE
#r "nuget: XUnit"
#r "nuget: FSharp.Data"
#load "Utils.fs"
#load "TopologicalSort.fs"
#else
namespace Excercises2018
#endif

open Utils
open Xunit
open System
open System.Text
open System.Collections.Generic

module Helpers =

  let print<'a> (toString : 'a -> string) (grid : 'a[,]) =
    let height = grid.GetLength(0)
    let width = grid.GetLength(1)
    let sb = new StringBuilder((width + 2) * height + 2)
    let _ = sb.AppendLine()
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        ignore <| sb.Append(toString grid.[y, x])
      ignore <| sb.AppendLine()
    sb.ToString()

  module Tuple =
    let ofFive array =
      match array with
      | [|a;b;c;d;e|] -> a,b,c,d,e
      | _ -> invalidArg "array" "Must contain five elements"

module Day01 =

  let repeatedValues seed (changes : int []) =
    let frequencies = new HashSet<_>()
    ignore <| frequencies.Add 0

    let rec loop current idx =
      let next = current + changes.[idx]
      if frequencies.Add next
      then loop next ((idx + 1) % changes.Length)
      else next

    loop seed 0

  [<Theory>]
  [<FileData(2018, 1, 445)>]
  let part1 (input: string array) expected =
    let changes = input |> Array.map int
    changes |> Array.sum =! expected

  let part2_input = [| "+1"; "-1" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2018, 1, 219)>]
  [<MemberData(nameof part2_sample, 0)>]
  let part2 (input: string array) expected =
    let changes = input |> Array.map int
    repeatedValues 0 changes =! expected

module Day02 =

  let categorize (s:string) =
    let counts = s.ToCharArray() |> Array.countBy id
    let hasTwo = if counts |> Array.exists(snd >> (=) 2) then 1 else 0
    let hasThree = if counts |> Array.exists(snd >> (=) 3) then 1 else 0
    hasTwo, hasThree

  let inline (++) (a, b) (x, y) = (a + x, b + y)

  let checkSum input =
    input
    |> Array.map categorize
    |> Array.fold (++) (0,0)
    |> fun (a, b) -> a * b

  let part1_input = [| "abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 2, 5658)>]
  [<MemberData(nameof part1_sample, 12)>]
  let part1 (input: string array) expected =
    checkSum input =! expected

  let diff (s:string) (t:string) =
    (s.ToCharArray(), t.ToCharArray())
    ||> Array.map2(fun c1 c2 -> int c2 - int c1)
    |> Array.sumBy(fun i -> if i = 0 then 0 else 1)

  let commonLetters (s:string) (t:string) =
    (s.ToCharArray(), t.ToCharArray())
    ||> Array.map2(fun c1 c2 -> if c1 = c2 then string c1 else "")
    |> String.concat ""

  [<Theory>]
  [<FileData(2018, 2, "nmgyjkpruszlbaqwficavxneo")>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let result =
      (input, input)
      ||> Array.allPairs
      |> Array.filter(fun (a, b) -> a <> b)
      |> Array.distinctBy(fun (a, b) -> if String.CompareOrdinal(a, b) > 0 then (a, b) else (b, a))
      |> Array.map(fun (a, b) -> diff a b, a, b)
      |> Array.sortBy(fun(a, _, _) -> a)
      |> Array.head
      |> fun (_, a, b) -> commonLetters a b
    result =! expected

module Day03 =

  type Claim =
    { Id : int
      Left : int
      Top : int
      Width : int
      Height : int}
    static member Parse(s : string) =
      let split = s.Split("# ,@:x".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
      { Id = int split.[0]
        Left = int split.[1]
        Top = int split.[2]
        Width = int split.[3]
        Height = int split.[4] }
    member this.Covers(x, y) =
      this.Left <= x && x < this.Left + this.Width &&
      this.Top <= y && y < this.Top + this.Height

  let fillCount (fabric : _[,]) (claim : Claim) =
    for y = claim.Top to claim.Top + claim.Height - 1 do
      for x = claim.Left to claim.Left + claim.Width - 1 do
        fabric.[y, x] <- fabric.[y, x] + 1
    fabric

  let count (fabric : _[,]) =
    let mutable i = 0
    let height = fabric.GetLength(0)
    let width = fabric.GetLength(1)
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if fabric.[y, x] > 1 then i <- i + 1
    i

  let findNonOverlapping width height (input : Claim []) =
    let set = input |> HashSet
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let covered = input |> Array.filter(fun claim -> claim.Covers(x, y))
        if covered.Length > 1 then
          for c in covered do
            ignore <| set.Remove(c)
    set |> Seq.head

  let part1_input = [| "#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 3, 110891)>]
  [<MemberData(nameof part1_sample, 4)>]
  let part1 (input: string array) expected =
    let claims = input |> Array.map Claim.Parse
    let size = if input.Length <= 3 then 8 else 1000
    let counted = (Array2D.create size size 0, claims) ||> Array.fold fillCount
    count counted =! expected

  let part2_input = [| "#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2018, 3, 297)>]
  [<MemberData(nameof part2_sample, 3)>]
  let part2 (input: string array) expected =
    let claims = input |> Array.map Claim.Parse
    let size = if input.Length <= 3 then 8 else 1000
    let claim = findNonOverlapping size size claims
    claim.Id =! expected

module Day04 =

  let parse (input : string) =
    let time = input.Substring(1, 16) |> DateTime.Parse
    time, input.Substring(19)

  let groupWhen predicate list =
    match list with
    | [] -> []
    | [ x ] -> [[x]]
    | x :: xs ->
      xs |> List.fold(fun (all, builder) x ->
        if predicate x
        then List.rev builder :: all, [x]
        else all, x :: builder) ([], [x])
      |> fun (all, builder) -> List.rev builder::all |> List.rev

  type GuardState =
    | Awake
    | Asleep
    static member Parse(s) =
      match s with
      | "wakes up" -> Awake
      | "falls asleep" -> Asleep
      | _ -> invalidArg "s" <| "Could not parse " + s

  let parseGroup (group : (DateTime * string) list) =
    let (id, beginTime, tail) =
      match group with
      | (beginTime, guard) :: tail when guard.StartsWith("Guard") ->
        let id = guard.Split(' ').[1]
        id, beginTime, tail
      | _ -> invalidOp "Group must start with 'Guard'"
    let states =
      (beginTime, "wakes up") ::  tail
      |> List.pairwise
      |> List.collect(fun ((start, state), (end', _)) ->
        let minutes = (end' - start).TotalMinutes |> int
        let state = GuardState.Parse state
        List.init minutes (fun i -> start.AddMinutes(float i), state) )
    id, states

  let parseGroups list =
    list
    |> groupWhen(fun (_, x : string) -> x.StartsWith "Guard")
    |> List.map parseGroup

  let guardMostAsleep groups =
    groups
    |> List.map(fun (id : string, states) ->
      id, states |> List.filter(fun (_, state) -> match state with Asleep -> true | _ -> false) |> List.length)
    |> List.groupBy fst
    |> List.map(fun (id, grp) -> id, grp |> List.sumBy snd)
    |> List.maxBy snd
    |> fst

  let mostAsleepMinute group mostAsleepId =
    group
    |> List.collect(fun (id, l) ->
      if id = mostAsleepId then
        l
        |> List.choose(fun (time, s) -> match s with Asleep -> Some time | _ -> None)
      else [])
    |> List.countBy(fun (dt : DateTime) -> dt.Minute)
    |> List.maxBy snd

  [<Theory>]
  [<FileData(2018, 4, 106710)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let sorted = input |> Array.map parse |> Array.sortBy fst |> List.ofArray
    let groups = parseGroups sorted
    let guardId = guardMostAsleep groups
    let (minute, _) = mostAsleepMinute groups guardId
    let result = (guardId.Substring(1) |> int) * minute
    result =! expected

  [<Theory>]
  [<FileData(2018, 4, 10491)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let sorted = input |> Array.map parse |> Array.sortBy fst |> List.ofArray
    let groups = parseGroups sorted
    let result =
      groups
      |> List.map(fun (id : string, states) ->
        id, states |> List.choose(fun (time, state) -> match state with Asleep -> Some time| _ -> None))
      |> List.groupBy fst
      |> List.map(fun (guard, grp) ->
        let l = grp |> List.map snd |> List.collect id |> List.countBy(fun dt -> dt.Minute)
        let minute, count =
          match l with
          | [] ->  (-1, 0)
          | l -> l |> List.maxBy snd
        guard, minute, count)
      |> List.maxBy(fun (_, _, c) -> c)
      |> fun (a, b, _) -> (a.Substring(1) |> int) * b
    result =! expected

module Day05 =

  module Char =
    let isUpperLowerPair (a: char) (b: char) =
      let d = int a - int b
      d = 32 || d = -32

    let equalsIgnoreCase (a: char) (b: char) =
      let d = int a - int b
      d = 0 || d = 32 || d = -32

  let removePairs list =
    let rec loop acc list =
      match list with
      | [] -> acc
      | x :: xs ->
        match acc with
        | y :: ys when Char.isUpperLowerPair x y -> loop ys xs
        | _ -> loop (x :: acc) xs
    loop [] list

  let removePairsIgnoreChar list ignoreChar =
    let rec loop acc list =
      match list with
      | [] -> acc
      | x :: xs ->
        if Char.equalsIgnoreCase ignoreChar x then
          loop acc xs
        else
          match acc with
          | y :: ys when Char.isUpperLowerPair x y -> loop ys xs
          | _ -> loop (x :: acc) xs
    loop [] list

  let part1_input = [| "dabAcCaCBAcCcaDA" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 5, 10638)>]
  [<MemberData(nameof part1_sample, 10)>]
  let part1 (input: string array) expected =
    input.[0] |> Seq.toList |> removePairs |> List.length =! expected

  let part2_input = [| "dabAcCaCBAcCcaDA" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2018, 5, 4944)>]
  [<MemberData(nameof part2_sample, 4)>]
  let part2 (input: string array) expected =
    let inputList = Seq.toList input.[0]
    let result =
      seq { 'a' .. 'z' }
      |> Seq.map(removePairsIgnoreChar inputList >> List.length)
      |> Seq.min
    result =! expected

module Day06 =

  let sep = [|','|]
  let parse(s : string) =
    match s.Split(sep, StringSplitOptions.RemoveEmptyEntries) with
    | [| x; y |] -> int x, int y
    | _ -> failwith <| "Bad input: " + s

  module Array2D =
    let toSeq (array2D : 'a[,]) = seq {
      let en = array2D.GetEnumerator()
      while en.MoveNext() do
        yield en.Current :?> 'a
    }

  let fillWithDistance input (grid : _[,]) =
    for x = 0 to grid.GetLength(1) - 1 do
      for y = 0 to grid.GetLength(0) - 1 do
        let (d, coords) =
          input
          |> Array.groupBy(fun (_, (x', y')) -> abs(x-x') + abs(y-y'))
          |> Array.minBy fst
        grid.[y, x] <-
          match coords with
          | [| ch, _ |] -> ch, d
          | _ -> '.', d

  let borderElements (grid : _[,]) =
    let set = new HashSet<_>()
    let width = grid.GetLength(1) - 1
    let height = grid.GetLength(0) - 1
    for x = 0 to width do
      set.Add(fst grid.[0, x]) |> ignore
      set.Add(fst grid.[height, x]) |> ignore
    for y = 0 to height do
      set.Add(fst grid.[y, 0]) |> ignore
      set.Add(fst grid.[y, width]) |> ignore
    set

  let fillWithDistance2 input (grid : _[,]) minDist =
    let mutable count = 0
    for x = 0 to grid.GetLength(1) - 1 do
      for y = 0 to grid.GetLength(0) - 1 do
        let totalDist =
          input
          |> Array.sumBy(fun (_, (x', y')) -> abs(x-x') + abs(y-y'))
        if totalDist < minDist then
          grid.[y, x] <- ('#', 0)
          count <- count + 1
    count

  let part1_input = [| "1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 6, 3687)>]
  [<MemberData(nameof part1_sample, 17)>]
  let part1 (input: string array) expected =
    let coords = input |> Array.map parse
    let width, height = coords |> Array.fold Tuple.max (0,0)
    let grid = Array2D.create (height + 1) (width + 1) ('.', -1)
    let coords' = coords |> Array.zip ([| 'A' .. 'z' |] |> Array.take coords.Length)
    fillWithDistance coords' grid
    let border = borderElements grid
    let (_, area) =
      grid
      |> Array2D.toSeq
      |> Seq.countBy fst
      |> Seq.filter(fun (ch, _) -> not <| border.Contains ch)
      |> Seq.maxBy snd
    area =! expected

  [<Theory>]
  [<FileData(2018, 6, 40134)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let coords = input |> Array.map parse
    let width, height = coords |> Array.fold Tuple.max (0,0)
    let grid = Array2D.create (height + 1) (width + 1) ('.', -1)
    let coords' = coords |> Array.zip ([| 'A' .. 'z' |] |> Array.take coords.Length)
    let minDist = if input.Length <= 6 then 32 else 10000
    let result = fillWithDistance2 coords' grid minDist
    result =! expected

module Day07 =

  type Step =
    { Name : char
      DependsOn : char []}
    static member Parse(s:string) =
      let name = s.[36]
      { Name = name ; DependsOn = [| s.[5] |] }

  let addFreeSteps input =
    input
    |> Array.collect(fun x -> x.DependsOn)
    |> Array.except (input |> Array.map(fun x -> x.Name))
    |> Array.map(fun x -> { Name = x ; DependsOn = [||] } )
    |> fun free -> Array.concat [| input ; free |]

  let getExecutionOrder input =
    input
    |> Array.groupBy(fun x -> x.Name)
    |> Array.map(fun (name, grp) -> { Name = name ; DependsOn = grp |> Array.collect(fun y -> y.DependsOn)})
    |> Array.sortBy(fun x -> x.Name)
    |> TopologicalSort.getTopologicalSortOrderWith(fun x -> x.Name) (fun x -> x.DependsOn)
    |> Array.rev

  let part1_input = [|
    "Step C must be finished before step A can begin."
    "Step C must be finished before step F can begin."
    "Step A must be finished before step B can begin."
    "Step A must be finished before step D can begin."
    "Step B must be finished before step E can begin."
    "Step D must be finished before step E can begin."
    "Step F must be finished before step E can begin."
  |]
  let part1_sample (result: string) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 7, "MNOUBYITKXZFHQRJDASGCPEVWL")>]
  [<MemberData(nameof part1_sample, "CABDFE")>]
  let part1 (input: string array) expected =
    let steps = input |> Array.map Step.Parse |> addFreeSteps
    let order = getExecutionOrder steps |> Array.map(fun x -> x.Name) |> String.Concat
    order =! expected

module Day08 =

  type Node = Node of int * meta : int list * children : Node list

  let rec parse items =
    let rec loop id items =
      match items with
      | [] -> failwith "Input must not be empty"
      | childCount :: metaCount :: tail ->
        let mutable tail' = tail
        let mutable id' = id
        let children =
          List.init childCount <| fun _ ->
            let children, tail'', id'' = loop (id' + 1) tail'
            tail' <- tail''
            id' <- id''
            children
        let meta, tail' = tail' |> List.splitAt metaCount
        Node(id, meta, children), tail', id'
      | _ -> failwith "Not enough elements"
    let (node, _, _) = loop 0 items
    node

  let rec addMeta (Node(_, meta, children)) =
    (+) (List.sum meta) (List.sumBy addMeta children)

  let rec getNodeValue (Node(_, meta, children)) =
    match children with
    | [] -> List.sum meta
    | _ ->
      meta
      |> List.sumBy(fun idx ->
        children
        |> List.tryItem(idx - 1)
        |> Option.map getNodeValue
        |> Option.defaultValue 0)

  let part1_input = [| "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2018, 8, 47464)>]
  [<MemberData(nameof part1_sample, 138)>]
  let part1 (input: string array) expected =
    let data = input.[0].Split(' ') |> Array.map int |> List.ofArray
    let node = parse data
    addMeta node =! expected

  let part2_input = [| "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2018, 8, 23054)>]
  [<MemberData(nameof part2_sample, 66)>]
  let part2 (input: string array) expected =
    let data = input.[0].Split(' ') |> Array.map int |> List.ofArray
    let node = parse data
    getNodeValue node =! expected

module Day09 =
  // Incomplete in original file
  ()

module Day10 =
  // Incomplete in original file
  ()

module Day12 =
  // Incomplete in original file
  ()
