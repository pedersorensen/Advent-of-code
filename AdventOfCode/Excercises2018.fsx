#load "Utils.fsx"
#load "TopologicalSort.fs"

open Utils
open System
open System.Text
open System.Collections.Generic

Year <- 2018

//https://adventofcode.com/2018

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

module Day1 =
  let changes = readInput 1 |> Array.map int

  let repeatedValues seed (changes : int []) =
    let frequencies = new HashSet<_>()
    ignore <| frequencies.Add 0

    let rec loop current idx =
      let next = current + changes.[idx]
      if frequencies.Add next
      then loop next ((idx + 1) % changes.Length)
      else next

    loop seed 0

  repeatedValues 0 [|+1; -1|]// first reaches 0 twice.
  repeatedValues 0 [|+3; +3; +4; -2; -4|]// first reaches 10 twice.
  repeatedValues 0 [|-6; +3; +8; +5; -6|]// first reaches 5 twice.
  repeatedValues 0 [|+7; +7; -2; -7; -4|]// first reaches 14 twice.
  repeatedValues 0 changes

module Day2 =
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

  let testInput = [|
    "abcdef" // contains no letters that appear exactly two or three times.
    "bababc" // contains two a and three b, so it counts for both.
    "abbcde" // contains two b, but no letter appears exactly three times.
    "abcccd" // contains three c, but no letter appears exactly two times.
    "aabcdd" // contains two a and two d, but it only counts once.
    "abcdee" // contains two e.
    "ababab" // contains three a and three b, but it only counts once.
  |]

  let input = readInput 2

  checkSum testInput
  checkSum input

  let diff (s:string) (t:string) =
    (s.ToCharArray(), t.ToCharArray())
    ||> Array.map2(fun c1 c2 -> int c2 - int c1)
    |> Array.sumBy(fun i -> if i = 0 then 0 else 1)

  let testInput2 = [|
    "abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"
  |]

  (input, input)
  ||> Array.allPairs
  |> Array.filter(fun (a, b) -> a <> b)
  |> Array.distinctBy(fun (a, b) -> if String.CompareOrdinal(a, b) > 0 then (a, b) else (b, a))
  |> Array.map(fun (a, b) -> diff a b, a, b)
  |> Array.sortBy(fun(a, _, _) -> a)
  |> Array.item 0

  "fzvstwblgqkhpuixdrnevmaycd" = "fivstwblgqkhpuixdrnevmaycd"

module Day3 =
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
    member this.Overlaps(claim : Claim) =
      this.Covers(claim.Left, claim.Top) ||
      this.Covers(claim.Left + claim.Width, claim.Top) ||
      this.Covers(claim.Left, claim.Top + claim.Height) ||
      this.Covers(claim.Left + claim.Width, claim.Top + claim.Height)

  let fill (fabric : _[,]) (claim : Claim) =
    for y = claim.Top to claim.Top + claim.Height - 1 do
      for x = claim.Left to claim.Left + claim.Width - 1 do
        fabric.[y, x] <- '#'
    fabric

  let fillCount (fabric : _[,]) (claim : Claim) =
    for y = claim.Top to claim.Top + claim.Height - 1 do
      for x = claim.Left to claim.Left + claim.Width - 1 do
        fabric.[y, x] <- fabric.[y, x] + 1
    fabric

  fsi.AddPrinter <| print<int> string
  fsi.AddPrinter <| print<char> string

  let input = readInput 3 |> Array.map Claim.Parse

  let testInput =
    [|
      "#1 @ 1,3: 4x4"
      "#2 @ 3,1: 4x4"
      "#3 @ 5,5: 2x2"
    |]
    |> Array.map Claim.Parse
  (Array2D.create 8 8 '.', testInput) ||> Array.fold fill
  let countedTest = (Array2D.create 8 8 0, testInput) ||> Array.fold fillCount

  let counted = (Array2D.create 1000 1000 0, input) ||> Array.fold fillCount

  let count (fabric : _[,]) =
    let mutable i = 0
    let height = fabric.GetLength(0)
    let width = fabric.GetLength(1)
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if fabric.[y, x] > 1 then i <- i + 1
    i

  count counted
  count countedTest

  let findCovered width height (input : Claim []) =
    let set = input |> HashSet
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let covered = input |> Array.filter(fun claim -> claim.Covers(x, y))
        if covered.Length > 1 then
          for c in covered do
            ignore <| set.Remove(c)
    set |> Seq.toArray

  let res1 = findCovered 8 8 testInput
  let res2 = findCovered 1000 1000 input

  let overlaps =
    input
    |> Array.filter(fun claim ->
      input
      |> Array.exists(fun c -> claim.Id <> c.Id && (c.Overlaps claim || claim.Overlaps c))
      |> not)
    //(testInput, testInput)
    //||> Array.allPairs
    //|> Array.filter(fun (a, b) -> a.Id <> b.Id && a.Overlaps(b))
    //|> Array.collect(fun (a, b) -> [|a;b|])
    //|> Array.distinctBy(fun x -> x.Id)
  testInput |> Array.except overlaps

module Day4 =
  let parse (input : string) =
    let time = input.Substring(1, 16) |> DateTime.Parse
    time, input.Substring(19)

  let testInput =
    [|
      "[1518-11-01 00:00] Guard #10 begins shift"
      "[1518-11-01 00:05] falls asleep"
      "[1518-11-01 00:25] wakes up"
      "[1518-11-01 00:30] falls asleep"
      "[1518-11-01 00:55] wakes up"

      "[1518-11-01 23:58] Guard #99 begins shift"
      "[1518-11-02 00:40] falls asleep"
      "[1518-11-02 00:50] wakes up"

      "[1518-11-03 00:05] Guard #10 begins shift"
      "[1518-11-03 00:24] falls asleep"
      "[1518-11-03 00:29] wakes up"

      "[1518-11-04 00:02] Guard #99 begins shift"
      "[1518-11-04 00:36] falls asleep"
      "[1518-11-04 00:46] wakes up"

      "[1518-11-05 00:03] Guard #99 begins shift"
      "[1518-11-05 00:45] falls asleep"
      "[1518-11-05 00:55] wakes up"
    |]
    |> Array.map parse
    |> List.ofArray

  let input = readInput 4 |> Array.map parse |> Array.sortBy fst |> List.ofArray

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

  let testGroup = testInput |> parseGroups
  let testGuardMostAsleep = guardMostAsleep testGroup

  mostAsleepMinute testGroup testGuardMostAsleep

  let actualGroup = input |> parseGroups
  let actualGuardMostAsleep = guardMostAsleep actualGroup

  mostAsleepMinute actualGroup actualGuardMostAsleep

  testGroup
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

module Day5 =
  let input = readInput 5 |> Array.exactlyOne
  let testInput = "dabAcCaCBAcCcaDA"

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

  // 10638
  let part1() =
    input |> Seq.toList |> removePairs |> List.length

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

  // 4944
  let part2() =
    let inputList = Seq.toList input
    { 'a' .. 'z' }
    |> Seq.map(removePairsIgnoreChar inputList >> List.length)
    |> Seq.min

module Day6 =
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

  fsi.AddPrinter <| print<char*int> (fun (ch, _) -> ch.ToString())

  let input = readInput 6 |> Array.map parse
  let testInput = [| "1, 1" ; "1, 6" ; "8, 3" ; "3, 4" ; "5, 5" ; "8, 9" |] |> Array.map parse

  let input' = input

  let width, height = input' |> Array.fold Tuple.max (0,0)

  let grid = Array2D.create (height + 1) (width + 1) ('.', -1)
  let input'' = input' |> Array.zip ([| 'A' .. 'z' |] |> Array.take input'.Length)

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

  let fillWithDistance2 input (grid : _[,]) min =
    for x = 0 to grid.GetLength(1) - 1 do
      for y = 0 to grid.GetLength(0) - 1 do
        let coords =
          input
          |> Array.sumBy(fun (_, (x', y')) -> abs(x-x') + abs(y-y'))
        if coords < min then
          grid.[y, x] <- ('#', 0)

  fillWithDistance input'' grid
  fillWithDistance2 input'' grid 10000

  grid |> Array2D.toSeq |> Seq.countBy fst

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

  let border = borderElements grid
  let areas =
    grid
    |> Array2D.toSeq
    |> Seq.countBy fst
    |> Seq.filter(fun (ch, _) -> not <| border.Contains ch)
    |> Seq.maxBy snd

module Day7 =
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

  let input =
    readInput 7
    |> Array.map Step.Parse
    |> addFreeSteps

  let testInput =
    [|
      "Step C must be finished before step A can begin."
      "Step C must be finished before step F can begin."
      "Step A must be finished before step B can begin."
      "Step A must be finished before step D can begin."
      "Step B must be finished before step E can begin."
      "Step D must be finished before step E can begin."
      "Step F must be finished before step E can begin."
    |]
    |> Array.map Step.Parse
    |> addFreeSteps

  let getExecutionOrder input =
    input
    |> Array.groupBy(fun x -> x.Name)
    |> Array.map(fun (name, grp) -> { Name = name ; DependsOn = grp |> Array.collect(fun y -> y.DependsOn)})
    |> Array.sortBy(fun x -> x.Name)
    |> TopologicalSort.getTopologicalSortOrderWith(fun x -> x.Name) (fun x -> x.DependsOn)
    |> Array.rev

  let testOrder = getExecutionOrder testInput
  testOrder |> Array.map(fun x -> x.Name) |> String.Concat
  getExecutionOrder input |> Array.map(fun x -> x.Name) |> String.Concat

  type WorkItem =
    { Step : Step
      Time : int
      IsReady : bool }
    static member OfStep step =
      { Step = step
        Time = int step.Name - int 'A' + 1
        IsReady = step.DependsOn |> Array.isEmpty }

  let toDo = testOrder |> Array.map WorkItem.OfStep

module Day8 =
  let input = readInput 8 |> Array.exactlyOne |> fun s -> s.Split(' ') |> Array.map int |> List.ofArray
  let testInput = [2;3;0;3;10;11;12;1;1;0;1;99;2;1;1;2]

  type Node = Node of int * meta : int list * children : Node list

  let rec print node =
    let b = new StringBuilder()
    let rec loop indent (Node(name, meta, children)) =
      let pre = String(' ', indent)
      Printf.bprintf b "%s- Id: %i\r\n%s  Meta: %A\r\n" pre name pre meta
      match children with
      | [] -> Printf.bprintf b "%s  Children: None\r\n" pre
      | _ ->
        Printf.bprintf b "%s  Children:\r\n" pre
        for child in children do
          loop (indent + 2) child
    loop 0 node
    b.ToString()

  fsi.AddPrinter print

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

  let node = parse testInput
  addMeta node
  getNodeValue node

module Day9 =
  let input = readInput 9 |> Array.exactlyOne

  fsi.AddPrinter <| fun (marbles : ResizeArray<int>, current : int) ->
    marbles
    |> Seq.mapi(fun i value ->
      if i = current then "(" + value.ToString() + ")"
      else " " + value.ToString() + " ")
    |> fun s -> String.Join("", s)

  let marbles = new ResizeArray<int>()
  let mutable c = 0

module Day10 =
  let input = readInput 10

  let testInput = [|
    "position=< 9,  1> velocity=< 0,  2>"
    "position=< 7,  0> velocity=<-1,  0>"
    "position=< 3, -2> velocity=<-1,  1>"
    "position=< 6, 10> velocity=<-2, -1>"
    "position=< 2, -4> velocity=< 2,  2>"
    "position=<-6, 10> velocity=< 2, -2>"
    "position=< 1,  8> velocity=< 1, -1>"
    "position=< 1,  7> velocity=< 1,  0>"
    "position=<-3, 11> velocity=< 1, -2>"
    "position=< 7,  6> velocity=<-1, -1>"
    "position=<-2,  3> velocity=< 1,  0>"
    "position=<-4,  3> velocity=< 2,  0>"
    "position=<10, -3> velocity=<-1,  1>"
    "position=< 5, 11> velocity=< 1, -2>"
    "position=< 4,  7> velocity=< 0, -1>"
    "position=< 8, -2> velocity=< 0,  1>"
    "position=<15,  0> velocity=<-2,  0>"
    "position=< 1,  6> velocity=< 1,  0>"
    "position=< 8,  9> velocity=< 0, -1>"
    "position=< 3,  3> velocity=<-1,  1>"
    "position=< 0,  5> velocity=< 0, -1>"
    "position=<-2,  2> velocity=< 2,  0>"
    "position=< 5, -2> velocity=< 1,  2>"
    "position=< 1,  4> velocity=< 2,  1>"
    "position=<-2,  7> velocity=< 2, -2>"
    "position=< 3,  6> velocity=<-1, -1>"
    "position=< 5,  0> velocity=< 1,  0>"
    "position=<-6,  0> velocity=< 2,  0>"
    "position=< 5,  9> velocity=< 1, -2>"
    "position=<14,  7> velocity=<-2,  0>"
    "position=<-3,  6> velocity=< 2, -1>"
    |]

  type Point =
    { X : int
      Y : int
      VX : int
      VY : int}
    static member Parse(s : string) =
      let s =s.Split([|'<';',';'>'|], StringSplitOptions.RemoveEmptyEntries)
      { X = int s.[1]
        Y = int s.[2]
        VX = int s.[4]
        VY = int s.[5] }

  type Grid =
    { Width : int
      Height : int
      Points : Point[] }
    static member OfPoints points =
      let (width, height) = points |> Array.fold(fun tpl p -> Tuple.max tpl (p.X, p.Y)) (0,0)
      let (left, top) = points |> Array.fold(fun tpl p -> Tuple.min tpl (p.X, p.Y)) (0,0)
      { Width = width - left + 1
        Height = height - top + 1
        Points = points |> Array.map(fun p -> { p with X = p.X - left ; Y = p.Y - top })}

  fsi.AddPrintTransformer <| fun (map : Grid) ->
    let grid = Array2D.create map.Height map.Width '.'
    for p in map.Points do grid.[p.Y, p.X] <- '#'
    box grid

  let advance (map : Grid) =
    { map with Points = map.Points |> Array.map(fun p -> { p with X = p.X + p.VX ; Y = p.Y + p.VY })}

  let map = input |> Array.map Point.Parse |> Grid.OfPoints
  map.Width

module Day12 =
  let input = readInput 12
  let initial = input.[0].Substring(15)
  let rules =
    input
    |> Array.skip 2
    |> Array.map(fun s -> s.Substring(0, 5).ToCharArray() |> Tuple.ofFive, s.[9])
    |> Map.ofArray

  let testRules =
    [|
      "...## => #"
      "..#.. => #"
      ".#... => #"
      ".#.#. => #"
      ".#.## => #"
      ".##.. => #"
      ".#### => #"
      "#.#.# => #"
      "#.### => #"
      "##.#. => #"
      "##.## => #"
      "###.. => #"
      "###.# => #"
      "####. => #"
    |]
    |> Array.map(fun s -> s.Substring(0, 5).ToCharArray() |> Tuple.ofFive, s.[9])
    |> Map.ofArray

  let testInitial = "#..#.#..##......###...###"

  type AutoArray<'T>(defaultValue) =
    let ensureSize (arr : ResizeArray<_>) idx =
      if idx >= arr.Count then
        for _i = arr.Count to idx do
          arr.Add(defaultValue)
      arr

    let positive = ResizeArray<'T>()
    let negative = ResizeArray<'T>()

    let get idx =
      if idx >= 0
      then (ensureSize positive idx).[idx]
      else (ensureSize negative -idx).[-idx]

    let set idx value =
      if idx >= 0
      then (ensureSize positive idx).[idx] <- value
      else (ensureSize negative -idx).[-idx] <- value

    member val Positive = positive
    member val Negative = negative
    member __.Item
      with get(idx) = get idx
      and set(idx) value = set idx value

    member __.Add item = positive.Add item
    member __.Min = -negative.Count
    member __.Max = positive.Count

  module AutoArray =
    let ofSeq defaultValue seq =
      let a = AutoArray(defaultValue)
      for item in seq do a.Add item
      a

  let apply rules (state : string) =
    let s =
      Array.init(state.Length - 5) (fun i ->
        rules |> Map.tryFind (state.Substring(i, 5)) |> Option.defaultValue '.')
      |> String
    if s.StartsWith("....") && s.EndsWith("....") then s
    elif s.StartsWith("....") then s + "...."
    elif s.EndsWith("....") then "...." + s
    else "...." + s + "...."

  let rec applyN n rules state =
    if n > 0 then applyN (n-1) rules (apply rules state) else state

  let state = AutoArray.ofSeq '.' testInitial
  Array.init(state.Max - state.Min) (fun i ->
    let j = i + state.Min
    let key = state.[j-2], state.[j-1], state.[j], state.[j+1], state.[j+2]
    testRules |> Map.tryFind key |> Option.defaultValue '.') |> String
