open System
open System.IO
open System.Text
open System.Collections.Generic

//https://adventofcode.com/2018

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput file = File.ReadAllLines("input/" + file)
let (|>>) x f = f x ; x

module Seq =
  let print (s : seq<_>) =
    s |> Seq.iter(printfn "%A")

module Day1 =
  let changes = readInput "day1.txt" |> Array.map int

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

  let input = readInput "day2.txt"

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

  let print<'a> (fabric : 'a[,]) =
    let height = fabric.GetLength(0)
    let width = fabric.GetLength(1)
    let sb = new StringBuilder((width + 2) * height + 2)
    let _ = sb.AppendLine()
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        ignore <| sb.Append(fabric.[y, x])
      ignore <| sb.AppendLine()
    sb.ToString()

  fsi.AddPrinter print<int>
  fsi.AddPrinter print<char>

  let input = readInput "day3.txt" |> Array.map Claim.Parse

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

  let input = readInput "day4.txt" |> Array.map parse |> Array.sortBy fst |> List.ofArray

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
  let input = readInput "day5.txt" |> Array.exactlyOne
  let testInput = "dabAcCaCBAcCcaDA"

  let upper ch = Char.ToUpper ch

  let isPair c1 c2 = c1 <> c2 && upper c1 = upper c2

  let removePairs predicate list =
    let rec loop acc = function
      | [] -> acc
      | x :: xs ->
        match acc with
        | y :: ys when predicate x y -> loop ys xs
        | _ -> loop (x :: acc) xs
    loop [] list

  let input' = input

  input' |> Seq.toList |> removePairs isPair |> List.length

  let inputList = input' |> Seq.toList
  ['a' .. 'z']
  |> Seq.map(fun ch -> inputList |> List.filter(fun c -> c <> ch && c <> upper ch))
  |> Seq.map(removePairs isPair >> List.length)
  |> Seq.min

module Day6 =
  let sep = [|','|]
  let parse(s : string) =
    match s.Split(sep, StringSplitOptions.RemoveEmptyEntries) with
    | [|x; y|] -> int x, int y
    | _ -> failwith <| "Bad input: " + s
      
  let input = readInput "day6.txt" |> Array.map parse
  let testInput = [| "1, 1" ; "1, 6" ; "8, 3" ; "3, 4" ; "5, 5" ; "8, 9" |] |> Array.map parse

  let tupleMax (a,b) (c,d) = max a c, max b d

  testInput |> Array.fold tupleMax (0,0)