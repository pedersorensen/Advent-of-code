#if INTERACTIVE
#r "nuget: FSharp.Data"
#load "Utils.fs"
Year <- 2021
#else
namespace Excercises2021
#endif

open Xunit
open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Runtime.InteropServices

type FileDataAttribute(day, result: obj) =
  inherit Sdk.DataAttribute()
  override _.GetData(_) =
    let path = ensureExists 2021 day
    [| [| File.ReadAllLines(path) |> box ; result |] |]

[<AutoOpen>]
module Utils =
  let (|Ints|) (data: string[]) = data |> Array.map int
  let (|SingeLineInts|) (data: string[]) = (Array.exactlyOne data).Split(',') |> Array.map int

  let makeSample result (data: string []) = [| [| box data ; box result |] |]

  let inline (=!) (actual : 'T) (expected : 'T) = Assert.Equal<'T>(expected, actual)

module Day01 =

  let countIncreasesA (input: int[]) =
    input
    |> Seq.pairwise
    |> Seq.where(fun (a, b) -> a < b)
    |> Seq.length

  let countIncreasesB (input: int[]) =
    let rec loop acc i =
      if i = input.Length then acc else
      let acc' =
        if input.[i] > input.[i - 1] then acc + 1
        else acc
      loop acc' (i + 1)
    loop 0 1

  let sample (result: int) = makeSample result [|
    "199"
    "200"
    "208"
    "210"
    "200"
    "207"
    "240"
    "269"
    "260"
    "263"
  |]

  [<Theory>]
  [<FileData(1, 1387)>]
  [<MemberData(nameof sample, 7)>]
  let part1 (Ints input) expected =
    countIncreasesA input =! expected
    countIncreasesB input =! expected

  [<Theory>]
  [<FileData(1, 1362)>]
  [<MemberData(nameof sample, 5)>]
  let part2 (Ints input) expected =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countIncreasesB
     =! expected

module Day02 =

  let (|Up|Down|Forward|) (s: string) =
    match s.Split(' ') with
    | [| "up"      ; x |] -> Up     (int x)
    | [| "down"    ; x |] -> Down   (int x)
    | [| "forward" ; x |] -> Forward(int x)
    | _ -> failwithf $"Unknown instruction: '{s}'"

  let moveIt directions =
    (struct(0, 0), directions)
    ||> Array.fold(fun struct(h, d) s ->
      match s with
      | Up      x -> h, d - x
      | Down    x -> h, d + x
      | Forward x -> h + x, d
    )
    |> fun struct(h, d) -> h * d

  let sample (result: int) = makeSample result [|
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
  |]

  [<Theory>]
  [<FileData(2, 2147104)>]
  [<MemberData(nameof sample, 150)>]
  let part1 input expected =
    moveIt input =! expected

  let moveItAgain directions =
    (struct(0, 0, 0), directions)
    ||> Array.fold(fun struct(h, d, aim) s ->
      match s with
      | Up x      -> h, d, aim - int x
      | Down x    -> h, d, aim + int x
      | Forward x -> h + int x, d + aim * int x, aim
    )
    |> fun struct(h, d, _) -> h * d

  [<Theory>]
  [<FileData(2, 2044620088)>]
  [<MemberData(nameof sample, 900)>]
  let part2 input expected =
    moveItAgain input =! expected

module Day03 =

  let binaryToInt digits =
    let struct (value, _) =
      (digits, struct(0, 1))
      ||> Seq.foldBack(fun s struct(n, powerOfTwo) ->
        let n' = if s = '0' then n else n + powerOfTwo
        n', 2 * powerOfTwo
      )
    value

  let getReadings (input: string[]) =
    let counts = Array.zeroCreate input[0].Length
    for s in input do
      for i = 0 to s.Length - 1 do
        let struct(zero, one) = counts[i]
        counts[i] <-
          if s[i] = '0'
          then zero + 1, one
          else zero, one + 1
    let gamma, epsilon =
      counts
      |> Array.map(fun struct(zero, one) -> if zero < one then '1', '0' else '0', '1')
      |> Array.unzip
    binaryToInt gamma, binaryToInt epsilon

  let sample (result: int) = makeSample result [|
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
  |]

  [<Theory>]
  [<FileData(3, 3847100)>]
  [<MemberData(nameof sample, 198)>]
  let part1 input expected =
    let (gamma, epsilon) = getReadings input
    gamma * epsilon =! expected

  let getFilterBit (a,b) i (input: string[]) =
    let struct(zeros, ones) =
      (struct(0,0), input)
      ||> Seq.fold(fun struct(z, o) s ->
        if s[i] = '0'
        then z + 1, o
        else z, o + 1
      )
    if ones < zeros then a else b

  let reduce m i (input: string[]) =
    let bit = getFilterBit m i input
    input |> Array.filter(fun s -> s[i] = bit)

  let filter m (input: string[]) =
    let rec loop i data =
      if Array.length data = 1
      then data[0]
      else reduce m i data |> loop (i + 1)
    loop 0 input

  [<Theory>]
  [<FileData(3, 4105235)>]
  [<MemberData(nameof sample, 230)>]
  let part2 input expected =
    let maxParam = ('0', '1')
    let minParam = ('1', '0')
    let oxygenRate = filter maxParam input |> binaryToInt
    let co2Rate = filter minParam input |> binaryToInt
    oxygenRate * co2Rate =! expected

module Day04 =

  let [<Literal>] DrawnBit = 1024
  let [<Literal>] Width = 5

  let setDrawnBit i = i ||| DrawnBit
  let isDrawn i = i &&& DrawnBit = DrawnBit

  type Board(values: int []) =
    do
      if values.Length <> 25
        then invalidArg (nameof values) "Input array must have length 25."
    let draw idx = values[idx] <- setDrawnBit values[idx]
    let isDrawn i = isDrawn values[i]
    let isFullRow j = isDrawn j && isDrawn (j + 1) && isDrawn (j + 2) && isDrawn (j + 3) && isDrawn (j + 4)
    let isFullColumn i = isDrawn i && isDrawn (i + 5) && isDrawn (i + 10) && isDrawn (i + 15) && isDrawn (i + 20)
    let draw number =
      let idx = Array.IndexOf(values, number)
      if idx > -1 then draw idx
    let hasWon() =
      let rec loop i =
        i < Width && (isFullColumn i || isFullRow (Width * i) || loop (i + 1))
      loop 0

    member _.Values = values
    member _.HasWon = hasWon()
    member _.Draw(number) = draw number
    member _.DrawAndCheck(number) = draw number ; hasWon()

  let toBoards (lines: seq<string>) = [|
    let buffer = ResizeArray()
    for l in lines do
      if String.IsNullOrWhiteSpace (l) then
        if buffer.Count> 1 then
          yield Board(buffer.ToArray())
          buffer.Clear()
      else buffer.AddRange(l.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    if buffer.Count > 0 then yield Board(buffer.ToArray())
  |]

  let parse (input: string[]) =
    let boards = input |> Seq.skip 1 |> toBoards
    boards, input[0].Split(',') |> Array.map int

  let getScore (board: Board, number) =
    let sum =
      board.Values
      |> Array.sumBy(fun i -> if isDrawn i then 0 else i)
    sum * number

  let getWinnerScore input =
    let boards, numbers = parse input
    let board, number =
      numbers
      |> Array.pick(fun number ->
        for board in boards do board.Draw(number)
        boards
        |> Array.tryFind(fun b -> b.HasWon)
        |> Option.map(fun board -> board, number)
      )
    getScore (board, number)

  let sample (result: int) = makeSample result [|
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    "8  2 23  4 24"
    "21  9 14 16  7"
    "6 10  3 18  5"
    "1 12 20 15 19"
    ""
    "3 15  0  2 22"
    "9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    "2  0 12  3  7"
  |]

  [<Theory>]
  [<FileData(4, 5685)>]
  [<MemberData(nameof sample, 4512)>]
  let part1 input expected =
    getWinnerScore input =! expected

  let getLastWinnerScore(input: string[]) =
    let boards, numbers = parse input
    let boards = ResizeArray(boards)
    let winners = ResizeArray()
    numbers
    |> Array.pick(fun number ->
      for i = 0 to boards.Count - 1 do
        if boards[i].DrawAndCheck(number) then winners.Add(i)
      if boards.Count = 1 && boards[0].HasWon then
        Some(boards[0], number)
      else
        for i = winners.Count - 1 downto 0 do
          boards.RemoveAt(winners[i])
        winners.Clear()
        None
    )
    |> getScore

  [<Theory>]
  [<FileData(4, 21070)>]
  [<MemberData(nameof sample, 1924)>]
  let part2 input expected =
    getLastWinnerScore input =! expected

module Day05 =

  let countOverlaps skipDiagonals (input: string[]) =
    let mutable overlaps = 0
    let first = HashSet()
    let second = HashSet()

    let add x y =
      let point = struct(x, y)
      if first.Add(point) |> not && second.Add(point) then
        overlaps <- overlaps + 1

    for s in input do
      let p = s.Split([|' ' ; ','|])
      let x1 = int p[0]
      let y1 = int p[1]
      let x2 = int p[3]
      let y2 = int p[4]

      let dx, dy = (x2 - x1), (y2 - y1)
      if not skipDiagonals || dx = 0 || dy = 0 then
        let l = max (abs dx) (abs dy)
        let deltax, deltay = sign dx, sign dy

        add x1 y1
        add x2 y2

        for i = 1 to l - 1 do
          let x = x1 + i * deltax
          let y = y1 + i * deltay
          add x y

    overlaps

  let sample (result: int) = makeSample result [|
    "0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"
  |]

  [<Theory>]
  [<FileData(5, 8622)>]
  [<MemberData(nameof sample, 5)>]
  let part1 input expected =
    input |> countOverlaps true =! expected

  [<Theory>]
  [<FileData(5, 22037)>]
  [<MemberData(nameof sample, 12)>]
  let part2 input expected =
    input |> countOverlaps false =! expected

module Day06 =

  let advance (array: ResizeArray<_>) =
    let toAdd = ResizeArray()
    for i = 0 to array.Count - 1 do
      let v = array[i] - 1
      if v < 0 then
        toAdd.Add(8)
        array[i] <- 6
      else
        array[i] <- v
    array.AddRange(toAdd)

  let simulate days (input: int []) =
    let arr = ResizeArray(input)
    for _ = 0 to days - 1 do
      advance arr
    int64 arr.Count

  let advance2 (days: int64[]) =
    let first = days[0]
    for i = 1 to days.Length - 1 do
      days[i - 1] <- days[i]
    days[6] <- days[6] + first
    days[8] <- first

  let simulate2 days fish =
    let daysLeft = Array.zeroCreate 9
    for f in fish do
      daysLeft[f] <- daysLeft[f] + 1L
    for _ = 0 to days - 1 do
      advance2 daysLeft
    Array.sum daysLeft

  let sample (result: int64) = makeSample result [|
    "3,4,3,1,2"
  |]

  [<Theory>]
  [<FileData(6, 374927L)>]
  [<MemberData(nameof sample, 5934L)>]
  let part1 (SingeLineInts input) expected =
    simulate 80 input =! expected
    simulate2 80 input =! expected

  [<Theory>]
  [<FileData(6, 1687617803407L)>]
  [<MemberData(nameof sample, 26984457539L)>]
  let part2 (SingeLineInts input) expected =
    simulate2 256 input =! expected

module Day07 =

  let getFuelCost(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> abs(p - i))
    pos
    |> Array.minBy getCost
    |> getCost

  let sample (result: int) = makeSample result [|
    "16,1,2,0,4,2,7,1,2,14"
  |]

  [<Theory>]
  [<FileData(7, 347509)>]
  [<MemberData(nameof sample, 37)>]
  let part1 (SingeLineInts input) expected =
    getFuelCost input =! expected

  let getFuelCost2(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> let n =  abs(p - i) in n * (n + 1) / 2)
    pos
    |> Array.minBy getCost
    |> getCost

  [<Theory>]
  [<FileData(7, 98257206)>]
  [<MemberData(nameof sample, 168)>]
  let part2 (SingeLineInts input) expected =
    getFuelCost2 input =! expected

module Day08 =

  let sample (result: int) = makeSample result [| |]

  [<Theory>]
  [<FileData(08, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(08, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day09 =

  let directions = [|
    +1,+0
    +0,-1
    +0,+1
    -1,+0
  |]

  let getLowPoints (input: string[]) =
    let height = input.Length
    let width = input[0].Length

    let get i j = if i < 0 || i >= height || j < 0 || j >= width then 'A' else input[i][j]

    let points = ResizeArray()

    for x = 0 to height - 1 do
      for y = 0 to width - 1 do
        let v = get x y
        let isLowest = directions |> Array.forall(fun (dx, dy) -> v < get (x + dx) (y + dy))
        if isLowest then points.Add(x, y)

    points.ToArray()

  let sample (result: int) = makeSample result [|
    "2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"
  |]

  [<Theory>]
  [<FileData(9, 570)>]
  [<MemberData(nameof sample, 15)>]
  let part1 input expected =
    getLowPoints input
    |> Array.sumBy(fun (x,y) -> int(input[x][y]) - int '0' + 1)
    =! expected

  let getBasinSize (input: string[]) point =
    let height = input.Length
    let width = input[0].Length
    let get (i, j) = if i < 0 || i >= height || j < 0 || j >= width then 'A' else input[i][j]

    let basin = HashSet<int*int>()

    let rec loop point =
      let (x, y) = point
      for (dx, dy) in directions do
        let point2 = dx + x, dy + y
        let ch = get point2
        if ch <> '9' && ch <> 'A' && basin.Add(point2) then
          loop point2

    basin.Add(point) |> ignore
    loop point
    basin.Count

  [<Theory>]
  [<FileData(9, 899392)>]
  [<MemberData(nameof sample, 1134)>]
  let part2 input expected =
    getLowPoints input
    |> Array.map(fun p -> getBasinSize input p)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce (*)
    =! expected

module Day10 =

  let sample (result: int) = makeSample result [|
    "[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]"
  |]

  let (|Open|Close|) ch = if ch = '(' || ch = '[' || ch = '{' || ch = '<' then Open ch else Close ch

  let isPair a b =
    (a = '(' && b = ')') ||
    (a = '[' && b = ']') ||
    (a = '{' && b = '}') ||
    (a = '<' && b = '>')

  let getOther ch =
    match ch with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> invalidArg (nameof ch) "Not a valid opening character."

  let rec loop left right =
    match left, right with
    | ch, [] -> Choice1Of2 ch
    | [], r :: right -> loop [r] right
    | l :: left, r :: right ->
      match l, r with
      | Open l, Close r when isPair l r -> loop left right
      | Open l, Close r -> Choice2Of2($"Corrupted, expected {getOther l}, got {r}.", r)
      | Open l, Open r -> loop (r :: l :: left) right
      | Close _, Open _ -> failwith "Cannot happen."
      | Close _, Close _ -> failwith "Cannot happen."

  let getScore ch =
    match ch with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> invalidArg (nameof ch) "Not a valid closing character."

  let getSyntaxErrorScore (input: string []) =
    input
    |> Array.partitionBy(List.ofSeq >> loop [])
    |> snd
    |> Array.sumBy(snd >> getScore)

  [<Theory>]
  [<FileData(10, 374061)>]
  [<MemberData(nameof sample, 26397)>]
  let part1 (input: string array) expected =
    getSyntaxErrorScore input =! expected

  let getScore2 ch =
    match ch with
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> invalidArg (nameof ch) "Not a valid opening character."

  let getCompletionScore (input: string []) =
    let scores =
      input
      |> Array.partitionBy(List.ofSeq >> loop [])
      |> fst
      |> Array.map(List.fold (fun score ch -> score * 5L + getScore2 ch) 0L)
      |> Array.sort
    scores[scores.Length / 2]

  [<Theory>]
  [<FileData(10, 2116639949)>]
  [<MemberData(nameof sample, 288957)>]
  let part2 input expected =
    getCompletionScore input =! expected

module Day11 =

  let sample (result: int) = makeSample result [|
    "5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"
  |]

  [<Theory>]
  [<FileData(11, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(11, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day12 =

  let sample (result: int) = makeSample result [|
    "start-A"
    "start-b"
    "A-c"
    "A-b"
    "b-d"
    "A-end"
    "b-end"
  |]

  [<Theory>]
  [<FileData(12, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(12, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day13 =

  let sample (result: int) = makeSample result [|
    "6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"
  |]

  [<Theory>]
  [<FileData(13, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(13, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day14 =

  let iterate (rules: Map<struct(char * char), char>) (polymer: StringBuilder) =
    let sb = StringBuilder(2 * polymer.Length - 1)
    for i = 0 to polymer.Length - 2 do
      sb.Append(polymer[i])
        .Append(rules[polymer[i], polymer[i + 1]])
        |> ignore
    sb.Append(polymer[polymer.Length - 1])

  let applyRules (input: string[]) =
    let rules =
      input
      |> Array.skip 2
      |> Array.map(fun s -> struct(s[0], s[1]), s[6])
      |> Map.ofArray

    let s =
      StringBuilder(input[0])
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
      |> iterate rules
    let counts = Dictionary<_, _>()
    for chunk in s.GetChunks() do
      let mutable exists = false
      for ch in chunk.Span do
        let mutable value = &CollectionsMarshal.GetValueRefOrAddDefault(counts, ch, &exists)
        value <- value + 1

    Seq.max counts.Values - Seq.min counts.Values

  let sample (result: int) = makeSample result [|
    "NNCB"
    ""
    "CH -> B"
    "HH -> N"
    "CB -> H"
    "NH -> C"
    "HB -> C"
    "HC -> B"
    "HN -> C"
    "NN -> C"
    "BH -> H"
    "NC -> B"
    "NB -> B"
    "BN -> B"
    "BB -> N"
    "BC -> B"
    "CC -> N"
    "CN -> C"
  |]

  [<Theory>]
  [<FileData(14, 2112)>]
  [<MemberData(nameof sample, 1588)>]
  let part1 input expected =
    applyRules input =! expected

  [<Theory>]
  [<FileData(14, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day15 =

  let sample (result: int) = makeSample result [|
    "1163751742"
    "1381373672"
    "2136511328"
    "3694931569"
    "7463417111"
    "1319128137"
    "1359912421"
    "3125421639"
    "1293138521"
    "2311944581"
  |]

  [<Theory>]
  [<FileData(15, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(15, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day16 =

  let sample (result: int) = makeSample result [|
    "0 = 0000"
    "1 = 0001"
    "2 = 0010"
    "3 = 0011"
    "4 = 0100"
    "5 = 0101"
    "6 = 0110"
    "7 = 0111"
    "8 = 1000"
    "9 = 1001"
    "A = 1010"
    "B = 1011"
    "C = 1100"
    "D = 1101"
    "E = 1110"
    "F = 1111"
  |]

  [<Theory>]
  [<FileData(16, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(16, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()


module Day17 =

  let sample (result: int) = makeSample result [|
    "target area: x=20..30, y=-10..-5"
  |]

  [<Theory>]
  [<FileData(17, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(17, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day18 =

  let sample (result: int) = makeSample result [|
    "[1,2]"
    "[[1,2],3]"
    "[9,[8,7]]"
    "[[1,9],[8,5]]"
    "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
    "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
    "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
  |]

  [<Theory>]
  [<FileData(18, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(18, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day19 =

  let sample (result: int) = makeSample result [|
    "--- scanner 0 ---"
    "0,2"
    "4,1"
    "3,3"
    ""
    "--- scanner 1 ---"
    "-1,-1"
    "-5,0"
    "-2,1"
  |]

  [<Theory>]
  [<FileData(19, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(19, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day20 =

  let sample (result: int) = makeSample result [|
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"
    "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###"
    ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#."
    ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....."
    ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.."
    "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....."
    "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
    ""
    "#..#."
    "#...."
    "##..#"
    "..#.."
    "..###"
  |]

  [<Theory>]
  [<FileData(20, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(20, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day21 =

  let sample (result: int) = makeSample result [|
    "Player 1 starting position: 4"
    "Player 2 starting position: 8"
  |]

  [<Theory>]
  [<FileData(21, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(21, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day22 =

  let sample (result: int) = makeSample result [|
    "on x=10..12,y=10..12,z=10..12"
    "on x=11..13,y=11..13,z=11..13"
    "off x=9..11,y=9..11,z=9..11"
    "on x=10..10,y=10..10,z=10..10"
  |]

  [<Theory>]
  [<FileData(22, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(22, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day23 =

  let sample (result: int) = makeSample result [|
    "#############"
    "#...........#"
    "###B#C#B#D###"
    "  #A#D#C#A#  "
    "  #########  "
  |]

  [<Theory>]
  [<FileData(23, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(23, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

module Day24 =

  let sample1 (result: int) = makeSample result [|
    "inp x"
    "mul x -1"
  |]

  let sample2 (result: int) = makeSample result [|
    "inp z"
    "inp x"
    "mul z 3"
    "eql z x"
  |]

  let sample3 (result: int) = makeSample result [|
    "inp w"
    "add z w"
    "mod z 2"
    "div w 2"
    "add y w"
    "mod y 2"
    "div w 2"
    "add x w"
    "mod x 2"
    "div w 2"
    "mod w 2"
  |]

  [<Theory>]
  [<FileData(24, 0)>]
  [<MemberData(nameof sample1, 0)>]
  [<MemberData(nameof sample2, 0)>]
  [<MemberData(nameof sample3, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(24, 0)>]
  [<MemberData(nameof sample1, 0)>]
  [<MemberData(nameof sample2, 0)>]
  [<MemberData(nameof sample3, 0)>]
  let part2 input expected =
    ()

module Day25 =

  let sample (result: int) = makeSample result [|
    "v...>>.vv>"
    ".vv>>.vv.."
    ">>.>v>...v"
    ">>v>>.>.v."
    "v>v.vv.v.."
    ">.>>..v..."
    ".vv..>.>v."
    "v.v..>>v.v"
    "....v..v.>"
  |]

  [<Theory>]
  [<FileData(25, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 input expected =
    ()

  [<Theory>]
  [<FileData(25, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 input expected =
    ()

