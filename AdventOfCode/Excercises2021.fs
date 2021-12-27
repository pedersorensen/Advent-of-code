namespace Excercises2021

open Xunit
open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Runtime.InteropServices

type FileDataAttribute(file: string, result: obj) =
  inherit Sdk.DataAttribute()
  override _.GetData(_) =
    let path = Path.Combine("input2021", file)
    [| [| File.ReadAllLines(path) |> box ; result |] |]

[<AutoOpen>]
module Utils =
  let (|Ints|) (data: string[]) = data |> Array.map int
  let (|SingeLineInts|) (data: string[]) = (Array.exactlyOne data).Split(',') |> Array.map int

  let inline (=!) (actual : 'T) (expected : 'T) = Assert.Equal<'T>(expected, actual)

module Day1 =

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

  [<Theory>]
  [<FileData("day1.txt", 1387)>]
  [<FileData("day1sample.txt", 7)>]
  let part1 (Ints input) expected =
    countIncreasesA input =! expected
    countIncreasesB input =! expected

  [<Theory>]
  [<FileData("day1.txt", 1362)>]
  [<FileData("day1sample.txt", 5)>]
  let part2 (Ints input) expected =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countIncreasesB
     =! expected

module Day2 =

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

  [<Theory>]
  [<FileData("day2.txt", 2147104)>]
  [<FileData("day2sample.txt", 150)>]
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
  [<FileData("day2.txt", 2044620088)>]
  [<FileData("day2sample.txt", 900)>]
  let part2 input expected =
    moveItAgain input =! expected

module Day3 =

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

  [<Theory>]
  [<FileData("day3.txt", 3847100)>]
  [<FileData("day3sample.txt", 198)>]
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
  [<FileData("day3.txt", 4105235)>]
  [<FileData("day3sample.txt", 230)>]
  let part2 input expected =
    let maxParam = ('0', '1')
    let minParam = ('1', '0')
    let oxygenRate = filter maxParam input |> binaryToInt
    let co2Rate = filter minParam input |> binaryToInt
    oxygenRate * co2Rate =! expected

module Day4 =

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

  [<Theory>]
  [<FileData("day4.txt", 5685)>]
  [<FileData("day4sample.txt", 4512)>]
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
  [<FileData("day4.txt", 21070)>]
  [<FileData("day4sample.txt", 1924)>]
  let part2 input expected =
    getLastWinnerScore input =! expected

module Day5 =

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

  [<Theory>]
  [<FileData("day5.txt", 8622)>]
  [<FileData("day5sample.txt", 5)>]
  let part1 input expected =
    input |> countOverlaps true =! expected

  [<Theory>]
  [<FileData("day5.txt", 22037)>]
  [<FileData("day5sample.txt", 12)>]
  let part2 input expected =
    input |> countOverlaps false =! expected

module Day6 =

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
    arr.Count

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

  [<Theory>]
  [<FileData("day6.txt", 374927)>]
  [<FileData("day6sample.txt", 5934)>]
  let part1 (SingeLineInts input) expected =
    simulate 80 input =! expected
    simulate2 80 input =! expected

  [<Theory>]
  [<FileData("day6.txt", 1687617803407L)>]
  [<FileData("day6sample.txt", 26984457539L)>]
  let part2 (SingeLineInts input) expected =
    simulate2 256 input =! expected

module Day7 =

  let getFuelCost(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> abs(p - i))
    pos
    |> Array.minBy getCost
    |> getCost

  [<Theory>]
  [<FileData("day7.txt", 347509)>]
  [<FileData("day7sample.txt", 37)>]
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
  [<FileData("day7.txt", 98257206)>]
  [<FileData("day7sample.txt", 168)>]
  let part2 (SingeLineInts input) expected =
    getFuelCost2 input =! expected

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

  [<Theory>]
  [<FileData("day14.txt", 2112)>]
  [<FileData("day14sample.txt", 1588)>]
  let part1 input expected =
    applyRules input =! expected
