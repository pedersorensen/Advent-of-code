#load "Utils.fsx"

open Utils
open System
open System.Collections.Generic

Year <- 2021

module Day1 =

  let input = readInput 1 |> Array.map int

  let sample = [|
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
  |]

  let countIncreases (array: int[]) =
    let rec loop acc i =
      if i = array.Length then acc else
      let acc' =
        if array.[i] > array.[i - 1] then acc + 1
        else acc
      loop acc' (i + 1)
    loop 0 1
  countIncreases input

  // 1387
  let part1() =
    input
    |> Seq.pairwise
    |> Seq.where(fun (a, b) -> a < b)
    |> Seq.length

  let part1'() =
    countIncreases input

  // 1362
  let part2() =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countIncreases

module Day2 =

  let input = readInput 2

  let sample = [|
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
  |]

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

  // 2147104
  let part1() =
    moveIt input

  let moveItAgain directions =
    (struct(0, 0, 0), directions)
    ||> Array.fold(fun struct(h, d, aim) s ->
      match s with
      | Up x      -> h, d, aim - int x
      | Down x    -> h, d, aim + int x
      | Forward x -> h + int x, d + aim * int x, aim
    )
    |> fun struct(h, d, _) -> h * d

  // 2044620088
  let part2() =
    moveItAgain input

module Day3 =

  let input = readInput 3

  let sample = [|
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

  // 3847100
  let part1() =
    let (gamma, epsilon) = getReadings input
    gamma * epsilon

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

  // 4105235
  let part2() =
    let maxParam = ('0', '1')
    let minParam = ('1', '0')
    let oxygenRate = filter maxParam input |> binaryToInt
    let co2Rate = filter minParam input |> binaryToInt
    oxygenRate * co2Rate

module Day4 =

  let input = readInput 4

  let sample = [|
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

  type Board(values: int []) =
    do
      if values.Length <> 25
        then invalidArg (nameof values) "Input array must have length 25."
    let drawn = Array.zeroCreate<bool> 25
    member _.Values = values
    member _.Drawn = drawn
    member _.Draw(i) =
      let idx = Array.IndexOf(values, i)
      if idx > -1 then drawn[idx] <- true
    member this.DrawAndCheck(i) =
      this.Draw(i)
      this.HasWon
    member _.HasWon =
      let mutable hasWon = false
      for i = 0 to 4 do
        let j = 5 * i
        let isFullRow = drawn[j] && drawn[j + 1] && drawn[j + 2] && drawn[j + 3] && drawn[j + 4]
        let isFullColumn = drawn[i] && drawn[i + 5] && drawn[i + 10] && drawn[i + 15] && drawn[i + 20]
        hasWon <- hasWon || isFullColumn || isFullRow
      hasWon

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
      (board.Drawn, board.Values)
      ||> Array.map2(fun drawn value -> if drawn then 0 else value)
      |> Array.sum
    sum * number

  let getWinnerScore input =
    let boards, numbers = parse input
    numbers
    |> Array.pick(fun number ->
      for board in boards do board.Draw(number)
      boards
      |> Array.tryFind(fun b -> b.HasWon)
      |> Option.map(fun board -> board, number)
    )
    |> getScore

  // 5685
  let part1() =
    //getWinnerScore sample // 4512
    getWinnerScore input

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

  // 21070
  let part2() =
    //getLastWinnerScore sample // 1924
    getLastWinnerScore input

module Day5 =

  let input = readInput 5

  let sample = [|
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

  // 8622
  let part1() =
    //sample |> countOverlaps true
    input |> countOverlaps true

  // 22037
  let part2() =
    //sample |> countOverlaps false
    input |> countOverlaps false

module Day6 =

  let input = readInput 6 |> Array.exactlyOne

  let sample = "3,4,3,1,2"

  let parse (input: string) = input.Split(',') |> Array.map int

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

  let simulate days input =
    let arr = ResizeArray(parse input)
    for _ = 0 to days - 1 do
      advance arr
    arr.Count

  // 374927
  let part1() =
    //simulate 80 sample
    simulate 80 input

  let advance2 (days: int64[]) =
    let first = days[0]
    for i = 1 to days.Length - 1 do
      days[i - 1] <- days[i]
    days[6] <- days[6] + first
    days[8] <- first

  let simulate2 days input =
    let fish = parse input
    let daysLeft = Array.zeroCreate 9
    for f in fish do
      daysLeft[f] <- daysLeft[f] + 1L
    for _ = 0 to days - 1 do
      advance2 daysLeft
    Array.sum daysLeft

  // 1687617803407
  let part2() =
    //simulate2 80 sample
    //simulate2 256 sample
    //simulate2 80 input
    simulate2 256 input

module Day7 =

  let input = readsInts 7

  let sample = "16,1,2,0,4,2,7,1,2,14".Split(',') |> Array.map int

  let getFuelCost(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> abs(p - i))
    pos
    |> Array.minBy getCost
    |> getCost

  // 347509
  let part1() =
    //getFuelCost sample // 37
    getFuelCost input

  let getFuelCost2(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> let n =  abs(p - i) in n * (n + 1) / 2)
    pos
    |> Array.minBy getCost
    |> getCost

  // 98257206
  let part2() =
    //getFuelCost2 sample // 168
    getFuelCost2 input

module Day14 =
  open System.Text
  open System.Runtime.InteropServices

  let input = readInput 14

  let sample = [|
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

  // 2112
  let part1() =
    //applyRules sample // 1588
    applyRules input

