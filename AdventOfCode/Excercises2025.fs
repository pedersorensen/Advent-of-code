#if INTERACTIVE
#r "nuget: System.Drawing.Common"
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2025
#endif

open System
open System.Collections.Generic
open Xunit

#if INTERACTIVE
makeTemplate 2025 1 |> clip
#endif

module Day07 =

  let input = [|
    ".......S......."
    "..............."
    ".......^......."
    "..............."
    "......^.^......"
    "..............."
    ".....^.^.^....."
    "..............."
    "....^.^...^...."
    "..............."
    "...^.^...^.^..."
    "..............."
    "..^...^.....^.."
    "..............."
    ".^.^.^.^.^...^."
    "..............."
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 7, 1619)>]
  [<MemberData(nameof sample, 21)>]
  let part1 (input: string array) expected =
    let map = input |> Array.map _.ToCharArray()
    let splits = HashSet<int * int>()
    let tryMoveDown (x, y) =
      if x + 1 >= map.Length then
        []
      elif map[x + 1][y] = '.' then
        map[x + 1][y] <- '|'
        [x + 1, y]
      elif map[x + 1][y] = '|' then
        []
      else
        map[x + 1][y - 1] <- '|'
        map[x + 1][y + 1] <- '|'
        splits.Add((x + 1, y)) |> ignore
        [
          x + 1, y - 1
          x + 1, y + 1
        ]

    let start = input[0].IndexOf('S')
    let mutable p0 = [0, start]
    while not p0.IsEmpty do
      p0 <- p0 |> List.collect tryMoveDown
    printfn "\r\n%s" (map |> Array.map String |> String.concat "\r\n")

    splits.Count =! expected

  [<Theory>]
  [<FileData(2025, 7, 0)>]
  [<MemberData(nameof sample, 40)>]
  let part2 (input: string array) expected =
    let map = input |> Array.map _.ToCharArray()
    let splits = ResizeArray<int * int>()
    let tryMoveDown (x, y) =
      if x + 1 >= map.Length then
        []
      elif map[x + 1][y] = '.' then
        map[x + 1][y] <- '|'
        //printfn "\r\n%s" (map |> Array.map String |> String.concat "\r\n")
        [x + 1, y]
      elif map[x + 1][y] = '|' then
        []
      else
        //printfn "Split at %d,%d" (x + 1) y
        map[x + 1][y - 1] <- '|'
        map[x + 1][y + 1] <- '|'
        //printfn "\r\n%s" (map |> Array.map String |> String.concat "\r\n")
        splits.Add((x + 1, y)) |> ignore
        [
          //x, y
          x + 1, y - 1
          x + 1, y + 1
        ]

    let start = input[0].IndexOf('S')
    let mutable p0 = [0, start]
    let mutable doMore = true

    while doMore do
      let temp = p0
      p0 <- p0 |> List.collect tryMoveDown
      doMore <- p0 <> temp

    splits.Count
    =! expected

module Day06 =

  let input = [|
    "123 328  51 64 "
    " 45 64  387 23 "
    "  6 98  215 314"
    "*   +   *   +"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 6, 4405895212738L)>]
  [<MemberData(nameof sample, 4277556)>]
  let part1 (input: string array) expected =
    let parsed = input |> Array.map parseNumbers<int64>
    let operations, result =
      input[^0].ToCharArray()
      |> Array.choose(fun ch ->
        match ch with
        | '+' -> Some (ch, 0L)
        | '*' -> Some (ch, 1L)
        | _   -> None
      )
      |> Array.unzip
    parsed
    |> Array.iter(fun row ->
      for i = 0 to row.Length - 1 do
        match operations[i] with
        | '+' -> result[i] <- result[i] + row[i]
        | '*' -> result[i] <- result[i] * row[i]
        | _ -> ()
    )
    Array.sum result =! expected

  [<Theory>]
  [<FileData(2025, 6, 7450962489289L)>]
  [<MemberData(nameof sample, 3263827)>]
  let part2 (input: string array) expected =
    let l = input.Length - 1
    let parsed = input |> Array.take l |> Array.map _.ToCharArray()
    let zero = Array.create l ' '
    let numbers =
      parsed
      |> Array.transpose
      |> Array.chunkWhen(fun _ -> (<>) zero)
      |> Array.map(
         Array.choose(fun chars ->
          if chars = zero
          then None
          else Some (String chars |> int64)
         )
      )
    let operations = input[^0].Replace(" ", "").ToCharArray()
    (numbers, operations)
    ||> Array.sumBy2(fun row ->
      function
      | '+' -> row |> Array.sum
      | '*' -> row |> Array.fold(*) 1L
      | _   -> 0
    ) =! expected

module Day05 =

  let input = [|
    "3-5"
    "10-14"
    "16-20"
    "12-18"
    ""
    "1"
    "5"
    "8"
    "11"
    "17"
    "32"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 5,681 )>]
  [<MemberData(nameof sample, 3)>]
  let part1 (input: string array) expected =
    let splitAt = Array.findIndex(fun s -> s = "") input
    let ranges, ingredients = Array.splitAt splitAt input
    let ranges2 = ranges |> Array.map parseNumbers<int64>
    ingredients
    |> Array.sumBy(fun i ->
      if i = "" then 0 else
      let i = int64 i
      if ranges2 |> Array.exists(fun parts -> i >= parts[0] && i <= parts[1])
      then 1 else 0
    )
    =! expected

  let mergeRanges (ranges: int64 array array) =
    let sorted = ranges |> Array.sortBy(fun r -> r[0])
    let merged = ResizeArray<int64[]>()
    for r in sorted do
      if merged.Count = 0 then
        merged.Add(r)
      else
        let last = merged[merged.Count - 1]
        if r[0] <= last[1] + 1L
        then last[1] <- max last[1] r[1]
        else merged.Add(r)
    merged.ToArray()

  [<Theory>]
  [<FileData(2025, 5, 348820208020395L)>]
  [<MemberData(nameof sample, 14)>]
  let part2 (input: string array) expected =
    Array.takeWhile(fun s -> s <> "") input
    |> Array.map parseNumbers<int64>
    |> mergeRanges
    |> Array.sumBy(fun r -> r[1] - r[0] + 1L)
    =! expected

module Day04 =

  let input = [|
    "..@@.@@@@."
    "@@@.@.@.@@"
    "@@@@@.@.@@"
    "@.@@@@..@."
    "@@.@@@@.@@"
    ".@@@@@@@.@"
    ".@.@.@.@@@"
    "@.@@@.@@@@"
    ".@@@@@@@@."
    "@.@.@@@.@."
  |]

  let sample (result: int) = makeSample result input

  let allDirections = [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) |]

  [<Theory>]
  [<FileData(2025, 4, 1505)>]
  [<MemberData(nameof sample, 13)>]
  let part1 (input: string array) expected =
    let mutable count = 0
    for i in 0 .. input.Length - 1 do
      for j in 0 .. input[i].Length - 1 do
        if input[i][j] = '@' then
          let mutable sum = 0
          for (di, dj) in allDirections do
            let x = i + di
            let y = j + dj
            if x >= 0 && x < input.Length && y >= 0 && y < input[i].Length then
              if input[x][y] = '@' then
                sum <- sum + 1
          if sum < 4 then
            count <- count + 1
    count =! expected

  [<Theory>]
  [<FileData(2025, 4, 9182)>]
  [<MemberData(nameof sample, 43)>]
  let part2 (input: string array) expected =
    let rolls = input |> Array.map _.ToCharArray()
    let mutable removed = 0
    let toRemove = ResizeArray<int * int>()
    toRemove.Add((0,0))
    while toRemove.Count > 0 do
      toRemove.Clear()
      for i in 0 .. rolls.Length - 1 do
        for j in 0 .. rolls[i].Length - 1 do
          if rolls[i][j] = '@' then
            let mutable sum = 0
            for (di, dj) in allDirections do
              let x = i + di
              let y = j + dj
              if x >= 0 && x < rolls.Length && y >= 0 && y < rolls[i].Length then
                if rolls[x][y] = '@' then
                  sum <- sum + 1
            if sum < 4 then toRemove.Add((i, j))
      for (i, j) in toRemove do
        rolls[i][j] <- '.'
      removed <- removed + toRemove.Count
    removed =! expected

module Day03 =

  let input = [|
    "987654321111111"
    "811111111111119"
    "234234234234278"
    "818181911112111"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 3, 17074)>]
  [<MemberData(nameof sample, 357)>]
  let part1 (input: string array) expected =
    let ints = input |> Array.map (fun s -> s.ToCharArray() |> Array.map(fun ch -> int ch - int '0'))
    ints
    |> Array.sumBy(fun i ->
      let max1 = Span.indexOfMax(i.AsSpan(0, i.Length - 1)) + 1
      let max2 = Span.indexOfMax(i.AsSpan(max1, i.Length - max1))
      10 * i[max1 - 1] + i[max2 + max1]
    )
    =! expected

  [<Theory>]
  [<FileData(2025, 3, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day02 =

  let input = [|
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 2, 21139440284L)>]
  [<MemberData(nameof sample, 1227775554)>]
  let part1 (input: string array) expected =
    let numbers = parseNumbers<int64> input[0]
    let mutable sum = 0L
    for i in 0 .. numbers.Length / 2 - 1 do
      for i in numbers[2 * i] .. numbers[2 * i + 1] do
        let s = i.ToString()
        if s.Length % 2 = 0 then
          let half  = s.Length / 2
          let left  = s.AsSpan(0, half)
          let right = s.AsSpan(half, half)
          if left.SequenceEqual(right) then
            sum <- sum + i
    sum =! expected

  [<Theory>]
  [<FileData(2025, 2, -1)>]
  [<MemberData(nameof sample, 4174379265L)>]
  let part2 (input: string array) expected =
    let numbers = parseNumbers<int64> input[0]
    let mutable sum = 0L
    for i in 0 .. numbers.Length / 2 - 1 do
      for i in numbers[2 * i] .. numbers[2 * i + 1] do
        let s = i.ToString()
        if s.Length % 2 = 0 then
          let half = s.Length / 2
          let left = s.AsSpan(0, half)
          let right = s.AsSpan(half, half)
          if left.SequenceEqual(right) then
            sum <- sum + i
    sum =! expected

module Day01 =

  let input = [|
    "L68"
    "L30"
    "R48"
    "L5"
    "R60"
    "L55"
    "L1"
    "L99"
    "R14"
    "L82"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map(fun s ->
      let sign = if s[0] = 'L' then 1 else -1
      sign * Int32.Parse(s.AsSpan().Slice(1))
    )

  let dial parsed =
    ((50, 0), parsed)
    ||> Array.fold(fun (agg, zeroCount) next ->
      let value = (agg - next + 100) % 100
      let zeroCount = if value = 0 then zeroCount + 1 else zeroCount
      value, zeroCount
    )
    |> snd

  [<Theory>]
  [<FileData(2025, 1, 1066)>]
  [<MemberData(nameof sample, 3)>]
  let part1 (input: string array) expected =
    parse input |> dial =! expected

  [<Theory>]
  [<FileData(2025, 1, 6223)>]
  [<MemberData(nameof sample, 6)>]
  let part2 (input: string array) expected =
    parse input
    |> Array.collect(fun n ->
      let arr = Array.zeroCreate (abs n)
      Array.fill arr 0 arr.Length (sign n)
      arr
    )
    |> dial
    =! expected
