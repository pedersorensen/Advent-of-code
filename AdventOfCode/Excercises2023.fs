#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
Year <- 2023
#else
namespace Excercises2023
#endif

open Xunit
open System
open System.Buffers
open System.Text.RegularExpressions
open System.Collections.Generic

module Day06 =

  let input = [|
    "Time:      7  15   30"
    "Distance:  9  40  200"
  |]

  let sample (result: int) = makeSample result input

  let countDistances (totalTime: int64) record =
    let mutable count = 0
    for time = 0 to int totalTime - 1 do
      let speed = int64 time
      let distance = speed * (totalTime - int64 time)
      if distance > record then count <- count + 1
    count

  [<Theory>]
  [<FileData(2023, 6, 170000)>]
  [<MemberData(nameof sample, 288)>]
  let part1 (input: string []) expected =
    let times   = parseNumbers input[0]
    let records = parseNumbers input[1]

    (times, records)
    ||> Array.map2 countDistances
    |> Array.reduce (*) =! expected

  [<Theory>]
  [<FileData(2023, 6, 20537782)>]
  [<MemberData(nameof sample, 71503)>]
  let part2 (input: string []) expected =
    let times   = parseNumbers (input[0].Replace(" ", ""))
    let records = parseNumbers (input[1].Replace(" ", ""))
    (times, records)
    ||> Array.map2 countDistances
    |> Array.reduce (*) =! expected

module Day05 =

  let input = [|
    "seeds: 79 14 55 13"
    ""
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"
  |]

  let sample (result: int) = makeSample result input

  let parse input =
    ((Array.empty, List.empty, List.empty), input)
    ||> Array.fold(fun (seeds, maps, entries) (line: string) ->
      match line.Split([| ':' ;  ' ' |], StringSplitOptions.RemoveEmptyEntries) with
      | [||] -> (seeds, maps, entries)
      | a when a[0] = "seeds" ->
        let seeds = Array.skip 1 a |> Array.map int64
        seeds, maps, entries
      | [| _ ; "map" |] ->
        let maps' = if entries.IsEmpty then maps else entries :: maps
        seeds, maps', List.empty
      | [| dst ; src ; range |] ->
        seeds, maps, (int64 dst, int64 src, int64 range) :: entries
      | a -> failwithf "%A" a
    )
    |> fun (seeds, maps, list) -> seeds, List.rev(list :: maps)

  let findEntry n entries : int64 =
    entries
    |> List.tryPick(fun (dst, src, range) ->
      let d = n - src
      if src <= n && d < range then Some (d + dst) else None
    )
    |> Option.defaultValue n

  [<Theory>]
  [<FileData(2023, 5, 650599855)>]
  [<MemberData(nameof sample, 35)>]
  let part1 (input: string []) expected =
    let seeds, maps = parse input
    (Int64.MaxValue, seeds)
    ||> Array.fold(fun i seed ->
      (seed, maps)
      ||> List.fold(fun src entries -> findEntry src entries)
      |> min i
    )
    =! expected

  [<Theory>]
  [<FileData(2023, 5, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    0 =! expected

module Day04 =

  let input = [|
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2023, 4, 25183)>]
  [<MemberData(nameof sample, 13)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      match line.Split([| ':' ;  '|' |], StringSplitOptions.RemoveEmptyEntries) with
      | [| _card ; winners ; numbers|] ->
        let winners = winners.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let numbers = numbers.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let count = Set.intersect winners numbers |> Set.count
        pown 2 (count - 1)
      | _ -> failwithf ""
    ) =! expected

  [<Theory>]
  [<FileData(2023, 4, 5667240)>]
  [<MemberData(nameof sample, 30)>]
  let part2 (input: string []) expected =
    let cardsWon = Dictionary<string, int>()
    let increment n card =
      match cardsWon.TryGetValue(card) with
      | true, count -> cardsWon.[card] <- count + n
      | _ -> cardsWon.[card] <- n
      cardsWon.[card]

    input
    |> Array.sumBy(fun line ->
      match line.Split([| ':' ;  '|' |], StringSplitOptions.RemoveEmptyEntries) with
      | [| card ; winners ; numbers|] ->
        let cardId = card.Substring(5) |> int
        let card = $"Card {cardId}"
        let cardCount = increment 1 card
        let winners = winners.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let numbers = numbers.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let count = Set.intersect winners numbers |> Set.count
        for i = 1 to count do
          let card = $"Card {cardId + i}"
          increment cardCount card |> ignore
        cardCount
      | _ -> failwithf ""
    ) =! expected

module Day03 =

  let input = [|
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
  |]

  let sample (result: int) = makeSample result input

  let getNumbers input =
    input
    |> Array.mapi(fun i line -> [|
        for m in Regex.Matches(line, "[0-9]+") do
          let partNumberId = (i, m.Index)
          let value = (Int32.Parse(m.ValueSpan), partNumberId)
          for j = 0 to m.Length - 1 do
            (i, m.Index + j), value
      |]
    )
    |> Array.collect id
    |> Map.ofArray

  let allDirections = [|
    (-1, -1); (-1, 0); (-1, 1)
    ( 0, -1);          ( 0, 1)
    ( 1, -1); ( 1, 0); ( 1, 1)
  |]

  [<Theory>]
  [<FileData(2023, 3, 553079)>]
  [<MemberData(nameof sample, 4361)>]
  let part1 (input: string []) expected =
    let numbers       = getNumbers input
    let notPartNumber = SearchValues.Create("0123456789.")
    let seen          = HashSet()
    let mutable sum = 0
    for i = 1 to input.Length - 2 do
      for j = 1 to input.[i].Length - 2 do
        if notPartNumber.Contains(input[i].[j]) |> not then
          for (di, dj) in allDirections do
            match numbers.TryFind(i + di, j + dj) with
            | Some(value, partNumberId) when seen.Add(partNumberId) ->
              sum <- sum + value
            | _ -> ()
    sum =! expected

  [<Theory>]
  [<FileData(2023, 3, 84363105)>]
  [<MemberData(nameof sample, 467835)>]
  let part2 (input: string []) expected =
    let numbers     = getNumbers input
    let mutable sum = 0
    for i = 1 to input.Length - 2 do
      for j = 1 to input.[i].Length - 2 do
        if input[i].[j] = '*' then
          allDirections
          |> Array.choose(fun (di, dj) -> numbers.TryFind(i + di, j + dj))
          |> Array.distinctBy snd
          |> function
          | [| (v1, _); (v2, _) |] -> sum <- sum + v1 * v2
          | _ -> ()
    sum =! expected

module Day02 =

  let input = [|
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  |]

  let sample (result: int) = makeSample result input

  let bag = Map [|
    "red", 12
    "green", 13
    "blue", 14
  |]

  [<Theory>]
  [<FileData(2023, 2, 2447)>]
  [<MemberData(nameof sample, 8)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let split =
        line.Split([| ':' ;  ';' |], StringSplitOptions.RemoveEmptyEntries)
      let gameId = split[0].Substring(5) |> int
      let a =
        split
        |> Array.forall(fun l ->
          l.Split([| ',' ; ' ' |], StringSplitOptions.RemoveEmptyEntries)
          |> Array.chunkBySize 2
          |> Array.forall(fun a ->
            if a[0] = "Game" then true else
            let color = a[1]
            let count = int a[0]
            count <= bag[color]
          )
        )

      if a then gameId else 0
    )
     =! expected

  [<Theory>]
  [<FileData(2023, 2, 56322)>]
  [<MemberData(nameof sample, 2286)>]
  let part2 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let rounds =
        line.Split([| ':' ;  ';' ; ',' ; ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.chunkBySize 2

      (rounds, Map.empty)
      ||> Array.foldBack(fun a map ->
        if a[0] = "Game" then map else

        let color = a[1]
        let count = int a[0]

        map
        |> Map.change color (function
          | None -> Some count
          | Some c -> Some (max c count)
        )
      )
      |> Seq.fold(fun prod kvp -> prod * kvp.Value) 1
    ) =! expected

module Day01 =

  let sample1 (result: int) = makeSample result [|
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
  |]

  let digits = SearchValues.Create("0123456789")

  [<Theory>]
  [<FileData(2023, 1, 54_634)>]
  [<MemberData(nameof sample1, 142)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)
      10 * int(line[i] - '0') + int(line[j] - '0')
    ) =! expected

  let sample2 (result: int) = makeSample result [|
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
  |]

  let digitsAsWords =
    [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

  [<Theory>]
  [<FileData(2023, 1, 53855)>]
  [<MemberData(nameof sample2, 281)>]
  let part2 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)

      let indexes =
        digitsAsWords
        |> Array.mapi(fun value digit -> value, line.IndexOf(digit), line.LastIndexOf(digit))

      let minWordValue, i2, _ =
        indexes
        |> Array.minBy(fun (_, idx, _) ->
          if idx < 0 then Int32.MaxValue else idx
        )

      let minValue =
        if i < 0 || (-1 < i2 && i2 < i)
        then minWordValue
        else int(line[i] - '0')

      let maxWordValue, _, j2 = indexes |> Array.maxBy(fun (_, _, idx) -> idx)

      let maxValue =
        if j < 0 || (-1 < j2 && j2 > j)
        then maxWordValue
        else int(line[j] - '0')

      10 * minValue + maxValue
    )
    =! expected

#if INTERACTIVE

let makeTemplate day =

  sprintf """module Day%02i =

  let input = [|
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2023, %i, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string []) expected =
    0 =! expected

  [<Theory>]
  [<FileData(2023, %i, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    0 =! expected""" day day day

makeTemplate |> clip

#endif
