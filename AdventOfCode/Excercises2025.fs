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
open System.Text.RegularExpressions
open Xunit

#if INTERACTIVE
makeTemplate 2025 1 |> clip
#endif

module Day12 =

  let input = [|
    "0:"
    "###"
    "##."
    "##."
    ""
    "1:"
    "###"
    "##."
    ".##"
    ""
    "2:"
    ".##"
    "###"
    "##."
    ""
    "3:"
    "##."
    "###"
    "##."
    ""
    "4:"
    "###"
    "#.."
    "###"
    ""
    "5:"
    "###"
    ".#."
    "###"
    ""
    "4x4: 0 0 0 0 2 0"
    "12x5: 1 0 1 0 2 2"
    "12x5: 1 0 1 0 3 2"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 12, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2025, 12, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day11 =

  let input = [|
    "aaa: you hhh"
    "you: bbb ccc"
    "bbb: ddd eee"
    "ccc: ddd eee fff"
    "ddd: ggg"
    "eee: out"
    "fff: out"
    "ggg: out"
    "hhh: ccc fff iii"
    "iii: out"
  |]

  let input2 = [|
    "svr: aaa bbb"
    "aaa: fft"
    "fft: ccc"
    "bbb: tty"
    "tty: ccc"
    "ccc: ddd eee"
    "ddd: hub"
    "hub: fff"
    "eee: dac"
    "dac: fff"
    "fff: ggg hhh"
    "ggg: out"
    "hhh: out"
  |]

  let sample  (result: int) = makeSample result input
  let sample2 (result: int) = makeSample result input2

  let parse input =
    input
    |> Array.map(fun (s: string) ->
      let parts = s.Split(": ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
      parts[0], parts[1 .. ]
    )
    |> Map.ofArray

  let navigate initialNode map =
    let rec loop node acc =
      Map.find node map
      |> Array.sumBy(fun child ->
        if child = "out"
        then 1
        else loop child acc
      )
    loop initialNode 0

  type PathResult = {
    HasFft    : bool
    HasDac    : bool
    Count     : int64
    DacCount  : int64
    FftCount  : int64
    BothCount : int64
  }

  let navigate2 initialNode (map: Map<string, string array>) =
    let cache = Dictionary<string, PathResult>()
    let rec loop node =
      match cache.TryGetValue(node) with
      | true, paths -> paths
      | false, _ ->
        let isDac, isFft = node = "dac", node = "fft"
        let mutable hasDac, hasFft = isDac, isFft
        let mutable count, dacCount, fftCount, bothCount = 0L, 0L, 0L, 0L
        for child in map[node] do
          if child = "out"
          then count <- count + 1L
          else
            let res = loop child
            count     <- count     + res.Count
            hasFft    <- hasFft  ||  res.HasFft
            hasDac    <- hasDac  ||  res.HasDac
            dacCount  <- dacCount  + res.DacCount
            fftCount  <- fftCount  + res.FftCount
            bothCount <- bothCount + res.BothCount
        let result = {
          Count         = count
          HasDac        = hasDac
          HasFft        = hasFft
          DacCount  = if isDac then count else dacCount
          FftCount  = if isFft then count else fftCount
          BothCount = if   isDac && isFft then count
                      elif isDac then fftCount
                      elif isFft then dacCount
                      else bothCount
        }
        cache[node] <- result
        result
    loop initialNode

  [<Theory>]
  [<FileData(2025, 11, 497)>]
  [<MemberData(nameof sample, 5)>]
  let part1 (input: string array) expected =
    parse input |> navigate  "you" =! expected
    parse input |> navigate2 "you" |> _.Count =! expected

  [<Theory>]
  [<FileData(2025, 11, 358564784931864L)>]
  [<MemberData(nameof sample2, 2)>]
  let part2 (input: string array) expected =
    parse input |> navigate2 "svr" |> _.BothCount =! expected

module Day10 =

  let input = [|
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 10, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2025, 10, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day09 =

  let input = [|
    "7,1"
    "11,1"
    "11,7"
    "9,7"
    "9,5"
    "2,5"
    "2,3"
    "7,3"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 9, 4776487744L)>]
  [<MemberData(nameof sample, 50)>]
  let part1 (input: string array) expected =
    let coords = input |> Array.map(parseNumbers<int64> >> fun arr -> arr[0], arr[1])
    let mutable maxArea = 0L
    for i = 0 to coords.Length - 1 do
      let (x1, y1) = coords[i]
      for j = i + 1 to coords.Length - 1 do
        let (x2, y2) = coords[j]
        let dx = x1 - x2 + 1L
        let dy = y1 - y2 + 1L
        let area = dx * dy
        if area > maxArea then
          maxArea <- area
    maxArea =! expected

  let printGrid (grid: char[][]) =
    grid |> Array.iter(fun row -> row |> Array.iter(printf "%c") ; printfn "")

  let initGrid width height (coords: (int64 * int64) array) =
    let grid = Array.init height (fun _ -> Array.create width '.')
    for (x, y) in coords do grid[int y][int x] <- '#'
    grid

  let sortPairs (a: int64 * int64) (b: int64 * int64) =
    let (ax, ay) = a
    let (bx, by) = b
    if ax <> bx then
      if ax < bx then a, b else b, a
    else
      if ay < by then a, b else b, a

  let isWithinArea square (x, y) =
    let (x1, y1), (x2, y2) = square
    x > min x1 x2 && x < max x1 x2 &&
    y > min y1 y2 && y < max y1 y2

  [<Theory>]
  //[<FileData(2025, 9, 0)>] // 3904033833 Too high
  [<MemberData(nameof sample, 50)>]
  let part2 (input: string array) expected =
    let coords = input |> Array.map(parseNumbers<int64> >> fun arr -> arr[0], arr[1])

    let maxX, maxY =
      coords
      |> Array.reduce(fun (ax, ay) (bx, by) -> max ax bx, max ay by)

    let grid = initGrid (int maxX + 1) (int maxY + 1) coords
    printGrid grid

    let mutable maxArea = 0L
    for i = 0 to coords.Length - 1 do
      for j = i + 1 to coords.Length - 1 do
        let (x1, y1), (x2, y2) = sortPairs coords[i] coords[j]
        //let (x1, y1), (x2, y2) = sortPairs (9L,5L) (2L,3L)
        printfn "Checking points (%d,%d) and (%d,%d)" x1 y1 x2 y2
        let grid = initGrid (int maxX + 1) (int maxY + 1) coords
        let topLeft     = (min x1 x2, min y1 y2)
        let bottomRight = (max x1 x2, max y1 y2)
        let (x1, y1), (x2, y2) = topLeft, bottomRight
        grid[int y1][int x1] <- 'A'
        grid[int y2][int x2] <- 'B'
        printGrid grid
        let allOutside =
          coords
          |> Array.forall(fun coord ->
            not (isWithinArea ((x1, y1), (x2, y2)) coord)
          )

        //if not allOutside then
        //  let coordsWithinArea =
        //    coords
        //    |> Array.filter(isWithinArea ((x1, y1), (x2, y2)))
        //  for (x,y) in coordsWithinArea do grid[int y][int x] <- 'O'
        //  printfn ""
        //  printGrid grid
        if allOutside then
          let dx = abs(x1 - x2) + 1L
          let dy = abs(y1 - y2) + 1L
          let area = dx * dy
          printfn "All outside: %b, area: %i" allOutside area
          printfn ""
          if area > maxArea then
            maxArea <- area

    maxArea =! expected

module Day08 =

  let input = [|
    "162,817,812"
    "57,618,57"
    "906,360,560"
    "592,479,940"
    "352,342,300"
    "466,668,158"
    "542,29,236"
    "431,825,988"
    "739,650,466"
    "52,470,668"
    "216,146,977"
    "819,987,18"
    "117,168,530"
    "805,96,715"
    "346,949,466"
    "970,615,88"
    "941,993,340"
    "862,61,35"
    "984,92,344"
    "425,690,689"
  |]

  let sample (result: int) = makeSample result input

  let parse input =
    input
    |> Array.map(parseNumbers<int64> >> fun arr -> arr[0], arr[1], arr[2])
    |> Array.ownPairs
    |> Array.sortBy(fun ((x1, y1, z1), (x2, y2, z2)) ->
      let dx = x2 - x1
      let dy = y2 - y1
      let dz = z2 - z1
      dx * dx + dy * dy + dz * dz
    )

  let makeConnection (circuits: ResizeArray<HashSet<int64 * int64 * int64>>) (p1, p2) =
    let set1 = circuits |> Seq.tryFind(fun set -> set.Contains(p1))
    let set2 = circuits |> Seq.tryFind(fun set -> set.Contains(p2))
    match set1, set2 with
    | Some set1, Some set2 ->
      if set1 <> set2 then
        set1.UnionWith(set2)
        circuits.Remove(set2) |> ignore
        set1.Add(p1) |> ignore
        set1.Add(p2) |> ignore
    | Some set, None
    | None    , Some set ->
      set.Add(p1) |> ignore
      set.Add(p2) |> ignore
    | None, None -> circuits.Add(HashSet<_>([| p1 ; p2|]))

  [<Theory>]
  [<FileData(2025, 8, 140008)>]
  [<MemberData(nameof sample, 40)>]
  let part1 (input: string array) expected =
    let circuits = ResizeArray<HashSet<_>>()
    let take = if input.Length <= 20 then 10 else 1000
    parse input
    |> Seq.truncate take
    |> Seq.iter(makeConnection circuits)
    circuits
    |> Seq.map _.Count
    |> Seq.sortDescending
    |> Seq.truncate 3
    |> Seq.product
    =! expected

  [<Theory>]
  [<FileData(2025, 8, 9253260633L)>]
  [<MemberData(nameof sample, 25272)>]
  let part2 (input: string array) expected =
    let circuits = ResizeArray<HashSet<_>>()
    parse input
    |> Seq.map(fun points ->
      makeConnection circuits points
      points
    )
    |> Seq.pick(fun ((x1, _, _), (x2, _, _)) ->
      if circuits.Count = 1 && circuits[0].Count = input.Length
      then Some(x1 * x2)
      else None
    )
    =! expected

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

  let sample (result: int64) = makeSample result input

  let iteratePairs pred (numbers: int64 array) =
    let mutable sum = 0L
    for i in 0 .. numbers.Length / 2 - 1 do
      for i in numbers[2 * i] .. numbers[2 * i + 1] do
        if i.ToString() |> pred then
          sum <- sum + i
    sum

  [<Theory>]
  [<FileData(2025, 2, 21139440284L)>]
  [<MemberData(nameof sample, 1227775554L)>]
  let part1 (input: string array) expected =
    parseNumbers<int64> input[0]
    |> iteratePairs(fun s ->
      s.Length % 2 = 0 &&
      let half  = s.Length / 2
      let left  = s.AsSpan(0, half)
      let right = s.AsSpan(half, half)
      left.SequenceEqual(right)
    ) =! expected

  [<Theory>]
  [<FileData(2025, 2, 21139440284L)>]
  [<MemberData(nameof sample, 1227775554L)>]
  let part1Alternate (input: string array) expected =
    let regex = Regex(@"^(\d+?)\1$")
    parseNumbers<int64> input[0]
    |> iteratePairs regex.IsMatch =! expected

  [<Theory>]
  [<FileData(2025, 2, 38731915928L)>]
  [<MemberData(nameof sample, 4174379265L)>]
  let part2 (input: string array) expected =
    let regex = Regex(@"^(\d+?)\1+$")
    parseNumbers<int64> input[0]
    |> iteratePairs regex.IsMatch =! expected

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
