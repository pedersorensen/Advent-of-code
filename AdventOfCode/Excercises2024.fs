#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2024
#endif

open Xunit
open System
open System.Buffers
open System.Collections.Generic
open System.Text.RegularExpressions

module Day10 =

  let input = [|
    "89010123"
    "78121874"
    "87430965"
    "96549874"
    "45678903"
    "32019012"
    "01329801"
    "10456732"
  |]

  let sample (result: int) = makeSample result input

  let directions = [|
    1,   0
    0,  -1
    -1,  0
    0,   1
  |]

  let get (input: string array) (i, j) = directions |> Array.choose(fun (dx, dy) ->
    let one = '1' - '0'
    try
      let i', j' = i + dx, j + dy
      if input[i'][j'] - input[i][j] = one
      then Some(i', j')
      else None
    with :? IndexOutOfRangeException -> None
  )

  let run distinct (input: string array) =
    let mutable sum = 0
    for i = 0 to input.Length - 1 do
      let line = input[i]
      for j = 0 to line.Length - 1 do
        if line[j] = '0' then
          let rec loop p = seq {
            for p in get input p do
              yield p
              yield! loop p
          }
          let count =
            loop (i, j)
            |> fun path -> if distinct then path |> Seq.distinct else path
            |> Seq.countTrue(fun (i, j) -> input[i][j]  = '9')
          sum <- sum + count
    sum

  [<Theory>]
  [<FileData(2024, 10, 816)>]
  [<MemberData(nameof sample, 36)>]
  let part1 (input: string array) expected =
    run true input=! expected

  [<Theory>]
  [<FileData(2024, 10, 1960)>]
  [<MemberData(nameof sample, 81)>]
  let part2 (input: string array) expected =
    run false input=! expected

module Day08 =

  let input = [|
    "............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"
  |]

  let sample (result: int) = makeSample result input

  let getNodes (input: string array) =
    let nodes = Dictionary()
    for i = 0 to input.Length - 1 do
      let line = input[i]
      for j = 0 to line.Length - 1 do
        let ch = line[j]
        if ch <> '.' then
          let values =
            match nodes.TryGetValue(ch) with
            | true, values -> values
            | false, _     -> [ ]
          nodes[ch] <- Point(i, j) :: values
    nodes.Values

  [<Theory>]
  [<FileData(2024, 8, 423)>]
  [<MemberData(nameof sample, 14)>]
  let part1 (input: string array) expected =
    let min, max = Point(0, 0), Point(input.Length, input[0].Length)
    getNodes input
    |> Seq.collect(fun nodes ->
      Seq.allPairs nodes nodes
      |> Seq.collect(fun (p1, p2) ->
        if p1 <= p2 then
          Seq.empty
        else
          let dx = p2 - p1
          [|
            p1 - dx
            p2 + dx
          |]
      )
    )
    |> Seq.filter(fun p -> min <= p && p < max)
    |> Seq.countDistinct
    =! expected

  [<Theory>]
  [<FileData(2024, 8, 1287)>]
  [<MemberData(nameof sample, 34)>]
  let part2 (input: string array) expected =
    let min, max = Point(0, 0), Point(input.Length, input[0].Length)
    getNodes input
    |> Seq.collect(fun nodes ->
      Seq.allPairs nodes nodes
      |> Seq.collect(fun (p1, p2) ->
        if p1 <= p2 then
          Seq.empty
        else
          let dx = p2 - p1
          let first =
            Seq.initInfinite (fun i -> p1 - i * dx)
            |> Seq.takeWhile(fun p -> min <= p && p < max)
          let second =
            Seq.initInfinite (fun i -> p2 + i * dx)
            |> Seq.takeWhile(fun p -> min <= p && p < max)
          Seq.append first second
      )
    )
    |> Seq.countDistinct
    =! expected

module Day07 =

  let input = [|
    "190: 10 19"
    "3267: 81 40 27"
    "83: 17 5"
    "156: 15 6"
    "7290: 6 8 6 15"
    "161011: 16 10 13"
    "192: 17 8 14"
    "21037: 9 7 18 13"
    "292: 11 6 16 20"
  |]

  let sample (result: int64) = makeSample result input

  let parse input =
    input
    |> Array.map(fun (x:string) ->
      x.Split([| ' ' ; ':'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int64
    )

  [<Theory>]
  [<FileData(2024, 7, 6392012777720L)>]
  [<MemberData(nameof sample, 3749L)>]
  let part1 (input: string array) expected =

    let test (line : _ array) =
      let result = line[0]
      let rec expand i acc = seq {
        if i < line.Length && acc <= result then
          let x = line[i]
          let i = i + 1
          yield! expand i (acc + x)
          yield! expand i (acc * x)
        else yield acc
      }
      expand 2 line[1]
      |> Seq.tryFind ((=) result)

    parse input
    |> Array.sumBy(fun line -> test line |> Option.defaultValue 0L)
    =! expected

  [<Theory>]
  [<FileData(2024, 7, 61561126043536L)>]
  [<MemberData(nameof sample, 11387L)>]
  let part2 (input: string array) expected =

    let concat a b =
      b + a * int64(Math.Pow(10, Math.Ceiling(Math.Log10(float b + 1.))))

    let test (line : _ array) =
      let result = line[0]
      let rec expand i acc = seq {
        if i < line.Length && acc <= result then
          let x = line[i]
          let i = i + 1
          yield! expand i (acc + x)
          yield! expand i (acc * x)
          yield! expand i (concat acc x)
        else yield acc
      }
      expand 2 line[1]
      |> Seq.tryFind ((=) result)

    parse input
    |> Array.sumBy(fun line -> test line |> Option.defaultValue 0L)
    =! expected

module Day06 =

  let input = [|
    "....#....."
    ".........#"
    ".........."
    "..#......."
    ".......#.."
    ".........."
    ".#..^....."
    "........#."
    "#........."
    "......#..."
  |]

  let sample (result: int) = makeSample result input

  let getStartingPoint (input: string array) =
    let x = input |> Array.findIndex(fun x -> x.Contains('^'))
    let y = input[x].AsSpan().IndexOf('^')
    x, y

  let move (map: char array array) p0 =
    let set = HashSet()
    let rec loop direction (x, y) =
      if set.Add(x, y, direction) then
        map[x][y] <- 'X'
        let x2, y2, direction2 =
          match direction with
          | 'u' -> x - 1, y, 'l'
          | 'd' -> x + 1, y, 'r'
          | 'l' -> x, y + 1, 'd'
          | 'r' -> x, y - 1, 'u'
          | _ -> failwith "Invalid direction"
        if x2 >= 0 && x2 < map.Length && y2 >= 0 && y2 < map[0].Length then
          if map[x2][y2] = '#'
          then loop direction2 (x, y)
          else loop direction (x2, y2)
        else 0
      else 1
    loop 'u' p0

  [<Theory>]
  [<FileData(2024, 6, 4722)>]
  [<MemberData(nameof sample, 41)>]
  let part1 (input: string array) expected =
    let map = input |> Array.map(fun x -> x.ToCharArray())
    getStartingPoint input |> move map =! 0
    map
    |> Array.sumBy(Array.countTrue ((=) 'X'))
    =! expected

  [<Theory>]
  [<FileData(2024, 6, 1602)>]
  [<MemberData(nameof sample, 6)>]
  let part2 (input: string array) expected =
    let p0 = getStartingPoint input
    let map0 = input |> Array.map(fun x -> x.ToCharArray())
    move map0 p0 =! 0
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[0].Length - 1 do
        if input[i][j] = '.' && map0[i][j] = 'X' then
          let map = input |> Array.map(fun x -> x.ToCharArray())
          map[i][j] <- '#'
          count <- count + move map p0
    count =! expected

module Day05 =

  let input = [|
    "47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
    ""
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    let rules, updates =
      input
      |> Array.map parseNumbers<int>
      |> Array.partition(fun a -> a.Length = 2)
    let postRules = rules |> Array.groupBy(Array.item 0) |> Array.map(fun (ch, grp) -> ch, grp |> Array.map(Array.item 1) |> Set.ofArray) |> Map.ofArray
    let preRules  = rules |> Array.groupBy(Array.item 1) |> Array.map(fun (ch, grp) -> ch, grp |> Array.map(Array.item 0) |> Set.ofArray) |> Map.ofArray
    preRules, postRules, updates

  let (|||) option defaultValue = option |> Option.defaultValue defaultValue

  [<Theory>]
  [<FileData(2024, 5, 5732)>]
  [<MemberData(nameof sample, 143)>]
  let part1 (input: string array) expected =
    let preRules, postRules, updates = parse input
    updates
    |> Array.sumBy(fun update ->
      let isValid =
        update.Length > 0 &&
        update
        |> Array.indexed
        |> Array.forall(fun (i, ch) ->
          let preRule  =  preRules.TryFind(ch) ||| Set.empty
          let postRule = postRules.TryFind(ch) ||| Set.empty
          Seq.init update.Length id
          |> Seq.forall(fun j ->
               i = j
            || j < i &&  preRule.Contains(update[j])
            || j > i && postRule.Contains(update[j])
          )
        )
      if isValid then update[update.Length / 2] else 0
    ) =! expected

  [<Theory>]
  [<FileData(2024, 5, 4716)>]
  [<MemberData(nameof sample, 123)>]
  let part2 (input: string array) expected =
    let preRules, postRules, updates = parse input
    updates
    |> Array.sumBy(fun update ->
      let isValid =
        update
        |> Array.indexed
        |> Array.forall(fun (i, ch) ->
          let preRule  =  preRules.TryFind(ch) ||| Set.empty
          let postRule = postRules.TryFind(ch) ||| Set.empty
          Seq.init update.Length id
          |> Seq.forall(fun j ->
            i = j
            || j < i &&  preRule.Contains(update[j])
            || j > i && postRule.Contains(update[j])
          )
        )
      if isValid then 0 else
        update
        |> Array.sortInPlaceWith(fun a b ->
          if (postRules.TryFind(a) ||| Set.empty).Contains(b) && (preRules.TryFind(b) ||| Set.empty).Contains(a) then -1 else 1
        )
        update[update.Length / 2]
    ) =! expected

module Day04 =

  let input = [|
    "MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"
  |]

  let directions = [|
    0,  1
    1,  1
    1,  0
    1, -1
    0, -1
    -1, -1
    -1,  0
    -1,  1
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 4, 2496)>]
  [<MemberData(nameof sample, 18)>]
  let part1 (input: string array) expected =
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[i].Length - 1 do
        if input[i][j] = 'X' then
          let subCount =
            directions
            |> Array.countTrue(fun (dx, dy) ->
              try  input[i + 1 * dx][j + 1 * dy] = 'M'
                && input[i + 2 * dx][j + 2 * dy] = 'A'
                && input[i + 3 * dx][j + 3 * dy] = 'S'
              with :? IndexOutOfRangeException -> false
            )
          count <- count + subCount
    count =! expected

  [<Theory>]
  [<FileData(2024, 4, 1967)>]
  [<MemberData(nameof sample, 9)>]
  let part2 (input: string array) expected =
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[i].Length - 1 do
        if input[i][j] = 'A' then
          try
            let xp, xm = input[i + 1], input[i - 1]
            let ch1, ch2  = xp[j + 1], xm[j - 1]
            if (ch1 = 'M' && ch2 = 'S') || (ch1 = 'S' && ch2 = 'M') then
              let ch3, ch4  = xm[j + 1], xp[j - 1]
              if (ch3 = 'M' && ch4 = 'S') || (ch3 = 'S' && ch4 = 'M') then
                count <- count + 1
          with :? IndexOutOfRangeException -> ()
    count =! expected

module Day03 =

  let input = [|
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  |]

  let sample (result: int) = makeSample result input

  let sumMatch (m: Match) =
    let d1 = m.Groups["d1"].Value |> int
    let d2 = m.Groups["d2"].Value |> int
    d1 * d2

  [<Theory>]
  [<FileData(2024, 3, 185797128)>]
  [<MemberData(nameof sample, 161)>]
  let part1 (input: string array) expected =
    Regex.Matches(input[0], "mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)")
    |> Seq.sumBy sumMatch
     =! expected

  let input2 = [|
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  |]

  let sample2 (result: int) = makeSample result input2

  [<Theory>]
  [<FileData(2024, 3, 89798695)>]
  [<MemberData(nameof sample2, 48)>]
  let part2 (input: string array) expected =
    ((0, true), Regex.Matches(input[0], "(mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)|do\(\)|don't\(\))"))
    ||> Seq.fold(fun (sum, doIt) m ->
      if   m.Value = "do()"    then sum, true
      elif m.Value = "don't()" then sum, false
      else
        if doIt
        then sum + sumMatch m, doIt
        else sum, doIt
    )
    |> fst
     =! expected

module Day02 =

  let input = [|
    "7 6 4 2 1"
    "1 2 7 8 9"
    "9 7 6 2 1"
    "1 3 2 4 5"
    "8 6 4 4 1"
    "1 3 6 7 9"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map (fun x ->
      x.Split(' ', StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int
    )

  let isSafe array =
    let pairs = array |> Array.pairwise |> Array.map (fun (a, b) -> a - b)
    (    pairs |> Array.forall (fun x -> x > 0)
      || pairs |> Array.forall (fun x -> x < 0)
    ) && pairs |> Array.forall (fun x -> abs x > 0 && abs x < 4)

  [<Theory>]
  [<FileData(2024, 2, 356)>]
  [<MemberData(nameof sample, 2)>]
  let part1 (input: string array) expected =
    parse input
    |> Array.countTrue isSafe
    =! expected

  let dampen array = seq {
    let l = Array.length array
    let result = Array.zeroCreate (l - 1)
    Array.Copy(array, 1, result, 0, l - 1)
    yield result
    let mutable x = array[0]
    for i = 0 to l - 2 do
      let temp = x
      x <- result[i]
      result[i] <- temp
      yield result
  }

  [<Theory>]
  [<FileData(2024, 2, 413)>]
  [<MemberData(nameof sample, 4)>]
  let part2 (input: string array) expected =
    parse input
    |> Array.countTrue(fun x -> isSafe x || dampen x |> Seq.exists isSafe)
    =! expected

module Day01 =

  let input = [|
    "3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map (fun x ->
      match x.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
      | [| a; b |] -> int a, int b
      | _ -> failwith "Invalid input"
    )
    |> Array.unzip

  [<Theory>]
  [<FileData(2024, 1, 2756096)>]
  [<MemberData(nameof sample, 11)>]
  let part1 (input: string array) expected =
    let left, right = parse input
    Array.sortInPlace left
    Array.sortInPlace right
    (left, right)
    ||> Array.sumBy2(fun a b -> abs (a - b))
    =! expected

  [<Theory>]
  [<FileData(2024, 1, 23117829)>]
  [<MemberData(nameof sample, 31)>]
  let part2 (input: string array) expected =
    let left, right = parse input
    let map = right |> Array.countBy id |> Map.ofArray
    left
    |> Array.sumBy(fun x -> x * (map.TryGetValue(x) |> snd) )
    =! expected

#if INTERACTIVE

let makeTemplate day =

  let sample = paste().Trim().Replace("\r\n", "\"\r\n    \"")

  $"""module Day%02i{day} =

  let input = [|
    "{sample}"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

"""

makeTemplate 8 |> clip

#endif
