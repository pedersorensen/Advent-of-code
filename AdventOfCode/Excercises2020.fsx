#load "Utils.fsx"

open Utils
open System

Year <- 2020

module Seq =

  let batchOnNewline combiner accumulator init array =
    (init, array)
    ||> Seq.fold(fun (state, acc) line ->
      if String.IsNullOrWhiteSpace line
      then fst init, accumulator state acc
      else combiner line state, acc)
    |> fun (set, count) -> accumulator set count

let cons head tail = head :: tail

module String =

  let splitAt (char: char) (s: string) = s.Split(char)

  let cutAt (char: char) (s: string) =
    let i = s.IndexOf(char)
    s.Substring(0, i), s.Substring(i + 1)

module Map =
  let addTuple (key, value) map =
    Map.add key value map

module Day1 =
  let input = readInput 1 |> Array.map int |> set

  let tryFindMatch sum s1 =
    let s2 = s1 |> Set.map(fun i -> sum - i)
    let i = Set.intersect s1 s2
    if i.Count <> 2 then None else
    match Set.toArray i with
    | [| a ; b |] -> Some(a, b)
    | _ -> failwith "Expected an array of two elements."

  // 691771
  let part1() =
    tryFindMatch 2020 input
    |> Option.iter(fun (a, b) ->
      printfn "Product: %i * %i : %i" a b (a * b))

  // 232508760
  let part2() =
    for i in input do
      input
      |> Set.remove i
      |> tryFindMatch (2020 - i)
      |> Option.iter(fun (a, b) ->
        printfn "Product: %i * %i * %i : %i" i a b (i * a * b))

module Day2 =
  let input = readInput 2

  let parsed =
    input
    |> Array.map(fun s ->
      match s.Split([|' ' ; ':' ; '-'|], StringSplitOptions.RemoveEmptyEntries) with
      | [|min ; max ; ch ; pw|] ->
        int min, int max, char ch, pw
      | _ -> failwith "Incorrect input"
    )

  // 628
  let part1() =
    parsed
    |> Array.countTrue(fun (min, max, char, pw) ->
      let count = pw |> Seq.countTrue((=) char)
      min <= count && count <= max
    )

  // 705
  let part2() =
    parsed
    |> Array.countTrue(fun (p1, p2, char, pw) ->
      let b1 = pw.[p1 - 1] = char
      let b2 = pw.[p2 - 1] = char
      b1 <> b2
    )

module Day3 =

  let [<Literal>] TreeMarker = '#'

  let countTrees (input: string[]) slope =
    let mutable p = (0,0)
    let w = input.[0].Length
    let mutable count = 0
    while snd p < input.Length do
      let (x, y) = p
      if input.[y].[x % w] = TreeMarker then count <- count + 1
      p <- Tuple.add p slope
    int64 count

  let countTrees2 (input: string[]) slope =
    let w = input.[0].Length
    (0,0)
    |> Seq.unfold(fun p ->
      if snd p >= input.Length then None else
      Some(p, Tuple.add slope p))
    |> Seq.countTrue(fun (x, y) -> input.[y].[x % w] = TreeMarker)
    |> int64

  let input = readInput 3

  let sample = [|
    "..##......."
    "#...#...#.."
    ".#....#..#."
    "..#.#...#.#"
    ".#...##..#."
    "..#.##....."
    ".#.#.#....#"
    ".#........#"
    "#.##...#..."
    "#...##....#"
    ".#..#...#.#"
  |]

  // 216
  let part1() =
    let slope = (3, 1)
    let s1 = countTrees sample slope
    let s2 = countTrees2 sample slope
    let c1 = countTrees input slope
    let c2 = countTrees2 input slope
    c2

  // 6708199680
  let part2() =
    let slopes = [|
      1, 1
      3, 1
      5, 1
      7, 1
      1, 2
    |]
    let s1 = slopes |> Array.map(countTrees2 sample) |> Array.reduce (*)
    let p1 = slopes |> Array.map(countTrees2 input) |> Array.reduce (*)
    let p2 = (1L, slopes) ||> Array.fold(fun p slope -> countTrees2 input slope * p)
    let p3 = (slopes, 1L) ||> Array.foldBack(countTrees2 input >> (*))
    p3

module Day4 =
  open System.Text.RegularExpressions

  let parse input = seq {
    let mutable d = ResizeArray();
    for line in input do
      if String.IsNullOrWhiteSpace line then
        yield Map.ofSeq d
        d <- ResizeArray()
      else
        for s in line.Split(' ') do
          let i = s.IndexOf(':')
          d.Add(s.Substring(0, i), s.Substring(i + 1))
    yield Map.ofSeq d
  }

  let parse2 input =
    ((Map.empty, []), input)
    ||> Seq.batchOnNewline
      (String.splitAt ' ' >> Array.foldBack(String.cutAt ':' >> Map.addTuple)) cons

  let isNumberInRange min max s =
    match Int32.TryParse(s) with
    | true, n when min <= n && n <= max -> true
    | _ -> false

  let isMatch pattern value = Regex.IsMatch(value, pattern)

  let isHeight value =
    let m = Regex.Match(value, "([0-9]+)(cm|in)")
    if not m.Success then false else
    let h = Int32.Parse m.Groups.[1].Value
    match m.Groups.[2].Value with
    | "cm" when 150 <= h && h <= 193 -> true
    | "in" when  59 <= h && h <=  76 -> true
    | _ -> false

  let requiredFields = [|
    "byr", isNumberInRange 1920 2002
    "iyr", isNumberInRange 2010 2020
    "eyr", isNumberInRange 2020 2030
    "hgt", isHeight
    "hcl", isMatch "^#[0-9a-f]{6}$"
    "ecl", isMatch "^(amb|blu|brn|gry|grn|hzl|oth)$"
    "pid", isMatch "^[0-9]{9}$"
    //"cid", fun _ -> true
  |]

  let input = readInput 4 |> parse |> Seq.toList
  let input2 = readInput 4 |> parse2

  // 242
  let part1() =
    input2
    |> Seq.countTrue(fun d ->
      requiredFields |> Array.forall(fst >> d.ContainsKey)
    )

  let validate passport =
    requiredFields
    |> Array.forall(fun (field, validator) ->
      passport
      |> Map.tryFind field
      |> Option.map validator
      |> Option.defaultValue false
    )

  let countValid passports =
    passports |> Seq.countTrue validate

  let invalid = parse [|
    "eyr:1972 cid:100"
    "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    ""
    "iyr:2019"
    "hcl:#602927 eyr:1967 hgt:170cm"
    "ecl:grn pid:012533040 byr:1946"
    ""
    "hcl:dab227 iyr:2012"
    "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
    ""
    "hgt:59cm ecl:zzz"
    "eyr:2038 hcl:74454a iyr:2023"
    "pid:3556412378 byr:2007"
  |]

  let valid = parse [|
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
    "hcl:#623a2f"
    ""
    "eyr:2029 ecl:blu cid:129 byr:1989"
    "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    ""
    "hcl:#888785"
    "hgt:164cm byr:2001 iyr:2015 cid:88"
    "pid:545766238 ecl:hzl"
    "eyr:2022"
    ""
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  |]

  // 186
  let part2() =
    let validCount = countValid valid // 4
    let invalidCount = countValid invalid // 0
    countValid input

module Day5 =
  let input = readInput 5

  let find low high input =
    ((low, high), input)
    ||> Seq.fold(fun (low, high) ch ->
      let mid = (high + low) / 2
      match ch with
      | 'F' | 'L' -> low, mid
      | 'B' | 'R' -> mid + 1, high
      | _ ->
        invalidArg (nameof input) "Input must be 'F', 'B', 'L' or 'R'."
    )
    |> fst

  let getSeat (seat: string) =
    if seat.Length <> 10 then
      invalidArg (nameof seat) "String must be of length 10."
    let row = seat.Substring(0, 7) |> find 0 127
    let column = seat.Substring(7) |> find 0 7
    row, column

  let getSeatId (row, column) = 8 * row + column

  // Splitting the seat string in two is not necessary due to the constuction
  // of the seat id, row * 8 + column
  let getSeatId2 seat = find 0 1023 seat

  // Even simpler, interpret the seat specification as a binary number
  let getSeatId3 seat =
    seat
    |> Seq.mapi(fun i ch -> if ch = 'F' || ch = 'L' then 0 else 512 >>> i )
    |> Seq.sum

  let seats = input |> Array.map getSeat

  // 838
  let part1() =
    seats |> Array.map getSeatId |> Array.max

  let part1'() =
    input |> Array.map getSeatId3 |> Array.max

  // 714
  let part2() =
    let minRow = seats |> Array.minBy fst |> fst
    let maxRow = seats |> Array.maxBy fst |> fst
    let (row, seats) =
      seats
      |> Array.groupBy fst
      |> Array.filter(fun (row, group) ->
        group.Length <> 8 && row <> minRow && row <> maxRow)
      |> Array.exactlyOne
    let taken = seats |> Array.map snd |> set
    let column = set [| 0 .. 7 |] - taken |> Seq.exactlyOne
    getSeatId(row, column)

  let part2'() =
    // Since all the seats are taken, all seat ids will be present except the
    // missing seat (hours), find the first non-contiguous entry
    input
    |> Array.map getSeatId3
    |> Array.sort
    |> Array.pairwise
    |> Array.find(fun (a, b) -> a + 1 <> b)
    |> fst
    |> (+) 1

module Day6 =

  let input = (readAllInput 6).Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

  let input' = readInput 6

  // 6549
  let part1() =
    input |> Array.sumBy(set >> Set.remove '\n' >> Set.count)

  let part1'() =
    ((Set.empty, 0), input')
    ||> Array.fold(fun (set, count) line ->
      if String.IsNullOrWhiteSpace line
      then Set.empty, set.Count + count
      else set + Set line, count)
    |> fun (set, count) -> set.Count + count

  let part1''() =
    ((Set.empty, 0), input') ||> Seq.batchOnNewline (Set >> (+)) (Set.count >> (+))

  // 3466
  let part2() =
    input
    |> Array.sumBy(fun s ->
      s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.map set
      |> Set.intersectMany
      |> Set.count
    )

  let part2'() =
    (([], 0), input')
    ||> Array.fold(fun (sets, count) line ->
      if String.IsNullOrWhiteSpace line
      then [], (Set.intersectMany sets).Count + count
      else Set line :: sets, count)
    |> fun (sets, count) -> (Set.intersectMany sets).Count + count

  let part2''() =
    (([], 0), input') ||> Seq.batchOnNewline (Set >> cons) (Set.intersectMany >> Set.count >> (+))

module Day7 =
  open System.Collections.Generic

  let input = readInput 7

  let parse input =
    input
    |> Array.map(fun (l: string) ->
      l
        .Replace(" bags contain ", ",")
        .Replace(" bag.", "")
        .Replace(" bags.", "")
        .Replace(" bag, ", ",")
        .Replace(" bags, ", ",")
        .Replace(",no other", "")
        .Split(',')
    )

  let sample = [|
    "light red bags contain 1 bright white bag, 2 muted yellow bags."
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    "bright white bags contain 1 shiny gold bag."
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    "faded blue bags contain no other bags."
    "dotted black bags contain no other bags."
  |]

  let sample2 = [|
    "shiny gold bags contain 2 dark red bags."
    "dark red bags contain 2 dark orange bags."
    "dark orange bags contain 2 dark yellow bags."
    "dark yellow bags contain 2 dark green bags."
    "dark green bags contain 2 dark blue bags."
    "dark blue bags contain 2 dark violet bags."
    "dark violet bags contain no other bags."
  |]

  let findAll bag rules =
    let filter = new HashSet<_>()
    let rec loop bag = seq {
      let bags =
        rules
        |> Array.choose(fun (outerBag, bags) ->
          if Map.containsKey bag bags && filter.Add(outerBag)
          then Some outerBag
          else None
        )
      yield! bags
      yield! bags |> Seq.collect loop
    }
    loop bag

  let makeRules input =
    input
    |> Array.map(fun rule ->
      Array.head rule, rule |> Array.tail |> Array.map(fun (s: string) -> s.Substring(2), int <| s.Substring(0, 1)) |> Map.ofArray
    )

  // 224
  let part1() =
    parse input
    |> makeRules
    |> findAll "shiny gold"
    |> Seq.length

  // 1488
  let part2() =
    let bagMap =
      parse input
      |> makeRules
      |> Map.ofArray

    let rec loop bag =
      let innerBags = bagMap.[bag] |> Seq.sumBy(fun (KeyValue(bag, count)) -> loop bag * count)
      innerBags + 1
    loop "shiny gold" - 1

//module Day8 =
//  let input = readInput 8

//module Day9 =
//  let input = readInput 9

//module Day10 =
//  let input = readInput 10

//module Day11 =
//  let input = readInput 11

//module Day12 =
//  let input = readInput 12

//module Day13 =
//  let input = readInput 13

//module Day14 =
//  let input = readInput 14

//module Day15 =
//  let input = readInput 15

//module Day16 =
//  let input = readInput 16

//module Day17 =
//  let input = readInput 17

//module Day18 =
//  let input = readInput 18

//module Day19 =
//  let input = readInput 19

//module Day20 =
//  let input = readInput 20

//module Day21 =
//  let input = readInput 21

//module Day22 =
//  let input = readInput 22

//module Day23 =
//  let input = readInput 23

//module Day24 =
//  let input = readInput 24

//module Day25 =
//  let input = readInput 25
