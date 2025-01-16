﻿#r "nuget: XUnit"
#r "nuget: FSharp.Data"
#load "Utils.fs"

open Utils
open System
open System.Collections.Generic

module Day1 =
  let input = readInput 2020 1 |> Array.map int |> set

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
  let input = readInput 2020 2

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

  let input = readInput 2020 3

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

  let isNumberInRange min max (s: string) =
    match Int32.TryParse(s) with
    | true, n when min <= n && n <= max -> true
    | _ -> false

  let isMatch (pattern: string) (value: string) = Regex.IsMatch(value, pattern)

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

  let input = readInput 2020 4 |> parse |> Seq.toList
  let input2 = readInput 2020 4 |> parse2

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
  let input = readInput 2020 5

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
  let input = (readAllInput 2020 6).Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

  let input' = readInput 2020 6

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
  let input = readInput 2020 7

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

module Day8 =
  let input = readInput 2020 8

  let sample = [|
    "nop +0"
    "acc +1"
    "jmp +4"
    "acc +3"
    "jmp -3"
    "acc -99"
    "acc +1"
    "jmp -4"
    "acc +6"
  |]

  let parse instructions =
    instructions
    |> Array.map(fun (s: string) ->
      s.Substring(0, 3), int(s.Substring(4))
    )

  let run (instructions: _[]) =
    let seen = Array.zeroCreate instructions.Length
    let mutable acc = 0
    let mutable i = 0
    let mutable stop = None
    while Option.isNone stop do
      if seen.[i] then stop <- Some false else
      seen.[i] <- true
      match instructions.[i] with
      | "nop", _ -> i <- i + 1
      | "acc", x ->
        i <- i + 1
        acc <- acc + x
      | "jmp", x -> i <- i + x
      | _ -> failwith "Invalid instruction"
      if i = instructions.Length then stop <- Some true
    stop.Value, acc

  // 1420
  let part1() =
    let s = parse sample |> run // 5
    parse input |> run

  let fix (instructions: _[]) =
    instructions
    |> Seq.mapi(fun i op ->
      if fst op = "acc" then None else
      let fixedOp =
        match op with
        | "nop", count -> "jmp", count
        | "jmp", count -> "nop", count
        | _ -> failwith "Invalid instruction"
      let finished, acc =
        instructions
        |> Array.mapi(fun i' op -> if i = i' then fixedOp else op)
        |> run
      if finished then Some acc else None
    )
    |> Seq.pick id

  // 1245
  let part2() =
    let s = parse sample |> fix // 8
    parse input |> fix

module Day9 =
  let input = readInput 2020 9 |> Array.map int64

  let sample = [|
    35L
    20L
    15L
    25L
    47L
    40L
    62L
    55L
    65L
    95L
    102L
    117L
    150L
    182L
    127L
    219L
    299L
    277L
    309L
    576L
  |]

  let findError preample (values: _[]) =
    values
    |> Seq.indexed
    |> Seq.skip preample
    |> Seq.find(fun (i, value) ->
      let set = values.[i - preample  .. i - 1] |> Set.ofArray
      set
      |> Set.map(fun i -> value - i)
      |> Set.filter(fun i -> i <> value/2L)
      |> Set.intersect set
      |> Seq.isEmpty
    )

  // 57195069
  let part1() =
    let s = findError 5 sample // 127
    findError 25 input

  let findWeakness (values: _[]) error =
    let arr =
      values
      |> Array.mapi(fun i _ ->
        let mutable sum = 0L
        let arr =
          values
          |> Seq.skip i
          |> Seq.takeWhile(fun v ->
            sum <- sum + v
            sum < error
          )
          |> Seq.toArray
        if sum = error && arr.Length > 1 then arr
        else Array.Empty()
      )
      |> Seq.find(fun a -> a.Length > 1)
    Seq.min arr + Seq.max arr

  // 7409241
  let part2() =
    let s = findWeakness sample 127L // 62
    part1() |> snd |> findWeakness input

module Day10 =
  let input = readInput 2020 10 |> Array.map int |> Array.sort

  let sample1 = [|
    1
    4
    5
    6
    7
    10
    11
    12
    15
    16
    19
  |]

  let sample2 = [|
    1
    2
    3
    4
    7
    8
    09
    10
    11
    14
    17
    18
    19
    20
    23
    24
    25
    28
    31
    32
    33
    34
    35
    38
    39
    42
    45
    46
    47
    48
    49
  |]

  let countJoltDifferences adapters =
    let (_, counts) =
      ((0, Map.empty), adapters)
      ||> Array.fold(fun (previous, counts) jolt ->
        let diff = jolt - previous
        let count = counts.TryFind(diff) |> Option.defaultValue 0
        jolt, counts.Add(diff, count + 1)
      )
    counts.[1] * (counts.[3] + 1)

  // 2574
  let part1() =
    let s1 = countJoltDifferences sample1 // 35
    let s2 = countJoltDifferences sample2 // 220
    countJoltDifferences input

  let countArrangements input =
    ([0, 1L], input)
    ||> Array.fold(fun list jolt ->
      let sum = list |> List.sumBy(fun (lesserJolt, count) -> if jolt - lesserJolt <= 3 then count else 0L)
      (jolt,sum) :: list
    )
    |> List.head
    |> snd

  // 2644613988352
  let part2() =
    let s1 = countArrangements sample1 // 8
    let s2 = countArrangements sample2 // 19208
    countArrangements input

module Day11 =
  let input = readInput 2020 11

  let sample = [|
    "L.LL.LL.LL"
    "LLLLLLL.LL"
    "L.L.L..L.."
    "LLLL.LL.LL"
    "L.LL.LL.LL"
    "L.LLLLL.LL"
    "..L.L....."
    "LLLLLLLLLL"
    "L.LLLLLL.L"
    "L.LLLLL.LL"
  |]

  let directions = [|
    -1, -1
    -1, +0
    -1, +1
    +0, +1
    +0, -1
    +1, +1
    +1, +0
    +1, -1
  |]

  let to2dArray (strings: string[]) =
    Array2D.init strings.Length strings.[0].Length (fun y x -> strings.[y].[x])

  let [<Literal>] Floor = '.'
  let [<Literal>] EmptySeat = 'L'
  let [<Literal>] OccupiedSeat = '#'

  let isInRange x y array =
    x > -1 && y > -1 && x < Array2D.length1 array && y < Array2D.length2 array

  let isOccupied x y array =
    isInRange x y array && array.[x, y] = OccupiedSeat

  let iterate array =
    let changes = ref 0
    let array' =
      array
      |> Array2D.mapi(fun i j seat ->
        if seat = Floor then Floor else
        let count =
          directions
          |> Array.countTrue(fun (dx, dy) -> isOccupied (i + dx) (j + dy) array)
        if seat = EmptySeat && count = 0 then
          incr changes
          OccupiedSeat
        elif seat = OccupiedSeat && count > 3 then
          incr changes
          EmptySeat
        else seat
      )
    if !changes = 0 then None else
    Some array'

  let iterateTillUnchanged array =
    array
    |> Seq.unfold(iterate >> Option.map(fun b -> b, b))
    |> Seq.last

  let countOccupied array =
    let count = ref 0
    array
    |> Array2D.iter(fun v -> if v = OccupiedSeat then incr count)
    !count

  let stableOccupation seats =
    to2dArray seats
    |> iterateTillUnchanged
    |> countOccupied

  // 2310
  let part1() =
    let s = stableOccupation sample // 37
    stableOccupation input

  let isDirectionOccupied i j dx dy array =
    (i, j)
    |> Seq.unfold(fun (i, j) ->
      let p = i + dx, j + dy
      let x, y = p
      if isInRange x y array then Some(p, p) else None
    )
    |> Seq.tryPick(fun (x, y) ->
      match array.[x, y] with
      | OccupiedSeat -> Some 1
      | EmptySeat -> Some 0
      | _ -> None)

  let iterate2 array =
    let changes = ref 0
    let array' =
      array
      |> Array2D.mapi(fun i j seat ->
        if seat = Floor then Floor else
        let count =
          directions
          |> Array.sumBy(fun (dx, dy) -> isDirectionOccupied i j dx dy array |> Option.defaultValue 0 )
        if seat = EmptySeat && count = 0 then
          incr changes
          OccupiedSeat
        elif seat = OccupiedSeat && count > 4 then
          incr changes
          EmptySeat
        else seat
      )
    if !changes = 0 then None else
    Some array'

  let iterate2TillUnchanged array =
    array
    |> Seq.unfold(iterate2 >> Option.map(fun b -> b, b))
    |> Seq.last

  let stableOccupation2 seats =
    to2dArray seats
    |> iterate2TillUnchanged
    |> countOccupied

  // 2074
  let part2() =
    let s = stableOccupation2 sample // 26
    stableOccupation2 input

module Day12 =
  let input = readInput 2020 12

  let sample = [|
    "F10"
    "N3"
    "F7"
    "R90"
    "F11"
  |]

  [<StructuredFormatDisplay("({X}, {Y})")>]
  type Vector =
    { X : int
      Y : int }
    static member (+) (v1, v2) = { X = v1.X + v2.X ; Y = v1.Y + v2.Y }
    static member (-) (v1, v2) = { X = v1.X - v2.X ; Y = v1.Y - v2.Y }
    static member (*) (d, v) = { X = v.X * d ; Y = v.Y * d }
    static member Create(x, y) = { X = x ; Y = y }
    member this.Manhattan = abs this.X + abs this.Y
    override this.ToString() = $"{this.X}, {this.Y}"

  module Vector =
    let rotate degrees { X = x ; Y = y } =
      let r = (float degrees) * Math.PI / 180.
      let sinr = sin r |> round |> int
      let cosr = cos r |> round |> int
      let x' = x * cosr - y * sinr
      let y' = x * sinr + y * cosr
      { X = x' ; Y = y' }

  let Origin = Vector.Create( 0,  0)
  let North  = Vector.Create( 0,  1)
  let South  = Vector.Create( 0, -1)
  let East   = Vector.Create( 1,  0)
  let West   = Vector.Create(-1,  0)

  let getEndPosition ship dir instructions =
    ((ship, dir), instructions)
    ||> Array.fold(fun (ship, dir: Vector) (s: string) ->
      let v = Int32.Parse(s.Substring(1))
      match s.[0] with
      | 'N' -> ship + v * North, dir
      | 'S' -> ship + v * South, dir
      | 'E' -> ship + v * East , dir
      | 'W' -> ship + v * West , dir
      | 'L' -> ship, Vector.rotate v dir
      | 'R' -> ship, Vector.rotate -v dir
      | 'F' -> ship + v * dir, dir
      | c -> failwithf "Invalid instruction: %c" c
    )

  // 882
  let part1() =
    let s = (getEndPosition Origin East sample |> fst).Manhattan // 25
    (getEndPosition Origin East input |> fst).Manhattan

  let getEndPosition2 ship waypoint instructions =
    ((ship, waypoint), instructions)
    ||> Array.fold(fun (ship, wp: Vector) (s: string) ->
      let v = Int32.Parse(s.Substring(1))
      match s.[0] with
      | 'N' -> ship, wp + v * North
      | 'S' -> ship, wp + v * South
      | 'E' -> ship, wp + v * East
      | 'W' -> ship, wp + v * West
      | 'L' -> ship, Vector.rotate  v wp
      | 'R' -> ship, Vector.rotate -v wp
      | 'F' -> ship + v * wp, wp
      | c -> failwithf "Invalid instruction: %c" c
    )

  // 28885
  let part2() =
    let wp = Vector.Create(10, 1)
    let s = (getEndPosition2 Origin wp sample |> fst).Manhattan // 286
    (getEndPosition2 Origin wp input |> fst).Manhattan

module Day13 =

  let input = readInput 2020 13

  let sample = [|
    "939"
    "7,13,x,x,59,x,31,19"
  |]

  let get (input: string[]) =
    let earliest = int input.[0]
    let intervals =
      input.[1].Split([|',' ; 'x'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int
    let busId =
      intervals
      |> Array.minBy(fun id -> id * (earliest / id + 1))
    let time = busId * (earliest / busId + 1)
    busId * (time - earliest)

  // 2165
  let part1() =
    let s = get sample // 295
    get input

  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
  /// Computes Bézout coefficients (x, y) such that
  /// a * x + b * y = gcd(a, b)
  let extendedGcd a b =
    let rec loop (r0, s0, t0) (r, s, t) =
      if r = 0 then
        let gcd = t0
        let bezout = s0, t0
        gcd, bezout//, (t, s)
      else
        let q = r0 / r
        let r' = r0 - q * r
        let s' = s0 - q * s
        let t' = t0 - q * t
        loop (r, s, t) (r', s', t')
    loop (a, 1, 0) (b, 0, 1)

  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
  // https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
  /// Computes the modular multiplicative inverse t of a mod n such that
  /// a * t = 1 % n
  let inverse a n =
    let rec loop (r0, t0) (r, t) =
      if r = 0L then
        if r0 > 1L then failwithf "%A is not invertible." a
        if t0 < 0L then t0 + n else t0
      else
        let q = r0 / r
        let r' = r0 - q * r
        let t' = t0 - q * t
        loop (r, t) (r', t')
    loop (n, 0L) (a, 1L)

  // https://brilliant.org/wiki/chinese-remainder-theorem/
  let getEarliestTime (input: string) =
    let (n, a') =
      input.Split(',')
      |> Array.mapi(fun i s ->
        match Int32.TryParse(s) with
        | true, v -> int64 v, int64 -i
        | _ -> 0L, int64 -i)
      |> Array.filter(fun (v, _) -> v <> 0L)
      |> Array.unzip
    let a = (n, a') ||> Array.map2(fun n a -> (n + a) % n)
    let N = n |> Array.reduce (*)
    let y = n |> Array.map(fun i -> N / i)
    let z = (y, n) ||> Array.map2 inverse
    (a, y, z)
    |||> Array.map3(fun a y z -> a * y * z)
    |> Array.sum
    |> fun x -> x % N

  // 534035653563227
  let part2() =
    let s1 = getEarliestTime "7,13,x,x,59,x,31,19" // 1068781
    let s2 = getEarliestTime "17,x,13,19" // 3417
    let s3 = getEarliestTime "67,7,59,61" // 754018
    let s4 = getEarliestTime "67,x,7,59,61" // 779210
    let s5 = getEarliestTime "67,7,x,59,61" // 1261476
    let s6 = getEarliestTime "1789,37,47,1889" // 1202161486
    getEarliestTime input.[1]

module Day14 =

  let input = readInput 2020 14

  let sample = [|
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    "mem[8] = 11"
    "mem[7] = 101"
    "mem[8] = 0"
  |]

  let memAddr (line: string) =
    let i = line.IndexOf('[') + 1
    let j = line.IndexOf(']') - i
    line.Substring(i, j)

  let applyMask (mask: string) (value: int64) =
    let bits = Convert.ToString(value, 2)
    let chars = Array.create mask.Length '0'
    for i = 0 to bits.Length - 1 do
      chars.[^i] <- bits.[^i]
    for i = 0 to chars.Length - 1 do
      let ch = mask.[i]
      if ch <> 'X' then
        chars.[i] <- ch
    Convert.ToInt64(String(chars), 2)

  let loadAndSum (values: string[]) =
    ((Map.empty, ""), values)
    ||> Array.fold(fun (map, mask) line ->
      let idx = line.IndexOf('=')
      let value = line.Substring(idx + 2)
      if line.StartsWith("mask") then
        map, value
      else
        let value = int64 value |> applyMask mask
        map.Add(memAddr line, value), mask
    )
    |> fst
    |> Seq.sumBy(fun kvp -> kvp.Value)

  // 14839536808842
  let part1() =
    let s = loadAndSum sample // 165
    loadAndSum input

  let sample2 = [|
    "mask = 000000000000000000000000000000X1001X"
    "mem[42] = 100"
    "mask = 00000000000000000000000000000000X0XX"
    "mem[26] = 1"
  |]

  let applyMask2 (mask: string) (memAddr: int64) =
    let bits = Convert.ToString(memAddr, 2)
    let chars = Array.create mask.Length '0'
    for i = 0 to bits.Length - 1 do
      chars.[^i] <- bits.[^i]
    let mutable floating = []
    for i = 0 to chars.Length - 1 do
      let ch = mask.[i]
      if ch <> '0' then
        chars.[i] <- ch
        if ch = 'X' then floating <- i :: floating
    chars, floating

  let rec unfloat xs = [
    match xs with
    | [] -> []
    | _ :: xs ->
      for l in unfloat xs do
        '0' :: l
        '1' :: l
  ]

  let loadAndSum2 (values: string[]) =
    ((Map.empty, ""), values)
    ||> Array.fold(fun (map, mask) line ->
      let idx = line.IndexOf('=')
      let value = line.Substring(idx + 2)
      if line.StartsWith("mask") then
        map, value
      else
        let memAddr = memAddr line |> int64
        let (addr, xs) = applyMask2 mask memAddr
        let added =
          (map, unfloat xs)
          ||> List.fold(fun map list ->
            (xs, list) ||> List.iter2(Array.set addr)
            let memAddr = String(addr)
            map.Add(memAddr, int64 value)
          )
        added, mask
    )
    |> fst
    |> Seq.sumBy(fun kvp -> kvp.Value)

  // 4215284199669
  let part2() =
    let s = loadAndSum2 sample2 // 208
    loadAndSum2 input

  let bitsCache = new Dictionary<_, _>()

  let toBits (i: int) =
    match bitsCache.TryGetValue(i) with
    | true, bits -> bits
    | _ ->
      let bits = Convert.ToString(i, 2)
      bitsCache.[i] <- bits
      bits

  let loadAndSum3 (values: string[]) =
    let map = Dictionary<_, _>()
    ("", values)
    ||> Array.fold(fun mask line ->
      let idx = line.IndexOf('=')
      let value = line.Substring(idx + 2)
      if line.StartsWith("mask") then
        value
      else
        let memAddr = memAddr line |> int64
        let (addr, xs) = applyMask2 mask memAddr
        let value = int64 value
        for i = 0 to (1 <<< xs.Length) do
          let s = toBits i
          let length = min xs.Length s.Length
          for j = 0 to length - 1 do
            addr.[xs.[j]] <- s.[^j]
          for j = length to xs.Length - 1 do
            addr.[xs.[j]] <- '0'
          let memAddr = String(addr)
          map.[memAddr] <- value
        mask
    )
    |> ignore
    map |> Seq.sumBy(fun kvp -> kvp.Value)

  // 4215284199669
  let part2'() =
    let s = loadAndSum3 sample2 // 208
    loadAndSum3 input

module Day15 =

  let input = (readInput 2020 15).[0].Split(',') |> Array.map int

  let sample = [| 0 ; 3 ; 6 |]

  type [<Measure>] turn

  let play rounds (input: int[]) =
    let rec loop turn lastSpokenAt (map: Map<_, _>) =
      let turn' = turn + 1<turn>
      let value =
        match lastSpokenAt with
        | None -> 0
        | Some last -> turn - last |> int
      if turn' >= rounds then value else
      let lastTurn = map.TryFind(value)
      map.Add(value, turn')
      |> loop turn' lastTurn
    input
    |> Array.mapi(fun i v -> v, (i + 1) * 1<turn>)
    |> Map.ofArray
    |> loop (input.Length * 1<turn>) None

  let play2 rounds (input: int[]) =
    let map = Dictionary()
    let rec loop turn lastSpokenAt =
      let turn' = turn + 1<turn>
      let value =
        match lastSpokenAt with
        | None -> 0
        | Some last -> turn - last |> int
      if turn' >= rounds then value else
      let lastTurn = map.TryGet(value)
      map.[value] <- turn'
      loop turn' lastTurn
    for i = 0 to input.Length - 1 do
      map.[input.[i]] <- (i + 1) * 1<turn>
    loop (input.Length * 1<turn>) None

  // 706
  let part1() =
    let s = play 10<turn> sample // 0
    let t = 2020<turn>
    let s = play t [|1;3;2|] // 1
    let s = play t [|2;1;3|] // 10
    let s = play t [|1;2;3|] // 27
    let s = play t [|2;3;1|] // 78
    let s = play t [|3;2;1|] // 438
    let s = play t [|3;1;2|] // 1836
    play t input

  // 19331
  let part2() =
    let t = 30000000<turn>
    play2 t input

module Day16 =
  open System.Text.RegularExpressions

  let input = readInput 2020 16

  let sample = [|
    "class: 1-3 or 5-7"
    "row: 6-11 or 33-44"
    "seat: 13-40 or 45-50"
    ""
    "your ticket:"
    "7,1,14"
    ""
    "nearby tickets:"
    "7,3,47"
    "40,4,50"
    "55,2,20"
    "38,6,12"
  |]

  let sample2 = [|
    "class: 0-1 or 4-19"
    "row: 0-5 or 8-19"
    "seat: 0-13 or 16-19"
    ""
    "your ticket:"
    "11,12,13"
    ""
    "nearby tickets:"
    "3,9,18"
    "15,1,5"
    "5,14,9"
  |]

  let parseRule rule =
    let m = Regex.Match(rule, "([a-z\s]+): (\d+)-(\d+) or (\d+)-(\d+)$")
    if not m.Success then failwith "Not a match."
    let grp = m.Groups
    let name = grp.[1].Value
    let r1 = int grp.[2].Value, int grp.[3].Value
    let r2 = int grp.[4].Value, int grp.[5].Value
    name, (r1, r2)

  let parseTicket (ticket: string) = ticket.Split(',') |> Array.map int

  let parse input =
    let i1 = Array.IndexOf(input, "")
    let i2 = Array.IndexOf(input, "", i1 + 1)
    let rules = input |> Array.take i1 |> Array.map parseRule
    let myTicket = input.[i1 + 2] |> parseTicket
    let l = input.Length - i2 - 2
    let nearbyTickets = Array.zeroCreate<string> l
    Array.Copy(input, i2 + 2, nearbyTickets, 0, l)
    rules, myTicket, nearbyTickets |> Array.map parseTicket

  let applyRule value rule =
    let (_name, ((l1, h1), (l2, h2))) = rule
    (l1 <= value && value <= h1) ||
    (l2 <= value && value <= h2)

  let errorRate rules tickets =
    tickets
    |> Array.collect(fun ticket ->
      ticket
      |> Array.filter(fun (value: int) ->
        rules
        |> Array.exists(applyRule value)
        |> not
      )
    )
    |> Array.sum

  let validTickets rules tickets =
    tickets
    |> Array.filter(fun ticket ->
      ticket
      |> Array.forall(fun (value: int) ->
        rules |> Array.exists(applyRule value)
      )
    )

  // 23954
  let part1() =
    let s =
      let (rules, _, tickets) = parse sample
      errorRate rules tickets // 71
    let (rules, _, tickets) = parse input
    errorRate rules tickets

  let rec reduce acc (map: Map<string, Set<int>>) =
    let o =
      map |> Seq.tryFind(fun kvp -> kvp.Value.Count = 1)
    match o with
    | Some (KeyValue(rule, set)) ->
      let value = Seq.exactlyOne set
      map.Remove(rule)
      |> Map.map(fun _ v -> v.Remove(value))
      |> reduce ((rule, value)::acc)
    | None -> acc

  // 453459307723
  let part2() =
    let (rules, my, tickets) = parse input
    let valid = validTickets rules tickets

    let fields = Array.init my.Length id

    let mapped =
      rules
      |> Array.map(fun rule ->
        fst rule,
        fields
        |> Array.filter(fun field ->
          valid
          |> Array.forall(fun ticket ->
            applyRule ticket.[field] rule
          )
        ) |> Set.ofArray
      ) |> Map.ofArray

    reduce [] mapped
    |> List.filter(fun (r, _) -> r.StartsWith("departure"))
    |> List.map(fun (_, i) -> int64 my.[i])
    |> List.reduce (*)

//module Day17 =
//  let input = readInput 2020 17

module Day18 =
  let input = readInput 2020 18

  let cint64 (ch: char) =
    if Char.IsNumber ch |> not then
      invalidOp <| sprintf "Not a number: %c" ch
    int64 ch - int64 '0'

  let sadd = Some (+)
  let smult = Some (*)

  let evalOp op acc v =
    match op with
    | Some op -> op acc v
    | None -> v

  let eval (exp: string) =
    let rec loop op tokens acc =
      match tokens with
      | token :: tail ->
        match token with
        | '+' -> loop sadd tail acc
        | '*' -> loop smult tail acc
        | v when Char.IsNumber v->
          cint64 v |> evalOp op acc |> loop None tail
        | '(' ->
          let v, tail = loop None tail 0L
          evalOp op acc v |> loop None tail
        | ')' -> acc, tail
        | ' ' -> loop op tail acc
        | c -> failwithf "Unexpected token: %c" c
      | tail -> acc, tail
    let tokens = List.ofSeq exp
    loop None tokens 0L |> fst

  // 12918250417632
  let part1() =
    let s1 = eval "1 + 2 * 3 + 4 * 5 + 6" // 71
    let s2 = eval "1 + (2 * 3) + (4 * (5 + 6))" // 51
    let s3 = eval "2 * 3 + (4 * 5)" // 26
    let s4 = eval "5 + (8 * 3 + 9 + 3 * 4 * 3)" // 437
    let s5 = eval "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" // 12240
    let s6 = eval "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" // 13632
    input |> Array.sumBy eval

  let eval2 (exp: string) =
    let rec loop op tokens acc =
      match tokens with
      | token :: tail ->
        match token with
        | '+' -> loop sadd tail acc
        | '*' ->
          let v, tail' = loop None tail 0L
          acc * v, tail'
        | v when Char.IsNumber v ->
          cint64 v |> evalOp op acc |> loop None tail
        | '(' ->
          let v, tail' = loop None tail 0L
          evalOp op acc v |> loop None tail'
        | ')' -> acc, tail
        | ' ' -> loop op tail acc
        | c -> failwithf "Unexpected token: %c" c
      | [] -> acc, []
    let tokens = List.ofSeq exp
    loop None tokens 0L |> fst

  let eval3 (tokens: string) =
    let rec loop op i acc =
      if i = tokens.Length then acc, i else
      let i' = i + 1
      match tokens.[i] with
      | '+' -> loop sadd i' acc
      | '*' ->
        let v, i'' = loop None i' 0L
        acc * v, i''
      | v when Char.IsNumber v ->
        cint64 v |> evalOp op acc |> loop None i'
      | '(' ->
        let v, i'' = loop None i' 0L
        evalOp op acc v |> loop None i''
      | ')' -> acc, i'
      | ' ' -> loop op i' acc
      | c -> failwithf "Unexpected token: %c" c
    loop None 0 0L |> fst

  // 171259538712010
  let part2() =
    let s1 = eval2 "1 + 2 * 3 + 4 * 5 + 6" // 231
    let s2 = eval2 "1 + (2 * 3) + (4 * (5 + 6))" // 51
    let s3 = eval2 "2 * 3 + (4 * 5)" // 46
    let s4 = eval2 "5 + (8 * 3 + 9 + 3 * 4 * 3)" // 1445
    let s5 = eval2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" // 669060
    let s6 = eval2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" // 23340
    let s = input |> Array.sumBy eval2
    input |> Array.sumBy eval3

//module Day19 =
//  let input = readInput 2020 19

module Day20 =
  let input = readInput 2020 20

  let sample = [|
    "Tile 2311:"
    "..##.#..#."
    "##..#....."
    "#...##..#."
    "####.#...#"
    "##.##.###."
    "##...#.###"
    ".#.#.#..##"
    "..#....#.."
    "###...#.#."
    "..###..###"
    ""
    "Tile 1951:"
    "#.##...##."
    "#.####...#"
    ".....#..##"
    "#...######"
    ".##.#....#"
    ".###.#####"
    "###.##.##."
    ".###....#."
    "..#.#..#.#"
    "#...##.#.."
    ""
    "Tile 1171:"
    "####...##."
    "#..##.#..#"
    "##.#..#.#."
    ".###.####."
    "..###.####"
    ".##....##."
    ".#...####."
    "#.##.####."
    "####..#..."
    ".....##..."
    ""
    "Tile 1427:"
    "###.##.#.."
    ".#..#.##.."
    ".#.##.#..#"
    "#.#.#.##.#"
    "....#...##"
    "...##..##."
    "...#.#####"
    ".#.####.#."
    "..#..###.#"
    "..##.#..#."
    ""
    "Tile 1489:"
    "##.#.#...."
    "..##...#.."
    ".##..##..."
    "..#...#..."
    "#####...#."
    "#..#.#.#.#"
    "...#.#.#.."
    "##.#...##."
    "..##.##.##"
    "###.##.#.."
    ""
    "Tile 2473:"
    "#....####."
    "#..#.##..."
    "#.##..#..."
    "######.#.#"
    ".#...#.#.#"
    ".#########"
    ".###.#..#."
    "########.#"
    "##...##.#."
    "..###.#.#."
    ""
    "Tile 2971:"
    "..#.#....#"
    "#...###..."
    "#.#.###..."
    "##.##..#.."
    ".#####..##"
    ".#..####.#"
    "#..#.#..#."
    "..####.###"
    "..#.#.###."
    "...#.#.#.#"
    ""
    "Tile 2729:"
    "...#.#.#.#"
    "####.#...."
    "..#.#....."
    "....#..#.#"
    ".##..##.#."
    ".#.####..."
    "####.#.#.."
    "##.####..."
    "##..#.##.."
    "#.##...##."
    ""
    "Tile 3079:"
    "#.#.#####."
    ".#..######"
    "..#......."
    "######...."
    "####.#..#."
    ".#...#.##."
    "#.#####.##"
    "..#.###..."
    "..#......."
    "..#.###..."
  |]

  module ResizeArray =
    let add item (array: ResizeArray<_>) =
      array.Add(item)
      array

  let parse input =
    (((null, null), Map.empty), input)
    ||> Seq.batchOnNewline(fun line (tile, lines) ->
      if line.StartsWith("Tile")
      then line.Substring(5, 4), ResizeArray()
      else tile, ResizeArray.add line lines
    ) (fun (tile, lines) ->
      if String.IsNullOrWhiteSpace(tile) then id else
      lines.ToArray() |> Map.add (int tile))

  let reverse (s: string) =
    let chars = s.ToCharArray()
    Array.Reverse(chars)
    String(chars)

  let getBorders (tile: string[]) =
    let leftChars  = Array.zeroCreate tile.Length
    let rightChars = Array.zeroCreate tile.Length
    for i = 0 to tile.Length - 1 do
      leftChars.[i]  <- tile.[i].[0]
      rightChars.[i] <- tile.[i].[^0]
    let left  = String(leftChars)
    let right = String(rightChars)
    Array.Reverse(leftChars)
    Array.Reverse(rightChars)
    let leftFlipped  = String(leftChars)
    let rightFlipped = String(rightChars)
    [|
      tile.[0]
      reverse tile.[0]
      tile.[^0]
      reverse tile.[^0]
      left
      leftFlipped
      right
      rightFlipped
    |]

  let getCorners tiles =
    // A mapping from a tile id to all permutations of its borders
    let tileToBorder =
      tiles |> Map.map(fun _ tile -> getBorders tile)
    // A mapping of all border permutations to any matching tile
    let borderToTile =
      tileToBorder
      |> Seq.collect(fun kvp -> kvp.Value |> Array.map(fun border -> border, kvp.Key))
      |> Seq.groupBy fst
      |> Seq.map(fun (border, v) -> border, Seq.map snd v |> Seq.toArray)
      |> Map.ofSeq
    // Find any tile that has exactly two borders that are not found on any other tile.
    tileToBorder
    |> Map.filter(fun _ borders ->
      borders
      |> Array.filter(fun border -> borderToTile.[border].Length = 1)
      |> Array.length
      |> (=) 4
    )
    |> Seq.map(fun kvp -> int64 kvp.Key)

  // 30425930368573
  let part1() =
    let s = parse sample |> getCorners |> Seq.reduce (*) // 20899048083289
    parse input |> getCorners |> Seq.reduce (*)

//module Day21 =
//  let input = readInput 2020 21

module Day22 =
  let input = readInput 2020 22

  let sample = [|
    "Player 1:"
    "9"
    "2"
    "6"
    "3"
    "1"
    ""
    "Player 2:"
    "5"
    "8"
    "4"
    "7"
    "10"
  |]

  let parse input =
    let map =
      (((null, null), Map.empty), input)
      ||> Seq.batchOnNewline(fun line (player, values) ->
          if line.StartsWith("Player")
          then line, LinkedList()
          else
            values.AddLast(int line) |> ignore
            player, values
      ) Map.addTuple
    map.["Player 1:"], map.["Player 2:"]

  let (|TryHead|Empty|) (list: LinkedList<_>) =
    match list.First with
    | null -> Empty
    | node -> TryHead node

  let play (p1, p2) =
    let rec loop() =
      match p1, p2 with
      | TryHead n1, TryHead n2 ->
        p1.Remove(n1)
        p2.Remove(n2)
        let v1 = n1.Value
        let v2 = n2.Value
        let (winner, v1, v2) =
          if v1 > v2
          then p1, v1, v2
          else p2, v2, v1
        winner.AddLast(v1).List.AddLast(v2) |> ignore
        loop()
      | TryHead _, Empty -> p1
      | Empty, TryHead _ -> p2
      | Empty, Empty -> failwith "No winner"
    loop()

  let score (winner: LinkedList<_>) =
    let cards = winner.Count
    winner
    |> Seq.mapi(fun i v -> v , (cards - i))
    |> Seq.sumBy(fun (a, b) -> a * b)

  // 32598
  let part1() =
    let s = parse sample |> play |> score // 306
    parse input |> play |> score

  module LinkedList =

    let cloneN n (list: LinkedList<'T>) =
      let clone = LinkedList<'T>()
      let rec loop n (node: LinkedListNode<'T>) =
        if n = 0 then clone else
        if isNull node then
          invalidOp "The input sequence has an insufficient number of elements."
        clone.AddLast(node.Value) |> ignore
        loop (n - 1) node.Next
      loop n list.First

  let rec play2 (p1:LinkedList<int>, p2:LinkedList<int>) =
    let seen = HashSet()
    let rec loop() =
      let p1Values = String.Join(", ", p1)
      let p2Values = String.Join(", ", p2)
      if seen.Add(p1Values + p2Values) |> not then p1 else
      match p1.First, p2.First with
      | null, null -> failwith "No winner"
      | _, null -> p1
      | null, _ -> p2
      | n1, n2 ->
        let v1 = n1.Value
        let v2 = n2.Value
        p1.Remove(n1)
        p2.Remove(n2)
        let (winner, v1, v2) =
          if p1.Count >= v1 && p2.Count >= v2 then
            let p1' = LinkedList.cloneN v1 p1
            let p2' = LinkedList.cloneN v2 p2
            if play2 (p1', p2') = p1'
            then p1, v1, v2
            else p2, v2, v1
          elif v1 > v2
          then p1, v1, v2
          else p2, v2, v1
        winner.AddLast(v1).List.AddLast(v2) |> ignore
        loop()
    loop()

  let sampleInf = [|
    "Player 1:"
    "43"
    "19"
    ""
    "Player 2:"
    "2"
    "29"
    "14"
  |]

  // 35836
  let part2() =
    let s = parse sample |> play2 |> score // 291
    let t = parse sampleInf |> play2 |> score // 105
    parse input |> play2 |> score

module Day23 =
  let input = readInput 2020 23 |> Array.exactlyOne

  let sample = "389125467"

  let parse (input: string) =
    input
    |> Seq.map(fun ch -> int ch - int '0')
    |> LinkedList

  type LinkedListNode<'T> with
    member this.NextOrWrap =
      match this.Next with
      | null -> this.List.First
      | node -> node

  module LinkedList =
    let findValue value (list: LinkedList<_>) =
      let rec loop (node: LinkedListNode<_>) =
        match node with
        | null -> failwith "No value"
        | node ->
          if node.Value = value then node else loop node.Next
      loop list.First

  let rec makeMap (list: LinkedList<_>) =
    let map = Dictionary()
    let rec loop (node: LinkedListNode<_>) =
      match node with
      | null -> map
      | node ->
        map.[node.Value] <- node
        loop node.Next
    loop list.First

  let play rounds (cups: LinkedList<int>) =
    let max = Seq.max cups
    let nodes = makeMap cups
    let rec loop round (current: LinkedListNode<_>) =
      let cups = current.List
      if round >= rounds then cups else
      let n1 = current.NextOrWrap
      let n2 = n1.NextOrWrap
      let n3 = n2.NextOrWrap
      cups.Remove(n1)
      cups.Remove(n2)
      cups.Remove(n3)
      let rec findNext value =
        if value < 1 then findNext max
        elif value = n1.Value || value = n2.Value || value = n3.Value
        then findNext (value - 1)
        else nodes.[value]
      let next = findNext (current.Value - 1)
      cups.AddAfter(next, n1)
      cups.AddAfter(n1, n2)
      cups.AddAfter(n2, n3)
      nodes.[n1.Value] <- n1
      nodes.[n2.Value] <- n2
      nodes.[n3.Value] <- n3
      loop (round + 1) current.NextOrWrap
    loop 0 cups.First

  let getValue cups =
    let n = cups |> LinkedList.findValue 1
    n.NextOrWrap
    |> Seq.unfold(fun n ->
      if n.Value = 1 then None else
        Some(n.Value, n.NextOrWrap))
    |> Seq.fold(fun s t -> 10 * s + t) 0

  // 38756249
  let part1() =
    let rounds = 100
    let s = parse sample |> play rounds |> getValue // 67384529
    parse input |> play rounds |> getValue

  let extendList total (list: LinkedList<_>) =
    let c = list.Count
    Seq.init (total - c) (fun i -> i + c + 1)
    |> Seq.append list
    |> LinkedList

  let play2 maxCups rounds list =
    let played =
      list
      |> extendList maxCups
      |> play rounds
      |> LinkedList.findValue 1
    let n1 = played.NextOrWrap
    let n2 = n1.NextOrWrap
    int64 n1.Value * int64 n2.Value

  // 21986479838
  let part2() =
    let rounds = 10_000_000
    let maxCups = 1_000_000
    let s = parse sample |> play2 maxCups rounds // 149245887792
    parse input |> play2 maxCups rounds

module Day24 =
  let input = readInput 2020 24

  let sample = [|
    "sesenwnenenewseeswwswswwnenewsewsw"
    "neeenesenwnwwswnenewnwwsewnenwseswesw"
    "seswneswswsenwwnwse"
    "nwnwneseeswswnenewneswwnewseswneseene"
    "swweswneswnenwsewnwneneseenw"
    "eesenwseswswnenwswnwnwsewwnwsene"
    "sewnenenenesenwsewnenwwwse"
    "wenwwweseeeweswwwnwwe"
    "wsweesenenewnwwnwsenewsenwwsesesenwne"
    "neeswseenwwswnwswswnw"
    "nenwswwsewswnenenewsenwsenwnesesenew"
    "enewnwewneswsewnwswenweswnenwsenwsw"
    "sweneswneswneneenwnewenewwneswswnese"
    "swwesenesewenwneswnwwneseswwne"
    "enesenwswwswneneswsenwnewswseenwsese"
    "wnwnesenesenenwwnenwsewesewsesesew"
    "nenewswnwewswnenesenwnesewesw"
    "eneswnwswnwsenenwnwnwwseeswneewsenese"
    "neswnwewnwnwseenwseesewsenwsweewe"
    "wseweeenwnesenwwwswnew"
  |]

  let parse (line: string) =
    ((None, []), line)
    ||> Seq.fold(fun (prev, acc) ch ->
      match ch with
      | 'n' | 's' -> Some ch, acc
      | _ ->
        match prev with
        | Some p -> None, $"{p}{ch}" :: acc
        | None   -> None, $"{ch}"    :: acc
    ) |> snd

  let e  =  1,  0
  let w  = -1,  0
  let ne =  0,  1
  let sw =  0, -1
  let nw = -1,  1
  let se =  1, -1

  let all = [|
    e
    w
    ne
    sw
    nw
    se
  |]

  let getLocation s =
    ((0, 0), s)
    ||> List.fold(fun p c ->
      match c with
      | "e"  -> e
      | "w"  -> w
      | "ne" -> ne
      | "sw" -> sw
      | "nw" -> nw
      | "se" -> se
      | c -> failwithf "Unexpected character: %s" c
      |> Tuple.add p
    )

  let blackTiles input =
    input
    |> Array.map(parse >> getLocation)
    |> Array.countBy id
    |> Array.choose(fun (p, c) -> if c % 2 = 0 then None else Some p)

  // 254
  let part1() =
    let s = blackTiles sample |> Array.length // 10
    blackTiles input |> Array.length

  let advance black =
    let blackS = set black
    black
    |> Array.collect(fun p -> all |> Array.map(Tuple.add p))
    |> Array.append black
    |> Array.distinct
    |> Array.choose(fun n ->
      let isBlack = blackS.Contains(n)
      let c =
        all
        |> Array.filter(fun d -> Tuple.add d n |> blackS.Contains)
        |> Array.length
      if isBlack && (c = 0 || c > 2) then None
      elif not isBlack && c = 2 then Some n
      elif isBlack then Some n else None
    )

  let run count input =
    (blackTiles input, Seq.init count ignore)
    ||> Seq.fold(fun d () -> advance d)
    |> Array.length

  // 3697
  let part2() =
    let s = run 100 sample // 2208
    run 100 input

//module Day25 =
//  let input = readInput 2020 25
