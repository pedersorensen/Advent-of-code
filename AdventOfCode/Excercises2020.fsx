#load "Utils.fsx"

open Utils
open System
open System.Collections.Generic

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

type IDictionary<'TKey, 'TValue> with
  member this.TryGet(key) =
    match this.TryGetValue(key) with
    | true, value -> Some value
    | _ -> None

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

module Day8 =
  let input = readInput 8

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
  let input = readInput 9 |> Array.map int64

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
  let input = readInput 10 |> Array.map int |> Array.sort

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
  let input = readInput 11

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
  let input = readInput 12

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

  let input = readInput 13

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

  let part2() =
    let intervals =
      input.[1].Split(',')
      |> Array.mapi(fun i s ->
        match Int32.TryParse(s) with
        | true, v -> v, i
        | _ -> 0, i)
      |> Array.filter(fun (v, _) -> v <> 0)

    //intervals
    //|> Array.map(fun (i, d) ->
    //  $"Math.DivRem(i + {d},  {i}) |> snd = 0 &&"
    //)
    //|> String.concat "\r\n"
    //|> clip

    (*
      lcm(A, B) = M = P * A = Q * B = (A * B) / gcd(A, B)

      lcm'(A, B, N) = M = P * A + N = Q * B
    *)

    let t =
      Seq.initInfinite(fun i -> 59 * i - 4)
      |> Seq.find(fun i ->
        Math.DivRem(i + 0,  7) |> snd = 0 &&
        Math.DivRem(i + 1, 13) |> snd = 0 &&
        //Math.DivRem(i + 4, 59) |> snd = 0 &&
        Math.DivRem(i + 6, 31) |> snd = 0 &&
        Math.DivRem(i + 7, 19) |> snd = 0
      )

    let t2 =
      Seq.initInfinite(fun i -> 59 * i - 4)
      |> Seq.find(fun i ->
        (i + 0) %  7 = 0 &&
        (i + 1) % 13 = 0 &&
        //(i + 4) % 59 = 0 &&
        (i + 6) % 31 = 0 &&
        (i + 7) % 19 = 0
      )

    (*
    let t0 = 7 * A
    let t1 = 13 * B = t0 + 1
    let t2 = 59 * C = t0 + 4
    let t3 = 31 * D = t0 + 6
    let t4 = 19 * E = t0 + 7
    *)
    ()

module Day14 =

  let input = readInput 14

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
        // TODO 'unfloat' just generated all number from 0 to 2^(xs.Length-1) but
        // in a very poorly performing way, rewrite this.
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

module Day15 =
  open System.Collections.Generic

  let input = (readInput 15).[0].Split(',') |> Array.map int

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
