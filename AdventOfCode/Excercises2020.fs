#if INTERACTIVE
#r "nuget: XUnit"
#r "nuget: FSharp.Data"
#load "Utils.fs"
#else
namespace Excercises2020
#endif

open Utils
open Xunit
open System
open System.Collections.Generic
open System.Text.RegularExpressions

module Day01 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  let tryFindMatch sum s1 =
    let s2 = s1 |> Set.map(fun i -> sum - i)
    let i = Set.intersect s1 s2
    if i.Count <> 2 then None else
    match Set.toArray i with
    | [| a ; b |] -> Some(a, b)
    | _ -> failwith "Expected an array of two elements."

  [<Theory>]
  [<FileData(2020, 1, 691771)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let input = input |> Array.map int |> set
    tryFindMatch 2020 input
    |> Option.map(fun (a, b) -> a * b)
    |> Option.defaultValue 0
    =! expected

  [<Theory>]
  [<FileData(2020, 1, 232508760)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let input = input |> Array.map int |> set
    let mutable result = 0
    for i in input do
      input
      |> Set.remove i
      |> tryFindMatch (2020 - i)
      |> Option.iter(fun (a, b) ->
        result <- i * a * b)
    result =! expected

module Day02 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  let parsed (input: string array) =
    input
    |> Array.map(fun s ->
      match s.Split([|' ' ; ':' ; '-'|], StringSplitOptions.RemoveEmptyEntries) with
      | [|min ; max ; ch ; pw|] ->
        int min, int max, char ch, pw
      | _ -> failwith "Incorrect input"
    )

  [<Theory>]
  [<FileData(2020, 2, 628)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    parsed input
    |> Array.countTrue(fun (min, max, char, pw) ->
      let count = pw |> Seq.countTrue((=) char)
      min <= count && count <= max
    )
    =! expected

  [<Theory>]
  [<FileData(2020, 2, 705)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    parsed input
    |> Array.countTrue(fun (p1, p2, char, pw) ->
      let b1 = pw.[p1 - 1] = char
      let b2 = pw.[p2 - 1] = char
      b1 <> b2
    )
    =! expected

module Day03 =

  let input = [| "" |]
  let sample (result: int64) = makeSample result input

  let [<Literal>] TreeMarker = '#'

  let countTrees2 (input: string[]) slope =
    let w = input.[0].Length
    (0,0)
    |> Seq.unfold(fun p ->
      if snd p >= input.Length then None else
      Some(p, Tuple.add slope p))
    |> Seq.countTrue(fun (x, y) -> input.[y].[x % w] = TreeMarker)
    |> int64

  [<Theory>]
  [<FileData(2020, 3, 216L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part1 (input: string array) expected =
    let slope = (3, 1)
    countTrees2 input slope
    =! expected

  [<Theory>]
  [<FileData(2020, 3, 6708199680L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    let slopes = [|
      1, 1
      3, 1
      5, 1
      7, 1
      1, 2
    |]
    (slopes, 1L)
    ||> Array.foldBack(countTrees2 input >> (*))
    =! expected

module Day04 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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
  |]

  [<Theory>]
  [<FileData(2020, 4, 242)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    parse2 input
    |> Seq.countTrue(fun d ->
      requiredFields |> Array.forall(fst >> d.ContainsKey)
    )
    =! expected

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

  [<Theory>]
  [<FileData(2020, 4, 186)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    parse2 input
    |> countValid
    =! expected

module Day05 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  let getSeatId3 seat =
    seat
    |> Seq.mapi(fun i ch -> if ch = 'F' || ch = 'L' then 0 else 512 >>> i )
    |> Seq.sum

  [<Theory>]
  [<FileData(2020, 5, 838)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    input |> Array.map getSeatId3 |> Array.max
    =! expected

  [<Theory>]
  [<FileData(2020, 5, 714)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    input
    |> Array.map getSeatId3
    |> Array.sort
    |> Array.pairwise
    |> Array.find(fun (a, b) -> a + 1 <> b)
    |> fst
    |> (+) 1
    =! expected

module Day06 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2020, 6, 6549)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    ((Set.empty, 0), input)
    ||> Seq.batchOnNewline (Set >> (+)) (Set.count >> (+))
    =! expected

  [<Theory>]
  [<FileData(2020, 6, 3466)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    (([], 0), input)
    ||> Seq.batchOnNewline (Set >> cons) (Set.intersectMany >> Set.count >> (+))
    =! expected

module Day07 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 7, 224)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    parse input
    |> makeRules
    |> findAll "shiny gold"
    |> Seq.length
    =! expected

  [<Theory>]
  [<FileData(2020, 7, 1488)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let bagMap =
      parse input
      |> makeRules
      |> Map.ofArray

    let rec loop bag =
      let innerBags = bagMap.[bag] |> Seq.sumBy(fun (KeyValue(bag, count)) -> loop bag * count)
      innerBags + 1
    loop "shiny gold" - 1
    =! expected

module Day08 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 8, 1420)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    parse input |> run |> snd
    =! expected

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

  [<Theory>]
  [<FileData(2020, 8, 1245)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    parse input |> fix
    =! expected

module Day09 =

  let input = [| "" |]
  let sample (result: int64) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 9, 57195069L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part1 (input: string array) expected =
    let values = input |> Array.map int64
    findError 25 values |> snd
    =! expected

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

  [<Theory>]
  [<FileData(2020, 9, 7409241L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    let values = input |> Array.map int64
    let _, error = findError 25 values
    findWeakness values error
    =! expected

module Day10 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  let countJoltDifferences adapters =
    let (_, counts) =
      ((0, Map.empty), adapters)
      ||> Array.fold(fun (previous, counts) jolt ->
        let diff = jolt - previous
        let count = counts.TryFind(diff) |> Option.defaultValue 0
        jolt, counts.Add(diff, count + 1)
      )
    counts.[1] * (counts.[3] + 1)

  [<Theory>]
  [<FileData(2020, 10, 2574)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    input |> Array.map int |> Array.sort
    |> countJoltDifferences
    =! expected

  let countArrangements input =
    ([0, 1L], input)
    ||> Array.fold(fun list jolt ->
      let sum = list |> List.sumBy(fun (lesserJolt, count) -> if jolt - lesserJolt <= 3 then count else 0L)
      (jolt,sum) :: list
    )
    |> List.head
    |> snd

  [<Theory>]
  [<FileData(2020, 10, 2644613988352L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    input |> Array.map int |> Array.sort
    |> countArrangements
    =! expected

module Day11 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 11, 2310)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    stableOccupation input
    =! expected

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

  [<Theory>]
  [<FileData(2020, 11, 2074)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    stableOccupation2 input
    =! expected

module Day12 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 12, 882)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    (getEndPosition Origin East input |> fst).Manhattan
    =! expected

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

  [<Theory>]
  [<FileData(2020, 12, 28885)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let wp = Vector.Create(10, 1)
    (getEndPosition2 Origin wp input |> fst).Manhattan
    =! expected

module Day13 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 13, 2165)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    get input
    =! expected

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

  [<Theory>]
  [<FileData(2020, 13, 534035653563227L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    getEarliestTime input.[1]
    =! expected

module Day14 =

  let input = [| "" |]
  let sample (result: int64) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 14, 14839536808842L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part1 (input: string array) expected =
    loadAndSum input
    =! expected

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

  [<Theory>]
  [<FileData(2020, 14, 4215284199669L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    loadAndSum2 input
    =! expected

module Day15 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  type [<Measure>] turn

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

  [<Theory>]
  [<FileData(2020, 15, 706)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let input = input.[0].Split(',') |> Array.map int
    play2 2020<turn> input
    =! expected

  [<Theory>]
  [<FileData(2020, 15, 19331)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let input = input.[0].Split(',') |> Array.map int
    play2 30000000<turn> input
    =! expected

module Day16 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

  let parseRule rule =
    let m = Regex.Match(rule, "([a-z\\s]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$")
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

  [<Theory>]
  [<FileData(2020, 16, 23954)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let (rules, _, tickets) = parse input
    errorRate rules tickets
    =! expected

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

  [<Theory>]
  [<FileData(2020, 16, 453459307723L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
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
    =! expected

module Day18 =

  let input = [| "" |]
  let sample (result: int64) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 18, 12918250417632L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part1 (input: string array) expected =
    input |> Array.sumBy eval
    =! expected

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

  [<Theory>]
  [<FileData(2020, 18, 171259538712010L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    input |> Array.sumBy eval2
    =! expected

module Day20 =

  let input = [| "" |]
  let sample (result: int64) = makeSample result input

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
    let tileToBorder =
      tiles |> Map.map(fun _ tile -> getBorders tile)
    let borderToTile =
      tileToBorder
      |> Seq.collect(fun kvp -> kvp.Value |> Array.map(fun border -> border, kvp.Key))
      |> Seq.groupBy fst
      |> Seq.map(fun (border, v) -> border, Seq.map snd v |> Seq.toArray)
      |> Map.ofSeq
    tileToBorder
    |> Map.filter(fun _ borders ->
      borders
      |> Array.filter(fun border -> borderToTile.[border].Length = 1)
      |> Array.length
      |> (=) 4
    )
    |> Seq.map(fun kvp -> int64 kvp.Key)

  [<Theory>]
  [<FileData(2020, 20, 30425930368573L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part1 (input: string array) expected =
    parse input |> getCorners |> Seq.reduce (*)
    =! expected

module Day22 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 22, 32598)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    parse input |> play |> score
    =! expected

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

  [<Theory>]
  [<FileData(2020, 22, 35836)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    parse input |> play2 |> score
    =! expected

module Day23 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 23, 38756249)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let rounds = 100
    parse (Array.exactlyOne input)
    |> play rounds
    |> getValue
    =! expected

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

  [<Theory>]
  [<FileData(2020, 23, 21986479838L)>]
  //[<MemberData(nameof sample, 0L)>]
  let part2 (input: string array) expected =
    let rounds = 10_000_000
    let maxCups = 1_000_000
    parse (Array.exactlyOne input)
    |> play2 maxCups rounds
    =! expected

module Day24 =

  let input = [| "" |]
  let sample (result: int) = makeSample result input

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

  [<Theory>]
  [<FileData(2020, 24, 254)>]
  //[<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    blackTiles input |> Array.length
    =! expected

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

  [<Theory>]
  [<FileData(2020, 24, 3697)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    run 100 input
    =! expected
