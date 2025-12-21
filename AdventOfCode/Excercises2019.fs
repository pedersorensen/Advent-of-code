#if INTERACTIVE
#r "nuget: XUnit"
#r "nuget: FSharp.Data"
#load "Utils.fs"
#else
namespace Excercises2019
#endif

open Utils
open Xunit
open System
open System.Collections.Generic
open System.Reflection

module List =

  /// Append the second list to the end of the first.
  /// WARNING: This mutates the first parameter when it is non-empty.
  let setTail (list1 : 'a list) (list2 : 'a list) =
    let rec helper (list : 'a list) =
      match list with
      | [] -> list2
      | [ _ ] ->
        let fi = typeof<'a list>.GetField("tail", BindingFlags.Instance ||| BindingFlags.NonPublic)
        if isNull fi then failwith "Could not find private 'tail' field on list."
        fi.SetValue(list, list2)
        list1
      | _ :: tail -> helper tail
    match list2 with
    | [] -> list1
    | _ -> helper list1

module Intcode =
  module private OpCodes =
    let [<Literal>] Add = 1
    let [<Literal>] Mult = 2
    let [<Literal>] Input = 3
    let [<Literal>] Output = 4
    let [<Literal>] JumpIfTrue = 5
    let [<Literal>] JumpIfFalse = 6
    let [<Literal>] LessThan = 7
    let [<Literal>] Equals = 8
    let [<Literal>] AdjustRelativeBase = 9
    let [<Literal>] Stop = 99

  let private codeAndModes code =
    let (code, opCode) = Math.DivRem(code, 100)
    let (code, m1) = Math.DivRem(code, 10)
    let (code, m2) = Math.DivRem(code, 10)
    let (code, m3) = Math.DivRem(code, 10)
    if code <> 0 then failwith "Too many digits in code"
    opCode, m1, m2, m3

  let run (input : seq<int>) (program : int []) =
    use en = input.GetEnumerator()
    let program = Array.copy program
    let rec inner i = seq {
      let code = program.[i]
      let code, m1, m2, m3 = codeAndModes code
      let val1 = lazy if m1 = 1 then program.[i+1] else program.[program.[i+1]]
      let val2 = lazy if m2 = 1 then program.[i+2] else program.[program.[i+2]]
      match code with
      | OpCodes.Stop -> ()
      | OpCodes.Add ->
        let pos3 = program.[i+3]
        if m3 <> 0 then failwith "Should have been position mode for Add"
        program.[pos3] <- val1.Value + val2.Value
        yield! inner (i+4)
      | OpCodes.Mult ->
        let pos3 = program.[i+3]
        if m3 <> 0 then failwith "Should have been position mode for Mult"
        program.[pos3] <- val1.Value * val2.Value
        yield! inner (i+4)
      | OpCodes.Input ->
        let pos1 = program.[i+1]
        if m1 <> 0 then failwith "Should have been position mode for Input"
        if not <| en.MoveNext() then failwith "No input"
        program.[pos1] <- en.Current
        yield! inner (i+2)
      | OpCodes.Output ->
        yield val1.Value
        yield! inner (i+2)
      | OpCodes.JumpIfTrue  ->
        let i = if val1.Value <> 0 then val2.Value else i + 3
        yield! inner i
      | OpCodes.JumpIfFalse ->
        let i = if val1.Value = 0 then val2.Value else i + 3
        yield! inner i
      | OpCodes.LessThan ->
        let pos3 = program.[i+3]
        if m3 <> 0 then failwith "Should have been position mode for LessThan"
        program.[pos3] <- if val1.Value < val2.Value then 1 else 0
        yield! inner (i + 4)
      | OpCodes.Equals ->
        let pos3 = program.[i+3]
        if m3 <> 0 then failwith "Should have been position mode for LessThan"
        program.[pos3] <- if val1.Value = val2.Value then 1 else 0
        yield! inner (i + 4)
      | OpCodes.AdjustRelativeBase ->
        yield! inner (i + 2)
      | code -> failwithf "Invalid opcode: %i" code
    }
    inner 0 |> Seq.toArray, program

module Day01 =

  let getFuel mass = max 0 (mass / 3 - 2)

  let part1_input = [| "12" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 1, 3266516)>]
  [<MemberData(nameof part1_sample, 2)>]
  let part1 (input: string array) expected =
    input
    |> Array.sumBy(int >> getFuel)
    =! expected

  let rec getFuel' mass =
    let fuel = getFuel mass
    if fuel > 0 then
      fuel + getFuel' fuel
    else fuel

  let part2_input = [| "14" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 1, 4896902)>]
  [<MemberData(nameof part2_sample, 2)>]
  let part2 (input: string array) expected =
    input
    |> Array.sumBy(int >> getFuel')
    =! expected

module Day02 =

  let runProgram input code =
    Intcode.run input code |> snd

  let part1_input = [| "1,0,0,0,99" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 2, 3562624)>]
  [<MemberData(nameof part1_sample, 2)>]
  let part1 (input: string array) expected =
    let input = input.[0].Split(',') |> Array.map int
    input.[1] <- 12
    input.[2] <- 2
    runProgram [] input |> Array.item 0
    =! expected

  let part2_input = [| "1,0,0,0,99" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 2, 8298)>]
  [<MemberData(nameof part2_sample, 2)>]
  let part2 (input: string array) expected =
    let program = input.[0].Split(',') |> Array.map int
    let mutable result = 0
    for noun in 0 .. 99 do
      for verb in 0 .. 99 do
        let input = Array.copy program
        input.[1] <- noun
        input.[2] <- verb
        let res = runProgram [] input
        if res.[0] = 19690720 then
          result <- 100 * noun + verb
    result =! expected

module Day03 =

  let buildWire (parts : string) =
    let rec inner (wire : (int*int) list) (parts : string list) =
      match parts with
      | [] -> wire
      | part :: tail ->
        let (x, y) = wire |> List.head
        let d = part.[0]
        let l = int(string(part.Substring(1)))
        let wire' =
          let wire' =
            match d with
            | 'U' -> List.init l (fun i -> x, y - i + l)
            | 'D' -> List.init l (fun i -> x, y + i - l)
            | 'L' -> List.init l (fun i -> x + i - l, y)
            | 'R' -> List.init l (fun i -> x - i + l, y)
            | _ -> failwith "Invalid direction"
          List.setTail wire' wire
        inner wire' tail
    parts.Split(',') |> List.ofArray
    |> inner [0,0]
    |> List.rev

  let getClosestIntersection w1 w2 =
    let points1 = buildWire w1 |> HashSet
    let points2 = buildWire w2
    points1.IntersectWith(points2)
    let _ = points1.Remove(0,0)
    points1 |> Seq.minBy(fun (x, y) -> abs x + abs y)

  let part1_input = [| "R8,U5,L5,D3"; "U7,R6,D4,L4" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 3, 1983)>]
  [<MemberData(nameof part1_sample, 6)>]
  let part1 (input: string array) expected =
    let (x, y) = getClosestIntersection input.[0] input.[1]
    abs x + abs y =! expected

  let getShortestIntersection w1 w2 =
    let wire1 = buildWire w1
    let wire2 = buildWire w2
    let points1 = HashSet wire1
    points1.IntersectWith(wire2)
    points1.Remove(0, 0) |> ignore
    points1
    |> Seq.map(fun p ->
      let d1 = wire1 |> List.findIndex((=) p)
      let d2 = wire2 |> List.findIndex((=) p)
      d1 + d2)
    |> Seq.min

  let part2_input = [| "R8,U5,L5,D3"; "U7,R6,D4,L4" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 3, 107754)>]
  [<MemberData(nameof part2_sample, 30)>]
  let part2 (input: string array) expected =
    getShortestIntersection input.[0] input.[1]
    =! expected

module Day04 =

  let hasPair (a:int) b c d e f =
    a = b || b = c || c = d || d = e || e = f

  let isDecreasing (a:int) b c d e f =
    a > b || b > c || c > d || d > e || e > f

  let inline digits i =
    let (i, f) = Math.DivRem(i, 10)
    let (i, e) = Math.DivRem(i, 10)
    let (i, d) = Math.DivRem(i, 10)
    let (i, c) = Math.DivRem(i, 10)
    let (i, b) = Math.DivRem(i, 10)
    let (i, a) = Math.DivRem(i, 10)
    if i > 0 then failwith "More than 6 digits"
    a, b, c, d, e, f

  let isValid i =
    let (a, b, c, d, e, f) = digits i
    hasPair a b c d e f && not (isDecreasing a b c d e f)

  [<Fact>]
  let part1() =
    let from, to' = 147981, 691423
    let result =
      seq { from .. to' }
      |> Seq.countBy isValid
      |> Seq.find fst
      |> snd
    result =! 1790

  let hasPairNoTriple a b c d e f =
    (a = b && b <> c) ||
    (a <> b && b = c && c <> d) ||
    (b <> c && c = d && d <> e) ||
    (c <> d && d = e && e <> f) ||
    (d <> e && e = f)

  let isValid' i =
    let (a, b, c, d, e, f) = digits i
    hasPairNoTriple a b c d e f && not (isDecreasing a b c d e f)

  [<Fact>]
  let part2() =
    let from, to' = 147981, 691423
    let result =
      seq { from .. to' }
      |> Seq.countBy isValid'
      |> Seq.find fst
      |> snd
    result =! 1206

module Day05 =

  let runWithInput inp code =
    Intcode.run inp code |> fst |> Array.last

  let part1_input = [| "1002,4,3,4,33" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 5, 2845163)>]
  [<MemberData(nameof part1_sample, 99)>]
  let part1 (input: string array) expected =
    let code = input.[0].Split(',') |> Array.map int
    runWithInput [1] code =! expected

  let part2_input = [| "3,9,8,9,10,9,4,9,99,-1,8" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 5, 9436229)>]
  [<MemberData(nameof part2_sample, 0)>]
  let part2 (input: string array) expected =
    let code = input.[0].Split(',') |> Array.map int
    runWithInput [5] code =! expected

module Day06 =

  type Tree<'T> = Node of 'T * Tree<'T> list

  let buildMap (input : string[]) =
    input
    |> Array.map(fun s -> match s.Split(')') with [|a;b|] -> a, b | _ -> failwith "Expected two entries")
    |> Array.groupBy fst
    |> Array.map(fun (k, v) -> k, v |> Array.map snd |> List.ofArray)
    |> Map.ofArray

  let buildTree (input : string[]) =
    let map = buildMap input

    let rec helper key =
      match map |> Map.tryFind key with
      | Some v -> Node(key, v |> List.map helper)
      | None -> Node(key, [])

    helper "COM"

  let rec count depth (Node(_, nodes)) =
    depth + List.sumBy(count (depth + 1)) nodes

  let part1_input = [| "COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 6, 171213)>]
  [<MemberData(nameof part1_sample, 42)>]
  let part1 (input: string array) expected =
    let root = buildTree input
    count 0 root =! expected

  let buildMap2 (input : string[]) =
    input
    |> Array.map(fun s -> match s.Split(')') with [|a;b|] -> b, a | _ -> failwith "Expected two entries")
    |> Map.ofArray

  let findRoute map key =
    let rec helper key =
      match map |> Map.tryFind key with
      | Some value -> value :: helper value
      | None -> []
    helper key |> List.rev

  let findEarliestAncestor l1 l2 =
    let rec helper l1 l2 =
      match l1, l2 with
      | x1 :: t1, x2 :: t2 when x1 = x2 ->
        if x1 = x2 then helper t1 t2
        else (t1, t2)
      | _ -> (l1, l2)
    helper l1 l2

  let part2_input = [| "COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"; "K)YOU"; "I)SAN" |]
  let part2_sample (result: int) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 6, 292)>]
  [<MemberData(nameof part2_sample, 4)>]
  let part2 (input: string array) expected =
    let map2 = buildMap2 input
    let route1 = findRoute map2 "YOU"
    let route2 = findRoute map2 "SAN"
    let (r1, r2) = findEarliestAncestor route1 route2
    r1.Length + r2.Length =! expected

module Day07 =

  let run input code = Intcode.run input code |> fst |> Array.exactlyOne

  let part1_input = [| "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 7, 255590)>]
  [<MemberData(nameof part1_sample, 43210)>]
  let part1 (input: string array) expected =
    let program = input.[0].Split(',') |> Array.map int
    let mutable output = 0
    for a in 0 .. 4 do
      for b in 0 .. 4 do
        for c in 0 .. 4 do
          for d in 0 .. 4 do
            for e in 0 .. 4 do
              if Set.ofList [a;b;c;d;e] |> Set.count = 5 then
                let ampA = run [a;0] program
                let ampB = run [b;ampA] program
                let ampC = run [c;ampB] program
                let ampD = run [d;ampC] program
                let ampE = run [e;ampD] program
                output <- max output ampE
    output =! expected

module Day08 =

  [<Theory>]
  [<FileData(2019, 8, 1950)>]
  let part1 (input: string array) expected =
    let input, w, h = input.[0], 25, 6
    let result =
      input
      |> Seq.chunkBySize(w*h)
      |> Seq.map(Seq.countBy id >> Map.ofSeq)
      |> Seq.minBy(Map.find '0')
      |> fun m -> m.['1'] * m.['2']
    result =! expected

  [<Theory>]
  [<FileData(2019, 8, "FKAHL")>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let input, w, h = input.[0], 25, 6
    let result =
      input
      |> Seq.chunkBySize(w*h)
      |> Seq.reduce(Array.map2(fun c1 c2 -> if c1 = '2' then c2 else c1))
      |> Seq.chunkBySize w
      |> Seq.map String
      |> String.concat "\n"
      |> fun s -> s.Replace('0', ' ').Replace('1', '█')
    result =! expected

module Day09 =
  // Incomplete in original file
  ()

module Day10 =

  [<StructuredFormatDisplay("({X}, {Y})")>]
  type Pos = { X : int ; Y : int}
  with
    static member (-)(p : Pos, q : Pos) = p.X - q.X, p.Y - q.Y

  let getAsteroids (chars:string[]) =
    chars
    |> Array.mapi(fun y row ->
      row.ToCharArray()
      |> Array.mapi(fun x c -> y, x, c))
    |> Array.collect(Array.choose(fun (y, x, c) -> if c = '#' then Some { X = x ; Y =  y } else None))

  let reduce(a, b) =
    let g = gcd a b
    if g = 0 then (0,0) elif g < 0 then -a/g, -b/g else a/g, b/g

  let getMaxVisible(asteroids : Pos[]) =
    asteroids
    |> Array.map(fun p1 ->
      let counts =
        asteroids |> Array.distinctBy(fun p2 -> p2 - p1 |> reduce)
      p1, counts.Length - 1
    )
    |> Array.maxBy snd

  let part1_input = [| ".#..#"; "....."; "#####"; "....#"; "...##" |]
  let part1_sample (result: int) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 10, 214)>]
  [<MemberData(nameof part1_sample, 8)>]
  let part1 (input: string array) expected =
    let asteroids = getAsteroids input
    let (_, count) = getMaxVisible asteroids
    count =! expected

  let getVaporizationOrder (asteroids:Pos[]) p1 =
    let seen = new Dictionary<(int * int), int>()
    asteroids
    |> Array.filter(fun p2 -> p2 <> p1)
    |> Array.sortBy(fun p2 ->
      let (dx, dy) = p1 - p2
      dx * dx + dy * dy)
    |> Array.map(fun p2 ->
      let (dx, dy) = p1 - p2
      let angle = Math.Atan2(float dy, float dx) - Math.PI / 2.
      let angle = if angle < 0. then angle + 2. * Math.PI else angle
      let direction = reduce(dx, dy)
      let count =
        match seen.TryGetValue(direction) with
        | true, count -> count + 2
        | false, _ -> 0
      seen.[direction] <- count
      p2, angle + float count * Math.PI)
    |> Array.sortBy snd

  [<Theory>]
  [<FileData(2019, 10, 502)>]
  //[<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    let asteroids = getAsteroids input
    let p1 = getMaxVisible asteroids |> fst
    let v1 = getVaporizationOrder asteroids p1
    let (p, _) = v1.[199]
    p.X * 100 + p.Y =! expected

module Day11 =
  // Incomplete in original file
  ()

module Day12 =

  [<StructuredFormatDisplay("<x={X},y={Y},z={Z}>")>]
  type Vec3D =
    { X : int
      Y : int
      Z : int }
    static member Zero = { X = 0 ; Y = 0 ; Z = 0 }
    static member (+)(v1 : Vec3D, v2 : Vec3D) =
      { X = v1.X + v2.X
        Y = v1.Y + v2.Y
        Z = v1.Z + v2.Z }

  type Moon =
    { Pos : Vec3D
      Vel : Vec3D }

  let parsePositions values =
    values
    |> Array.map(fun (s : string) ->
      match s.Split("<>, =".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) with
      | [|_;x;_;y;_;z|] -> { Pos = { X = int x ; Y = int y ; Z = int z } ; Vel = Vec3D.Zero }
      | _ -> failwith "Invalid input")

  let acc (p1:Moon) (p2:Moon) = {
    X = p1.Pos.X - p2.Pos.X |> sign
    Y = p1.Pos.Y - p2.Pos.Y |> sign
    Z = p1.Pos.Z - p2.Pos.Z |> sign
  }

  let advance positions =
    positions
    |> Array.map(fun moon ->
      let dv = positions |> Array.sumBy(fun moon' -> acc moon' moon)
      let v = moon.Vel + dv
      { moon with Pos = moon.Pos + v ; Vel = v}
    )

  let advance' positions =
    positions
    |> Array.map(fun (x:int, v) ->
      let dv = positions |> Array.sumBy(fun (x', _) -> sign(x'- x))
      let v = v + dv
      x + v, v)

  let totalEnergy (moon:Moon) =
    let pos = moon.Pos
    let pot = abs pos.X + abs pos.Y + abs pos.Z
    let vel = moon.Vel
    let kin = abs vel.X + abs vel.Y + abs vel.Z
    pot * kin

  let rec apply count f x =
    if count = 0 then x else apply (count - 1) f (f x)

  let loop x f =
    let rec inner count x' =
      if x = x' then count else inner (count+1) (f x')
    inner 1 (f x)

  let part1_input = [| "<x=-1, y=0, z=2>"; "<x=2, y=-10, z=-7>"; "<x=4, y=-8, z=8>"; "<x=3, y=5, z=-1>" |]
  let part1_sample (result: int64) = makeSample result part1_input

  [<Theory>]
  [<FileData(2019, 12, 7179L)>]
  [<MemberData(nameof part1_sample, 179L)>]
  let part1 (input: string array) expected =
    let positions = parsePositions input
    let result =
      positions
      |> apply 1000 advance
      |> Array.sumBy totalEnergy
      |> int64
    result =! expected

  let part2_input = [| "<x=-1, y=0, z=2>"; "<x=2, y=-10, z=-7>"; "<x=4, y=-8, z=8>"; "<x=3, y=5, z=-1>" |]
  let part2_sample (result: int64) = makeSample result part2_input

  [<Theory>]
  [<FileData(2019, 12, 428576638953552L)>]
  [<MemberData(nameof part2_sample, 2772L)>]
  let part2 (input: string array) expected =
    let positions = parsePositions input
    let xs, ys, zs =
      positions
      |> Array.map(fun m -> (m.Pos.X, m.Vel.X), (m.Pos.Y, m.Vel.Y), (m.Pos.Z, m.Vel.Z))
      |> Array.unzip3
    let c1 = loop xs advance' |> int64
    let c2 = loop ys advance' |> int64
    let c3 = loop zs advance' |> int64
    let answer = lcm64(lcm64 c1 c2) c3
    answer =! expected

module Day13 =
  // Incomplete in original file
  ()

module Day14 =
  // Incomplete in original file
  ()