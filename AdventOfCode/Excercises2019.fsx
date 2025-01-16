#r "nuget: XUnit"
#r "nuget: FSharp.Data"
#load "Utils.fs"

open Utils
open System

module List =

  open System.Reflection

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
        printfn "Read input: %i" en.Current
        program.[pos1] <- en.Current
        yield! inner (i+2)
      | OpCodes.Output ->
        printfn "yielding output: %i" val1.Value
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

module Day1 =
  // Part 1
  let getFuel mass = max 0 (mass / 3 - 2)
  getFuel 12
  getFuel 14
  getFuel 1969
  getFuel 100756

  let totalFuel =
    readInput 2019 1
    |> Array.sumBy(int >> getFuel)

  // Part 2
  let rec getFuel' mass =
    let fuel = getFuel mass
    if fuel > 0 then
      fuel + getFuel' fuel
    else fuel

  getFuel' 12
  getFuel' 14
  getFuel' 1969
  getFuel' 100756

  let totalFuel' =
    readInput 2019 1
    |> Array.sumBy(int >> getFuel')

module Day2 =
  open Intcode

  let example = "1,9,10,3,2,3,11,0,99,30,40,50".Split(',') |> Array.map int
  run [] example

  run [] [|1;0;0;0;99|] //becomes 2,0,0,0,99 (1 + 1 = 2).
  run [] [|2;3;0;3;99|] //becomes 2,3,0,6,99 (3 * 2 = 6).
  run [] [|2;4;4;5;99;0|] //becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
  run [] [|1;1;1;4;99;5;6;0;99|] //becomes 30,1,1,4,2,5,6,0,99.

  // Part 1
  let input = readsInts 2
  input.[1] <- 12
  input.[2] <- 2
  run [] input |> snd |> Array.item 0 // 3562624

  // Part 2
  for noun in { 0 .. 99 } do
    for verb in {0 .. 99 } do
      input.[1] <- noun
      input.[2] <- verb
      let res = run [] input |> snd
      if res.[0] = 19690720 then
        printfn "Noun: %i, verb: %i, total: %i" noun verb (100 * noun + verb) // 8298

module Day3 =
  open System.Collections.Generic
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

  let checkWire (wire : string) =
    ((0,0), wire.Split(','))
    ||> Array.fold(fun (x,y) d ->
      let l = int(string(d.Substring(1)))
      match d.[0] with
      | 'U' -> (x, y + l)
      | 'D' -> (x, y - l)
      | 'L' -> (x - l, y)
      | 'R' -> (x + l, y)
      | _ -> failwith "Invalid direction")

  buildWire "R8,U5,L5,D3" |> List.last
  checkWire "R8,U5,L5,D3"

  buildWire "U7,R6,D4,L4" |> List.last
  checkWire "U7,R6,D4,L4"

  buildWire "R75,D30,R83,U83,L12,D49,R71,U7,L72" |> List.last
  checkWire "R75,D30,R83,U83,L12,D49,R71,U7,L72"

  buildWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" |> List.last
  checkWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"

  let getClosestInsersection w1 w2 =
    let points1 = buildWire w1 |> HashSet
    let points2 = buildWire w2
    points1.IntersectWith(points2)
    let _ = points1.Remove(0,0)
    points1 |> Seq.minBy(fun (x, y) -> abs x + abs y)

  getClosestInsersection "R8,U5,L5,D3" "U7,R6,D4,L4"
  getClosestInsersection
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    "U62,R66,U55,R34,D71,R55,D58,R83" // distance 159
  getClosestInsersection
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" // distance 135

  // Part 1
  let (input1, input2) =
    match readInput 2019 3 with
    | [|input1 ; input2 |] -> input1, input2
    | _ -> failwith "Invalid input"

  let (x, y) = getClosestInsersection input1 input2
  let d = x + y // 1983

  // Part 2
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

  getShortestIntersection "R8,U5,L5,D3" "U7,R6,D4,L4" // distance 30
  getShortestIntersection
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    "U62,R66,U55,R34,D71,R55,D58,R83" // distance 610
  getShortestIntersection
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" // distance 410
  getShortestIntersection input1 input2 // 107754

module Day4 =

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

  let from, to' = 147981, 691423

  isValid 111111
  isValid 111123
  isValid 135679
  isValid 122345
  isValid 223450
  isValid 123789
  digits 123456

  // Part 1
  let validPasswords =
    {from .. to'}
    |> Seq.countBy isValid
    |> Seq.find fst
    |> snd

  // Part 2
  let hasPairNoTriple a b c d e f =
    (a = b && b <> c) ||
    (a <> b && b = c && c <> d) ||
    (b <> c && c = d && d <> e) ||
    (c <> d && d = e && e <> f) ||
    (d <> e && e = f)

  let isValid' i =
    let (a, b, c, d, e, f) = digits i
    hasPairNoTriple a b c d e f && not (isDecreasing a b c d e f)

  isValid' 688889
  isValid' 122345

  let validPasswords' =
    {from .. to'}
    |> Seq.countBy isValid'
    |> Seq.find fst
    |> snd

module Day5 =
  open Intcode

  run [0] [|1002;4;3;4;33|]
  run [0] [|1101;100;-1;4;0|]
  let input = readsInts 5
  run [1] input |> fst |> Array.last // 2845163
  // Part 2
  fst <| run [9] [|3;9;8;9;10;9;4;9;99;-1;8|] // - Using position mode; consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
  fst <| run [1] [|3;9;7;9;10;9;4;9;99;-1;8|] // - Using position mode; consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
  fst <| run [1] [|3;3;1108;-1;8;3;4;3;99|] // - Using immediate mode; consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
  fst <| run [8] [|3;3;1107;-1;8;3;4;3;99|] // - Using immediate mode; consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).

  //Here are some jump tests that take an input, then output 0 if the input was
  //zero or 1 if the input was non-zero:
  fst <| run [-10] [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|] // (using position mode)
  fst <| run [-10] [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|] // (using immediate mode)

  fst <| run [9] [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
  fst <| run [5] input // 9436229

module Day6 =

  type Tree<'T> = Node of 'T * Tree<'T> list

  let printTree (tree : Tree<string>) =
    let rec helper depth (Node(value, nodes)) =
      match nodes with
      | [] -> sprintf "%*s" depth value
      | nodes ->
        nodes
        |> List.map(helper (depth + 2))
        |> String.concat "\r\n"
        |> sprintf "%*s:\r\n%s" depth value
    helper 0 tree
  fsi.AddPrinter printTree

  let input = readInput 2019 6
  let example = [| "COM)B" ; "B)C" ; "C)D" ; "D)E" ; "E)F" ; "B)G" ; "G)H" ; "D)I" ; "E)J" ; "J)K" ; "K)L" |]

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

  // Part 1
  let root1 = buildTree example
  let root2 = buildTree input
  let rec count depth (Node(_, nodes)) =
    depth + List.sumBy(count (depth + 1)) nodes

  count 0 root1
  count 0 root2

  // Part 2

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

  let findEarliestAncester l1 l2 =
    let rec helper l1 l2 =
      match l1, l2 with
      | x1 :: t1, x2 :: t2 when x1 = x2 ->
        if x1 = x2 then helper t1 t2
        else (t1, t2)
      | _ -> (l1, l2)
    helper l1 l2

  let example2 = [| "COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L"; "K)YOU"; "I)SAN"|]
  let map2 = buildMap2 input
  let route1 = findRoute map2 "YOU"
  let route2 = findRoute map2 "SAN"
  let (r1, r2) = findEarliestAncester route1 route2
  r1.Length + r2.Length

module Day7 =

  let input = readsInts 7

  //let example = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0".Split(',') |> Array.map int
  //let example = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0".Split(',') |> Array.map int
  let example = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0".Split(',') |> Array.map int
  let run input code = Intcode.run input code |> fst |> Array.exactlyOne
  let a, b, c, d, e = 1,0,4,3,2
  let mutable output = 0
  for a in { 0 .. 4 } do
    for b in { 0 .. 4 } do
      for c in { 0 .. 4 } do
        for d in { 0 .. 4 } do
          for e in { 0 .. 4 } do
            let ampA = run [a;0] example
            let ampB = run [b;ampA] example
            let ampC = run [c;ampB] example
            let ampD = run [d;ampC] example
            let ampE = run [e;ampD] example
            if ampE > output then
              output <- ampE
              printfn "%A, Amp: %i" (a, b, c, d, e) ampE
  output

module Day8 =
  let input, w, h = readInput 2019 8 |> Array.exactlyOne, 25, 6
  // Part 1
  let result =
    input
    |> Seq.chunkBySize(w*h)
    |> Seq.map(Seq.countBy id >> Map.ofSeq)
    |> Seq.minBy(Map.find '0')
    |> fun m -> m.['1'] * m.['2'] // 1950

  // Part 2
  input
  |> Seq.chunkBySize(w*h)
  |> Seq.reduce(Array.map2(fun c1 c2 -> if c1 = '2' then c2 else c1))
  |> Seq.chunkBySize w
  |> Seq.map String
  |> String.concat "\r\n"
  |> fun s -> s.Replace('0', ' ').Replace('1', '█')
  |> printfn "%s" // FKAHL

module Day9 =
  let input = readsInts 9

module Day10 =
  open System.Collections.Generic

  let printField (chars:string[]) = "\r\n" + (chars |> String.concat "\r\n" )
  fsi.AddPrinter printField

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

  let fields =
    [|
      [|
        ".#..#"
        "....."
        "#####"
        "....#"
        "...##"
      |]
      [|
        "......#.#."
        "#..#.#...."
        "..#######."
        ".#.#.###.."
        ".#..#....."
        "..#....#.#"
        "#..#....#."
        ".##.#..###"
        "##...#..#."
        ".#....####"
      |]
      [|
        "#.#...#.#."
        ".###....#."
        ".#....#..."
        "##.#.#.#.#"
        "....#.#.#."
        ".##..###.#"
        "..#...##.."
        "..##....##"
        "......#..."
        ".####.###."
      |]
      [|
        ".#..#..###"
        "####.###.#"
        "....###.#."
        "..###.##.#"
        "##.##.#.#."
        "....###..#"
        "..#.#..#.#"
        "#..#.#.###"
        ".##...##.#"
        ".....#.#.."
      |]
      [|
        ".#..##.###...#######"
        "##.############..##."
        ".#.######.########.#"
        ".###.#######.####.#."
        "#####.##.#.##.###.##"
        "..#####..#.#########"
        "####################"
        "#.####....###.#.#.##"
        "##.#################"
        "#####.##.###..####.."
        "..######..##.#######"
        "####.##.####...##..#"
        ".#####..#.######.###"
        "##...#.##########..."
        "#.##########.#######"
        ".####.#.###.###.#.##"
        "....##.##.###..#####"
        ".#.#.###########.###"
        "#.#.#.#####.####.###"
        "###.##.####.##.#..##"
      |]
      readInput 2019 10
    |]
    |> Array.map getAsteroids

  let getMaxVisible(asteroids : Pos[]) =
    asteroids
    |> Array.map(fun p1 ->
      let counts =
        asteroids |> Array.distinctBy(fun p2 -> p2 - p1 |> reduce)
      p1, counts.Length - 1
    )
    |> Array.maxBy snd

  // Part 1
  fields
  |> Array.map getMaxVisible
  |> Array.iter(fun (pos, count) -> printfn "%A: %i" pos count) // 214

  // Part 2
  let field =
    [|
      ".#....#####...#.."
      "##...##.#####..##"
      "##...#...#.#####."
      "..#.....X...###.."
      "..#.#.....#....##"
    |]

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

  let f = fields.[5]
  let p1 = getMaxVisible f |> fst
  let v1 = getVaporizationOrder f p1
  v1 |> Array.iteri(fun i (p, _) -> printfn "%i: %A" (i+1) p)
  v1.[199] // 502

module Day11 =
  let input = readsInt64s 11

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
    static member (-)(v1 : Vec3D, v2 : Vec3D) =
      { X = v1.X - v2.X
        Y = v1.Y - v2.Y
        Z = v1.Z - v2.Z }
    static member (~-)(v2 : Vec3D) =
      { X = -v2.X
        Y = -v2.Y
        Z = -v2.Z }

  type Moon =
    { Pos : Vec3D
      Vel : Vec3D }
    override this.ToString() =
      sprintf "pos=%A, vel=%A" this.Pos this.Vel

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

  let positions1 =
    [| "<x=-1, y=0, z=2>"
       "<x=2, y=-10, z=-7>"
       "<x=4, y=-8, z=8>"
       "<x=3, y=5, z=-1>" |]
    |> parsePositions

  let positions2 =
    [| "<x=-8, y=-10, z=0>"
       "<x=5, y=5, z=10>"
       "<x=2, y=-7, z=3>"
       "<x=9, y=-8, z=-3>"|]
    |> parsePositions
  let positions = readInput 2019 12 |> parsePositions

  positions1
  |> apply 1000 advance
  |> Array.sumBy totalEnergy // 7179
  //|> clip

  // Part 2
  let xs, ys, zs =
    positions
    |> Array.map(fun m -> (m.Pos.X, m.Vel.X), (m.Pos.Y, m.Vel.Y), (m.Pos.Z, m.Vel.Z))
    |> Array.unzip3

  let c1 = loop xs advance' |> int64
  let c2 = loop ys advance' |> int64
  let c3 = loop zs advance' |> int64

  let answer = lcm64(lcm64 c1 c2) c3 // 428576638953552L

module Day13 =
  let input = readInput 2019 13

module Day14 =
  open System.Collections.Generic

  type Reaction =
    { Input : (string * int)[]
      Output : string * int }
    static member Parse(s : string) =
      let arr =
        s.Split(" ,=>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> Array.chunkBySize 2
        |> Array.map(function [|a;b|] -> b, int a | _ -> failwith "Array must have two entries")
      { Input = arr |> Array.take(arr.Length - 1)
        Output = Array.last arr }

  let print (r : Reaction) =
      let (a, b) = r.Output
      r.Input |> Array.map(fun (p, q) -> sprintf "%i %s" q p)
      |> String.concat ", "
      |> fun s -> sprintf "%s => %i %s" s b a

  fsi.AddPrinter print
  fsi.AddPrinter<Reaction[]>(Array.map print >> String.concat "\r\n")

  let reactions =
    [|
      [|
        "10 ORE => 10 A"
        "1 ORE => 1 B"
        "7 A, 1 B => 1 C"
        "7 A, 1 C => 1 D"
        "7 A, 1 D => 1 E"
        "7 A, 1 E => 1 FUEL"
      |]
      [|
        "9 ORE => 2 A"
        "8 ORE => 3 B"
        "7 ORE => 5 C"
        "3 A, 4 B => 1 AB"
        "5 B, 7 C => 1 BC"
        "4 C, 1 A => 1 CA"
        "2 AB, 3 BC, 4 CA => 1 FUEL"
      |]
      [|
        "157 ORE => 5 NZVS"
        "165 ORE => 6 DCFZ"
        "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
        "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
        "179 ORE => 7 PSHF"
        "177 ORE => 5 HKGWZ"
        "7 DCFZ, 7 PSHF => 2 XJWVT"
        "165 ORE => 2 GPVTF"
        "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
      |]
      [|
        "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
        "17 NVRVD, 3 JNWZP => 8 VPVL"
        "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
        "22 VJHF, 37 MNCFX => 5 FWMGM"
        "139 ORE => 4 NVRVD"
        "144 ORE => 7 JNWZP"
        "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
        "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
        "145 ORE => 6 MNCFX"
        "1 NVRVD => 8 CXFTF"
        "1 VJHF, 6 MNCFX => 4 RFSQX"
        "176 ORE => 6 VJHF"
      |]
      [|
        "171 ORE => 8 CNZTR"
        "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
        "114 ORE => 4 BHXH"
        "14 VRPVC => 6 BMBT"
        "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
        "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
        "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
        "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
        "5 BMBT => 4 WPTQ"
        "189 ORE => 9 KTJDG"
        "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
        "12 VRPVC, 27 CNZTR => 2 XDBXC"
        "15 KTJDG, 12 BHXH => 5 XCVML"
        "3 BHXH, 2 VRPVC => 7 MZWV"
        "121 ORE => 7 VRPVC"
        "7 XCVML => 6 RJRHP"
        "5 BHXH, 4 VRPVC => 5 LTCX"
      |]
      readInput 2019 14
    |]
    |> Array.map(Array.map Reaction.Parse)

  type IDictionary<'TKey, 'TValue> with
    member this.GetOrAdd(key, defaultValue) =
      match this.TryGetValue key with
      | true, value -> value
      | false, _ ->
        this.[key] <- defaultValue
        defaultValue

  let inline (+=) (q:'T ref) (qty : 'T) =
    q := !q + qty

  let inline (-=) (q:'T ref) (qty : 'T) =
    if qty > !q then failwith "Result must not become negative."
    q := !q - qty

  // TODO
  // - Check for existing quantity.
  // - If non-zero subtract required quantity from existing.
  // - Reduce existing quantity.
  // - Create remaining quantity as required.

  let reactantsMap =
    reactions.[0]
    |> Array.map(fun r -> fst r.Output, (snd r.Output, r.Input))
    |> Map.ofArray

  let make (product:string) (qty:int) =
    let (minQty, reactants) = reactantsMap.[product]
    minQty, reactants

  let qty = 1
  let product ="FUEL"
  make product qty
