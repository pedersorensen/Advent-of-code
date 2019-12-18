#r @"C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\Common7\IDE\CommonExtensions\Microsoft\FSharp\FSharp.Compiler.Interactive.Settings.dll"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput day = File.ReadAllLines("input2019/" + day + ".txt")
let readsInts day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int

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
    readInput "day1"
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
    readInput "day1"
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
  let input = readsInts "day2"
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
    match readInput "day3" with
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
  let input = readsInts "day5"
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

  let input = readInput "day6"
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

  let input = readsInts "day7"

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
  let input, w, h = readInput "day8" |> Array.exactlyOne, 25, 6
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
  let input = readsInts "day9"

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

  let rec gcd a b = if b = 0 then a else gcd b (a % b)
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
      readInput "day10"
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
