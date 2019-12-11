open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput file = File.ReadAllLines("input2019/" + file + ".txt")

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
        list
      | _ :: tail -> helper tail
    match list2 with
    | [] -> list1
    | _ -> helper list1

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
  module OpCodes =
    let [<Literal>] Add = 1
    let [<Literal>] Mult = 2
    let [<Literal>] Stop = 99

  let run (program : int []) =
    let program = program |> Array.copy
    let rec inner i =
      let code = program.[i]
      if code = OpCodes.Stop then program else
      let pos1 = program.[i+1]
      let pos2 = program.[i+2]
      let pos3 = program.[i+3]
      program.[pos3] <- 
        if   code = OpCodes.Add  then program.[pos1] + program.[pos2]
        elif code = OpCodes.Mult then program.[pos1] * program.[pos2]
        else failwith ""
      inner (i+4)
    inner 0

  let example = "1,9,10,3,2,3,11,0,99,30,40,50".Split(',') |> Array.map int
  run example

  run [|1;0;0;0;99|] //becomes 2,0,0,0,99 (1 + 1 = 2).
  run [|2;3;0;3;99|] //becomes 2,3,0,6,99 (3 * 2 = 6).
  run [|2;4;4;5;99;0|] //becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
  run [|1;1;1;4;99;5;6;0;99|] //becomes 30,1,1,4,2,5,6,0,99.

  // Part 1
  let input = (readInput "day2" |> Array.exactlyOne).Split(',') |> Array.map int
  input.[1] <- 12
  input.[2] <- 2
  run input

  // Part 2
  for noun in { 0 .. 99 } do
    for verb in {0 .. 99 } do
      input.[1] <- noun
      input.[2] <- verb
      let res = run input
      if res.[0] = 19690720 then
        printfn "Noun: %i, verb: %i, total: %i" noun verb (100 * noun + verb)

module Day3 =

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
            | 'U' -> List.init l (fun l -> x, y + l + 1)
            | 'D' -> List.init l (fun l -> x, y - l - 1)
            | 'L' -> List.init l (fun l -> x - l - 1, y)
            | 'R' -> List.init l (fun l -> x + l + 1, y)
            | _ -> failwith "Invalid direction"
          List.setTail (wire' |> List.rev) wire
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
    let wire1 = buildWire w1
    let wire2 = buildWire w2
    let points1 = wire1 |> Set.ofList
    let points2 = wire2 |> Set.ofList

    (points1, points2)
    ||> Set.intersect
    |> Set.remove (0,0)
    |> Seq.minBy(fun (x, y) -> abs x + abs y)

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
  let d = x + y

  // Part 2
  let getShortestIntersection w1 w2 =
    let wire1 = buildWire w1
    let wire2 = buildWire w2
    let points1 = wire1 |> Set.ofList
    let points2 = wire2 |> Set.ofList

    let intersections =
      (points1, points2)
      ||> Set.intersect
      |> Set.remove (0,0)
      |> Set.toList

    intersections
    |> List.map(fun p ->
      let d1 = wire1 |> List.findIndex((=) p)
      let d2 = wire2 |> List.findIndex((=) p)
      d1 + d2)
    |> List.min

  getShortestIntersection "R8,U5,L5,D3" "U7,R6,D4,L4" // distance 30
  getShortestIntersection 
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    "U62,R66,U55,R34,D71,R55,D58,R83" // distance 610
  getShortestIntersection 
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" // distance 410
  getShortestIntersection input1 input2

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
