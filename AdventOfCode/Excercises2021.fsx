#load "Utils.fsx"

open Utils
open System.Collections.Generic

Year <- 2021

module Day1 =

  let input = readInput 1 |> Array.map int

  let sample = [|
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
  |]

  let countIncreases (array: int[]) =
    let rec loop acc i =
      if i = array.Length then acc else
      let acc' =
        if array.[i] > array.[i - 1] then acc + 1
        else acc
      loop acc' (i + 1)
    loop 0 1
  countIncreases input

  // 1387
  let part1() =
    input
    |> Seq.pairwise
    |> Seq.where(fun (a, b) -> a < b)
    |> Seq.length

  let part1'() =
    countIncreases input

  // 1362
  let part2() =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countIncreases

module Day2 =

  let input = readInput 2

  let sample = [|
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
  |]

  let (|Up|Down|Forward|) (s: string) =
    match s.Split(' ') with
    | [| "up"      ; x |] -> Up     (int x)
    | [| "down"    ; x |] -> Down   (int x)
    | [| "forward" ; x |] -> Forward(int x)
    | _ -> failwithf $"Unknown instruction: '{s}'"

  let moveIt directions =
    (struct(0, 0), directions)
    ||> Array.fold(fun struct(h, d) s ->
      match s with
      | Up      x -> h, d - x
      | Down    x -> h, d + x
      | Forward x -> h + x, d
    )
    |> fun struct(h, d) -> h * d

  // 2147104
  let part1() =
    moveIt input

  let moveItAgain directions =
    (struct(0, 0, 0), directions)
    ||> Array.fold(fun struct(h, d, aim) s ->
      match s with
      | Up x      -> h, d, aim - int x
      | Down x    -> h, d, aim + int x
      | Forward x -> h + int x, d + aim * int x, aim
    )
    |> fun struct(h, d, _) -> h * d

  // 2044620088
  let part2() =
    moveItAgain input

module Day3 =

  let input = readInput 3

  let sample = [|
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
  |]

  let binaryToInt digits =
    let struct (value, _) =
      (digits, struct(0, 1))
      ||> Seq.foldBack(fun s struct(n, powerOfTwo) ->
        let n' = if s = '0' then n else n + powerOfTwo
        n', 2 * powerOfTwo
      )
    value

  let getReadings (input: string[]) =
    let counts = Array.zeroCreate input[0].Length
    for s in input do
      for i = 0 to s.Length - 1 do
        let struct(zero, one) = counts[i]
        counts[i] <-
          if s[i] = '0'
          then zero + 1, one
          else zero, one + 1
    let gamma, epsilon =
      counts
      |> Array.map(fun struct(zero, one) -> if zero < one then '1', '0' else '0', '1')
      |> Array.unzip
    binaryToInt gamma, binaryToInt epsilon

  // 3847100
  let part1() =
    let (gamma, epsilon) = getReadings input
    gamma * epsilon

  let getFilterBit (a,b) i (input: string[]) =
    let struct(zeros, ones) =
      (struct(0,0), input)
      ||> Seq.fold(fun struct(z, o) s ->
        if s[i] = '0'
        then z + 1, o
        else z, o + 1
      )
    if ones < zeros then a else b

  let reduce m i (input: string[]) =
    let bit = getFilterBit m i input
    input |> Array.filter(fun s -> s[i] = bit)

  let filter m (input: string[]) =
    let rec loop i data =
      if Array.length data = 1
      then data[0]
      else reduce m i data |> loop (i + 1)
    loop 0 input

  // 4105235
  let part2() =
    let maxParam = ('0', '1')
    let minParam = ('1', '0')
    let oxygenRate = filter maxParam input |> binaryToInt
    let co2Rate = filter minParam input |> binaryToInt
    oxygenRate * co2Rate

module Day5 =

  let input = readInput 5

  let sample = [|
    "0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"
  |]

  let countOverlaps skipDiagonals (input: string[]) =
    let mutable overlaps = 0
    let first = HashSet()
    let second = HashSet()

    let add x y =
      let point = struct(x, y)
      if first.Add(point) |> not && second.Add(point) then
        overlaps <- overlaps + 1

    for s in input do
      let p = s.Split([|' ' ; ','|])
      let x1 = int p[0]
      let y1 = int p[1]
      let x2 = int p[3]
      let y2 = int p[4]

      let dx, dy = (x2 - x1), (y2 - y1)
      if not skipDiagonals || dx = 0 || dy = 0 then
        let l = max (abs dx) (abs dy)
        let deltax, deltay = sign dx, sign dy

        add x1 y1
        add x2 y2

        for i = 1 to l - 1 do
          let x = x1 + i * deltax
          let y = y1 + i * deltay
          add x y

    overlaps

  // 8622
  let part1() =
    //sample |> countOverlaps true
    input |> countOverlaps true

  // 22037
  let part2() =
    //sample |> countOverlaps false
    input |> countOverlaps false

module Day6 =

  let input = readInput 6 |> Array.exactlyOne

  let sample = "3,4,3,1,2"

  let parse (input: string) = input.Split(',') |> Array.map int

  let advance (array: ResizeArray<_>) =
    let toAdd = ResizeArray()
    for i = 0 to array.Count - 1 do
      let v = array[i] - 1
      if v < 0 then
        toAdd.Add(8)
        array[i] <- 6
      else
        array[i] <- v
    array.AddRange(toAdd)

  let simulate days input =
    let arr = ResizeArray(parse input)
    for _ = 0 to days - 1 do
      advance arr
    arr.Count

  // 374927
  let part1() =
    //simulate 80 sample
    simulate 80 input

  let advance2 (days: int64[]) =
    let first = days[0]
    for i = 1 to days.Length - 1 do
      days[i - 1] <- days[i]
    days[6] <- days[6] + first
    days[8] <- first

  let simulate2 days input =
    let fish = parse input
    let daysLeft = Array.zeroCreate 9
    for f in fish do
      daysLeft[f] <- daysLeft[f] + 1L
    for _ = 0 to days - 1 do
      advance2 daysLeft
    Array.sum daysLeft

  // 1687617803407
  let part2() =
    //simulate2 80 sample
    //simulate2 256 sample
    //simulate2 80 input
    simulate2 256 input

module Day7 =

  let input = readsInts 7

  let sample = "16,1,2,0,4,2,7,1,2,14".Split(',') |> Array.map int

  let getFuelCost(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> abs(p - i))
    pos
    |> Array.minBy getCost
    |> getCost

  // 347509
  let part1() =
    //getFuelCost sample // 37
    getFuelCost input

  let getFuelCost2(input: int[]) =
    let m = Array.max input
    let pos = Array.init m id
    let getCost p = input |> Array.sumBy(fun i -> let n =  abs(p - i) in n * (n + 1) / 2)
    pos
    |> Array.minBy getCost
    |> getCost

  // 98257206
  let part2() =
    //getFuelCost2 sample // 168
    getFuelCost2 input
