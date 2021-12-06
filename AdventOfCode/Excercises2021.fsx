#load "Utils.fsx"

open Utils

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


