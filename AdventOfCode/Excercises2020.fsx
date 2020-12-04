﻿#load "Utils.fsx"

open Utils
open System

Year <- 2020

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

//module Day4 =
//  let input = readInput 4

//module Day5 =
//  let input = readInput 5

//module Day6 =
//  let input = readInput 6

//module Day7 =
//  let input = readInput 7

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
