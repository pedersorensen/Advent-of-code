#load "Utils.fsx"

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
    |> Array.sumBy(fun (min, max, char, pw) ->
      let count = pw |> Seq.sumBy(fun c -> if c = char then 1 else 0)
      if min <= count && count <= max then 1 else 0
    )

  // 705
  let part2() =
    parsed
    |> Array.sumBy(fun (p1, p2, char, pw) ->
      let b1 = pw.[p1 - 1] = char
      let b2 = pw.[p2 - 1] = char
      if b1 <> b2 then 1 else 0
    )

//module Day3 =
//  let input = readInput 3

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
