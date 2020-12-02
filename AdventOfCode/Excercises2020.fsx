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
