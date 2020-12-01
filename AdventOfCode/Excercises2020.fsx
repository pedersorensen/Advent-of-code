open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput day = File.ReadAllLines("input2020/" + day + ".txt")
let readsInts day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int
let readsInt64s day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int64

module Day1 =
  let input = readInput "day1" |> Array.map int |> Set.ofArray

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
    |> Option.iter(fun (a, b) -> printfn "Product: %i * %i : %i" a b (a * b))

  // 232508760
  let part2() =
    for i in input do
      input
      |> Set.remove i
      |> tryFindMatch (2020 - i)
      |> Option.iter(fun (a, b) ->
        printfn "Product: %i * %i * %i : %i" i a b (i * a * b))
