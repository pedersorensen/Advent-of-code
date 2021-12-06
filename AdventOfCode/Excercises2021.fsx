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
