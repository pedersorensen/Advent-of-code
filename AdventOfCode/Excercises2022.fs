#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
Year <- 2022
#else
namespace Excercises2022
#endif

open Xunit
open System
open System.IO

type FileDataAttribute(day, result: obj) =
  inherit Sdk.DataAttribute()
  override _.GetData(_) =
    let path = ensureExists 2022 day
    [| [| File.ReadAllLines(path) |> box ; result |] |]

[<AutoOpen>]
module Utils =
  let (|Ints|) (data: string[]) = data |> Array.map int
  let (|SingeLineInts|) (data: string[]) = (Array.exactlyOne data).Split(',') |> Array.map int

  let makeSample result (data: string []) = [| [| box data ; box result |] |]

  let inline (=!) (actual : 'T) (expected : 'T) = Assert.Equal<'T>(expected, actual)

module Day01 =

  let sample (result: int) = makeSample result [|
    "1000"
    "2000"
    "3000"
    ""
    "4000"
    ""
    "5000"
    "6000"
    ""
    "7000"
    "8000"
    "9000"
    ""
    "10000"
  |]

  [<Theory>]
  [<FileData(1, 67_450)>]
  [<MemberData(nameof sample, 24_000)>]
  let part1 (input: string []) expected =
    input
    |> Array.chunkBy(String.IsNullOrWhiteSpace)
    |> Array.map(Array.sumBy int)
    |> Array.max
    =! expected

  [<Theory>]
  [<FileData(1, 199357)>]
  [<MemberData(nameof sample, 45_000)>]
  let part2 (input: string []) expected =
    input
    |> Array.chunkBy(String.IsNullOrWhiteSpace)
    |> Array.map(Array.sumBy int)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum
    =! expected

module Day02 =

  let sample (result: int) = makeSample result [|
    "A Y"
    "B X"
    "C Z"
  |]

  type Hand = Rock | Paper | Scissor

  let mapHand =
    function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissor
    | _ -> invalidOp ""

  let evaluate =
    function
    | Rock, Rock -> 3 + 1
    | Rock, Paper -> 6 + 2
    | Rock, Scissor -> 0 + 3
    | Paper, Rock -> 0 + 1
    | Paper, Paper -> 3 + 2
    | Paper, Scissor -> 6 + 3
    | Scissor, Rock -> 6 + 1
    | Scissor, Paper -> 0 + 2
    | Scissor, Scissor -> 3 + 3

  [<Theory>]
  [<FileData(2, 10404)>]
  [<MemberData(nameof sample, 15)>]
  let part1 (input: string []) expected =
    input
    |> Array.map(fun s ->
      match s.Split(' ') with
      | [|a ; b|] -> mapHand a, mapHand b
      | _ -> invalidOp ""
    )
    |> Array.sumBy evaluate
    =! expected

  let mapSnd =
    function
    | Rock, "X" -> Scissor
    | Rock, "Y" -> Rock
    | Rock, "Z" -> Paper
    | Paper, "X" -> Rock
    | Paper, "Y" -> Paper
    | Paper, "Z" -> Scissor
    | Scissor, "X" -> Paper
    | Scissor, "Y" -> Scissor
    | Scissor, "Z" -> Rock
    | _ -> invalidOp ""

  [<Theory>]
  [<FileData(2, 10334)>]
  [<MemberData(nameof sample, 12)>]
  let part2 (input: string []) expected =
    input
    |> Array.map(fun s ->
      match s.Split(' ') with
      | [|a ; b|] ->
        let a = mapHand a
        a, mapSnd (a, b)
      | _ -> invalidOp ""
    )
    |> Array.sumBy evaluate
    =! expected
