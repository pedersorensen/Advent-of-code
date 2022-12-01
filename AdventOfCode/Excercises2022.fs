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
