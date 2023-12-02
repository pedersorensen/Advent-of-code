#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
Year <- 2023
#else
namespace Excercises2023
#endif

open Xunit
open System
open System.Buffers

module Day01 =

  let sample1 (result: int) = makeSample result [|
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
  |]

  let digits = SearchValues.Create("0123456789")

  [<Theory>]
  [<FileData(2023, 1, 54_634)>]
  [<MemberData(nameof sample1, 142)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)
      10 * int(line[i] - '0') + int(line[j] - '0')
    ) =! expected

  let sample2 (result: int) = makeSample result [|
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
  |]

  let digitsAsWords =
    [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

  [<Theory>]
  [<FileData(2023, 1, 53855)>]
  [<MemberData(nameof sample2, 281)>]
  let part2 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)

      let words =
        digitsAsWords
        |> Array.mapi(fun value d -> value, line.IndexOf(d))
        |> Array.choose(fun (value, idx) -> if idx < 0 then None else Some (value, idx))

      let words2 =
        digitsAsWords
        |> Array.mapi(fun value d -> value, line.LastIndexOf(d))
        |> Array.choose(fun (value, idx) -> if idx < 0 then None else Some (value, idx))

      let i =
        match words with
        | [||] -> int(line[i] - '0')
        | _ ->
          let minValue, i2 = words |> Array.minBy snd
          let min = if i >= 0 && i < i2 then int(line[i] - '0') else minValue
          min

      let j =
        match words2 with
        | [||] -> int(line[j] - '0')
        | _ ->
          let maxValue, j2 = words2 |> Array.maxBy snd
          let max = if j >= 0 && j > j2 then int(line[j] - '0') else maxValue
          max
      10 * i + j

    )
    =! expected

#if INTERACTIVE

let makeTemplate day =

  sprintf """module Day%02i =

  let input = [|
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2022, %i, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string []) expected =
    0 =! expected

  [<Theory>]
  [<FileData(2022, %i, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    0 =! expected""" day day day

makeTemplate |> clip

#endif
