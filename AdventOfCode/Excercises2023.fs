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

      let indexes =
        digitsAsWords
        |> Array.mapi(fun value digit -> value, line.IndexOf(digit), line.LastIndexOf(digit))

      let minWordValue, i2, _ =
        indexes
        |> Array.minBy(fun (_, idx, _) ->
          if idx < 0 then Int32.MaxValue else idx
        )

      let minValue =
        if i < 0 || (-1 < i2 && i2 < i)
        then minWordValue
        else int(line[i] - '0')

      let maxWordValue, _, j2 = indexes |> Array.maxBy(fun (_, _, idx) -> idx)

      let maxValue =
        if j < 0 || (-1 < j2 && j2 > j)
        then maxWordValue
        else int(line[j] - '0')

      10 * minValue + maxValue
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
