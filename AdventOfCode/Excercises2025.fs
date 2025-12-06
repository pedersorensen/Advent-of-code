#if INTERACTIVE
#r "nuget: System.Drawing.Common"
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2025
#endif

open System
open Xunit

#if INTERACTIVE
makeTemplate 2025 1 |> clip
#endif

module Day02 =

  let input = [|
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2025, 2, 21139440284L)>]
  [<MemberData(nameof sample, 1227775554)>]
  let part1 (input: string array) expected =
    let numbers = parseNumbers<int64> input[0]
    let mutable sum = 0L
    for i in 0 .. numbers.Length / 2 - 1 do
      for i in numbers[2 * i] .. numbers[2 * i + 1] do
        let s = i.ToString()
        if s.Length % 2 = 0 then
          let half  = s.Length / 2
          let left  = s.AsSpan(0, half)
          let right = s.AsSpan(half, half)
          if left.SequenceEqual(right) then
            sum <- sum + i
    sum =! expected

  [<Theory>]
  [<FileData(2025, 2, -1)>]
  [<MemberData(nameof sample, 4174379265L)>]
  let part2 (input: string array) expected =
    let numbers = parseNumbers<int64> input[0]
    let mutable sum = 0L
    for i in 0 .. numbers.Length / 2 - 1 do
      for i in numbers[2 * i] .. numbers[2 * i + 1] do
        let s = i.ToString()
        if s.Length % 2 = 0 then
          let half = s.Length / 2
          let left = s.AsSpan(0, half)
          let right = s.AsSpan(half, half)
          if left.SequenceEqual(right) then
            sum <- sum + i
    sum =! expected

module Day01 =

  let input = [|
    "L68"
    "L30"
    "R48"
    "L5"
    "R60"
    "L55"
    "L1"
    "L99"
    "R14"
    "L82"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map(fun s ->
      let sign = if s[0] = 'L' then 1 else -1
      sign * Int32.Parse(s.AsSpan().Slice(1))
    )

  let dial parsed =
    ((50, 0), parsed)
    ||> Array.fold(fun (agg, zeroCount) next ->
      let value = (agg - next + 100) % 100
      let zeroCount = if value = 0 then zeroCount + 1 else zeroCount
      value, zeroCount
    )
    |> snd

  [<Theory>]
  [<FileData(2025, 1, 1066)>]
  [<MemberData(nameof sample, 3)>]
  let part1 (input: string array) expected =
    parse input |> dial =! expected

  [<Theory>]
  [<FileData(2025, 1, 6223)>]
  [<MemberData(nameof sample, 6)>]
  let part2 (input: string array) expected =
    parse input
    |> Array.collect(fun n ->
      let arr = Array.zeroCreate (abs n)
      Array.fill arr 0 arr.Length (sign n)
      arr
    )
    |> dial
    =! expected
