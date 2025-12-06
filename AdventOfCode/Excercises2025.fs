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
