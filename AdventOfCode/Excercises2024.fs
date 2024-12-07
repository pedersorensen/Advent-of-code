#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2024
#endif

open Xunit
open System
open System.Buffers
open System.Collections.Generic
open System.Text.RegularExpressions

module Day7 =

  let input = [|
    "190: 10 19"
    "3267: 81 40 27"
    "83: 17 5"
    "156: 15 6"
    "7290: 6 8 6 15"
    "161011: 16 10 13"
    "192: 17 8 14"
    "21037: 9 7 18 13"
    "292: 11 6 16 20"
  |]

  let sample (result: int64) = makeSample result input

  let parse input =
    input
    |> Array.map(fun (x:string) ->
      x.Split([| ' ' ; ':'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int64
    )

  [<Theory>]
  [<FileData(2024, 7, 6392012777720L)>]
  [<MemberData(nameof sample, 3749L)>]
  let part1 (input: string array) expected =

    let test (line : _ array) =
      let result = line[0]
      let rec expand i acc = seq {
        if i < line.Length && acc <= result then
          let x = line[i]
          let i = i + 1
          yield! expand i (acc + x)
          yield! expand i (acc * x)
        else yield acc
      }
      expand 2 line[1]
      |> Seq.tryFind ((=) result)

    parse input
    |> Array.sumBy(fun line -> test line |> Option.defaultValue 0L)
    =! expected

  [<Theory>]
  [<FileData(2024, 7, 61561126043536L)>]
  [<MemberData(nameof sample, 11387L)>]
  let part2 (input: string array) expected =

    let concat a b =
      b + a * int64(Math.Pow(10, Math.Ceiling(Math.Log10(float b + 1.))))

    let test (line : _ array) =
      let result = line[0]
      let rec expand i acc = seq {
        if i < line.Length && acc <= result then
          let x = line[i]
          let i = i + 1
          yield! expand i (acc + x)
          yield! expand i (acc * x)
          yield! expand i (concat acc x)
        else yield acc
      }
      expand 2 line[1]
      |> Seq.tryFind ((=) result)

    parse input
    |> Array.sumBy(fun line -> test line |> Option.defaultValue 0L)
    =! expected

module Day3 =

  let input = [|
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  |]

  let sample (result: int) = makeSample result input

  let sumMatch (m: Match) =
    let d1 = m.Groups["d1"].Value |> int
    let d2 = m.Groups["d2"].Value |> int
    d1 * d2

  [<Theory>]
  [<FileData(2024, 3, 185797128)>]
  [<MemberData(nameof sample, 161)>]
  let part1 (input: string array) expected =
    Regex.Matches(input[0], "mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)")
    |> Seq.sumBy sumMatch
     =! expected

  let input2 = [|
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  |]

  let sample2 (result: int) = makeSample result input2

  [<Theory>]
  [<FileData(2024, 3, 89798695)>]
  [<MemberData(nameof sample2, 48)>]
  let part2 (input: string array) expected =
    ((0, true), Regex.Matches(input[0], "(mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)|do\(\)|don't\(\))"))
    ||> Seq.fold(fun (sum, doIt) m ->
      if   m.Value = "do()"    then sum, true
      elif m.Value = "don't()" then sum, false
      else
        if doIt
        then sum + sumMatch m, doIt
        else sum, doIt
    )
    |> fst
     =! expected

module Day2 =

  let input = [|
    "7 6 4 2 1"
    "1 2 7 8 9"
    "9 7 6 2 1"
    "1 3 2 4 5"
    "8 6 4 4 1"
    "1 3 6 7 9"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map (fun x ->
      x.Split(' ', StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int
    )

  let isSafe array =
    let pairs = array |> Array.pairwise |> Array.map (fun (a, b) -> a - b)
    (    pairs |> Array.forall (fun x -> x > 0)
      || pairs |> Array.forall (fun x -> x < 0)
    ) && pairs |> Array.forall (fun x -> abs x > 0 && abs x < 4)

  [<Theory>]
  [<FileData(2024, 2, 356)>]
  [<MemberData(nameof sample, 2)>]
  let part1 (input: string array) expected =
    parse input
    |> Array.countTrue isSafe
    =! expected

  let dampen array = seq {
    let l = Array.length array
    let result = Array.zeroCreate (l - 1)
    Array.Copy(array, 1, result, 0, l - 1)
    yield result
    let mutable x = array[0]
    for i = 0 to l - 2 do
      let temp = x
      x <- result[i]
      result[i] <- temp
      yield result
  }

  [<Theory>]
  [<FileData(2024, 2, 413)>]
  [<MemberData(nameof sample, 4)>]
  let part2 (input: string array) expected =
    parse input
    |> Array.countTrue(fun x -> isSafe x || dampen x |> Seq.exists isSafe)
    =! expected

module Day1 =

  let input = [|
    "3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map (fun x ->
      match x.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
      | [| a; b |] -> int a, int b
      | _ -> failwith "Invalid input"
    )
    |> Array.unzip

  [<Theory>]
  [<FileData(2024, 1, 2756096)>]
  [<MemberData(nameof sample, 11)>]
  let part1 (input: string array) expected =
    let left, right = parse input
    Array.sortInPlace left
    Array.sortInPlace right
    (left, right)
    ||> Array.sumBy2(fun a b -> abs (a - b))
    =! expected

  [<Theory>]
  [<FileData(2024, 1, 23117829)>]
  [<MemberData(nameof sample, 31)>]
  let part2 (input: string array) expected =
    let left, right = parse input
    let map = right |> Array.countBy id |> Map.ofArray
    left
    |> Array.sumBy(fun x -> x * (map.TryGetValue(x) |> snd) )
    =! expected

#if INTERACTIVE

let makeTemplate day =

  let sample = paste().Trim().Replace("\r\n", "\"\r\n    \"")

  $"""module Day%i{day} =

  let input = [|
    "{sample}"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

"""

makeTemplate 3 |> clip

#endif
