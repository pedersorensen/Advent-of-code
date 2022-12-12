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
open System.Collections.Generic
open System.Text.RegularExpressions

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
  [<FileData(2022, 1, 67_450)>]
  [<MemberData(nameof sample, 24_000)>]
  let part1 (input: string []) expected =
    input
    |> Array.chunkBy(String.IsNullOrWhiteSpace)
    |> Array.map(Array.sumBy int)
    |> Array.max
    =! expected

  [<Theory>]
  [<FileData(2022, 1, 199357)>]
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
  [<FileData(2022, 2, 10404)>]
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
  [<FileData(2022, 2, 10334)>]
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

module Day03 =
  open System.Collections.Generic

  let sample (result: int) = makeSample result [|
    "vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  |]

  let getPriority (ch: Char) =
    let a = int 'a'
    let z = int 'z'
    let A = int 'A'
    let Z = int 'Z'
    let i = int ch
    if a <= i && i <= z then i - a + 1
    elif A <= i && i <= Z then i - A + 27
    else raise(ArgumentOutOfRangeException(nameof ch, ch, "Invalid value."))

  [<Theory>]
  [<FileData(2022, 3, 7990)>]
  [<MemberData(nameof sample, 157)>]
  let part1 (input: string []) expected =
    input
    |> Seq.collect(fun s ->
      let midpoint = s.Length / 2
      let set = HashSet(Seq.take midpoint s)
      set.IntersectWith(Seq.skip midpoint s)
      set
    )
    |> Seq.sumBy getPriority
    =! expected

  [<Theory>]
  [<FileData(2022, 3, 2602)>]
  [<MemberData(nameof sample, 70)>]
  let part2 (input: string []) expected =
    input
    |> Array.chunkBySize 3
    |> Seq.collect (fun chunk ->
      let set = HashSet(chunk[0])
      set.IntersectWith(chunk[1])
      set.IntersectWith(chunk[2])
      set
    )
    |> Seq.sumBy getPriority
    =! expected

module Day04 =

  let sample (result: int) = makeSample result [|
    "2-4,6-8"
    "2-3,4-5"
    "5-7,7-9"
    "2-8,3-7"
    "6-6,4-6"
    "2-6,4-8"
  |]

  [<Theory>]
  [<FileData(2022, 4, 573)>]
  [<MemberData(nameof sample, 2)>]
  let part1 (input: string []) expected =
    input
    |> Array.countTrue(fun s ->
      match s.Split([|',' ; '-'|]) with
      | [| a ; b ; c ; d |] ->
        let a, b, c, d = int a, int b, int c, int d
        (a >=c && b <= d) || (a <=c && b >= d)
      | _ -> failwith "Bad format"
    )
    =! expected

  [<Theory>]
  [<FileData(2022, 4, 867)>]
  [<MemberData(nameof sample, 4)>]
  let part2 (input: string []) expected =
    input
    |> Array.countTrue(fun s ->
      match s.Split([|',' ; '-'|]) with
      | [|a;b;c;d|] ->
        let a, b, c, d = int a, int b, int c, int d
        (b >= c && a < d) || (d >= a && c < b)
      | _ -> failwith "Bad format"
    )
    =! expected

 module Day05 =

  let sample (result: int) = makeSample result [|
    "    [D]    "
    "[N] [C]    "
    "[Z] [M] [P]"
    "1   2   3 "
    ""
    "move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"
  |]

  let getStacks sep input =
    input
    |> Seq.take (sep - 1)
    |> Seq.collect(fun line ->
      Regex.Matches(line, "\w")
      |> Seq.map(fun m -> m.Index, m.Value)
      |> Seq.toArray
    )
    |> Seq.groupBy(fst)
    |> Seq.sortBy fst
    |> Seq.map(fun (_, grp) -> grp |> Seq.map snd |> Seq.rev |> Stack)
    |> Seq.toArray

  let getInstructions sep input =
    input
    |> Array.skip(sep + 1)
    |> Array.map(fun line ->
      let m = Regex.Matches(line, "\d+")
      int m[0].Value, int m[1].Value, int m[2].Value
    )

  let move (stacks: Stack<_> []) (count, source, dest) =
    let source = stacks[source - 1]
    let dest = stacks[dest - 1]
    for _ = 0 to count - 1 do
      dest.Push(source.Pop())

  [<Theory>]
  [<FileData(2022, 05, "VPCDMSLWJ")>]
  [<MemberData(nameof sample, "CMZ")>]
  let part1 (input: string []) (expected: int) =
    let sep = Array.IndexOf(input, "")
    let stacks = getStacks sep input
    getInstructions sep input |> Array.iter(move stacks)
    String.Join("", stacks |> Array.map Seq.head) =! expected.ToString()

  let move2 (stacks: Stack<_> []) (count, source, dest) =
    let source = stacks[source - 1]
    let dest = stacks[dest - 1]
    let temp = Stack()
    for _ = 0 to count - 1 do
      temp.Push(source.Pop())
    let mutable item = Unchecked.defaultof<_>
    while temp.TryPop(&item) do
      dest.Push(item)

  [<Theory>]
  [<FileData(2022, 05, "TPWCGNCCG")>]
  [<MemberData(nameof sample, "MCD")>]
  let part2 (input: string []) expected =
    let sep = Array.IndexOf(input, "")
    let stacks = getStacks sep input
    getInstructions sep input |> Array.iter(move2 stacks)
    String.Join("", stacks |> Array.map Seq.head) =! expected.ToString()

#if INTERACTIVE

let makeTemplate day =

  sprintf """ module Day%02i =

  let sample (result: int) = makeSample result [| |]

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
