open System
open System.IO
open System.Text
open System.Collections.Generic

//https://adventofcode.com/2018

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput file = File.ReadAllLines("input/" + file)

module Seq =
  let print (s : seq<_>) =
    s |> Seq.iter(printfn "%A")

module Day1 =
  let changes = readInput "day1.txt" |> Array.map int

  let repeatedValues seed (changes : int []) =
    let frequencies = new HashSet<_>()
    ignore <| frequencies.Add 0

    let rec loop current idx =
      let next = current + changes.[idx]
      if frequencies.Add next
      then loop next ((idx + 1) % changes.Length)
      else next

    loop seed 0

  repeatedValues 0 [|+1; -1|]// first reaches 0 twice.
  repeatedValues 0 [|+3; +3; +4; -2; -4|]// first reaches 10 twice.
  repeatedValues 0 [|-6; +3; +8; +5; -6|]// first reaches 5 twice.
  repeatedValues 0 [|+7; +7; -2; -7; -4|]// first reaches 14 twice.
  repeatedValues 0 changes

module Day2 =
  let categorize (s:string) =
    let counts = s.ToCharArray() |> Array.countBy id
    let hasTwo = if counts |> Array.exists(snd >> (=) 2) then 1 else 0
    let hasThree = if counts |> Array.exists(snd >> (=) 3) then 1 else 0
    hasTwo, hasThree

  let inline (++) (a, b) (x, y) = (a + x, b + y)

  let checkSum input =
    input
    |> Array.map categorize
    |> Array.fold (++) (0,0)
    |> fun (a, b) -> a * b

  let testInput = [|
    "abcdef" // contains no letters that appear exactly two or three times.
    "bababc" // contains two a and three b, so it counts for both.
    "abbcde" // contains two b, but no letter appears exactly three times.
    "abcccd" // contains three c, but no letter appears exactly two times.
    "aabcdd" // contains two a and two d, but it only counts once.
    "abcdee" // contains two e.
    "ababab" // contains three a and three b, but it only counts once.
  |]

  let input = readInput "day2.txt"

  checkSum testInput
  checkSum input

  let diff (s:string) (t:string) =
    (s.ToCharArray(), t.ToCharArray())
    ||> Array.map2(fun c1 c2 -> int c2 - int c1)
    |> Array.sumBy(fun i -> if i = 0 then 0 else 1)

  let testInput2 = [|
    "abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"
  |]

  (input, input)
  ||> Array.allPairs
  |> Array.filter(fun (a, b) -> a <> b)
  |> Array.distinctBy(fun (a, b) -> if String.CompareOrdinal(a, b) > 0 then (a, b) else (b, a))
  |> Array.map(fun (a, b) -> diff a b, a, b)
  |> Array.sortBy(fun(a, _, _) -> a)
  |> Array.item 0

  "fzvstwblgqkhpuixdrnevmaycd" = "fivstwblgqkhpuixdrnevmaycd"

module Day3 =

  type Claim =
    { Left : int
      Top : int
      Width : int
      Height : int}
    static member Parse(s : string) =
      let split = s.Split("# ,@:x".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
      { Left = int split.[1]
        Top = int split.[2]
        Width = int split.[3]
        Height = int split.[4] }

  let fill (fabric : _[,]) (claim : Claim) =
    for y = claim.Top to claim.Top + claim.Height - 1 do
      for x = claim.Left to claim.Left + claim.Width - 1 do
        fabric.[y, x] <- '#'
    fabric

  let fillCount (fabric : _[,]) (claim : Claim) =
    for y = claim.Top to claim.Top + claim.Height - 1 do
      for x = claim.Left to claim.Left + claim.Width - 1 do
        fabric.[y, x] <- fabric.[y, x] + 1
    fabric

  let print<'a> (fabric : 'a[,]) =
    let height = fabric.GetLength(0)
    let width = fabric.GetLength(1)
    let sb = new StringBuilder((width + 2) * height + 2)
    let _ = sb.AppendLine()
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        Printf.bprintf sb "%O" fabric.[y, x]
      Printf.bprintf sb "\r\n"
    sb.ToString()

  fsi.AddPrinter print<int>
  fsi.AddPrinter print<char>

  let input = readInput "day3.txt" |> Array.map Claim.Parse

  let testInput =
    [|
      "#1 @ 1,3: 4x4"
      "#2 @ 3,1: 4x4"
      "#3 @ 5,5: 2x2"
    |]
    |> Array.map Claim.Parse
  (Array2D.create 8 8 '.', testInput) ||> Array.fold fill
  (Array2D.create 8 8 0, testInput) ||> Array.fold fillCount

  //(Array2D.create 1000 1000 '.', input) ||> Array.fold fill
  let counted = (Array2D.create 1000 1000 0, input) ||> Array.fold fillCount

  let count (fabric : int[,])=
    let mutable i = 0
    let height = fabric.GetLength(0)
    let width = fabric.GetLength(1)
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if fabric.[y, x] > 1 then i <- i + 1
    i
  count counted
