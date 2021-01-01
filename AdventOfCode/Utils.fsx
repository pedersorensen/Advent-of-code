#r "nuget: FSharp.Data"

open System
open System.IO
open System.Collections.Generic
open FSharp.Data

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let mutable Year = 0

let ensureExists (day: int) =
  if Year = 0 then invalidOp "Year has not been set."
  let path = $"input{Year}/day{day}.txt"
  if File.Exists path |> not then
    let cookies = [| "session", File.ReadAllText("cookie.txt") |]
    Directory.CreateDirectory($"input{Year}") |> ignore
    printfn $"Input for day {day} does not exists, downloading file {path}."
    let url = $"https://adventofcode.com/{Year}/day/{day}/input"
    let response = Http.RequestString(url, cookies = cookies)
    File.WriteAllText(path, response)
  path

let readInput (day: int) = File.ReadAllLines(ensureExists day)
let readAllInput (day: int) = File.ReadAllText(ensureExists day)

let readsInts day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int
let readsInt64s day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int64

/// Greatest Common Divisor
let rec gcd a b = if b = 0 then a else gcd b (a % b)
/// Greatest Common Divisor
let rec gcd64 a b = if b = 0L then a else gcd64 b (a % b)
/// Lowest Common Multiple
let lcm64 a b = a * b / gcd64 a b

let cons head tail = head :: tail

module Seq =

  let print (s : seq<_>) = s |> Seq.iter(printfn "%A")
  let printS (s : seq<_>) = s |> Seq.iter(printfn "%s")

  let countTrue predicate source =
    source |> Seq.sumBy(fun x -> if predicate x then 1 else 0)

  let batchOnNewline combiner accumulator init array =
    (init, array)
    ||> Seq.fold(fun (state, acc) line ->
      if String.IsNullOrWhiteSpace line
      then fst init, accumulator state acc
      else combiner line state, acc)
    |> fun (set, count) -> accumulator set count

module Array =

  let countTrue predicate source =
    source |> Array.sumBy(fun x -> if predicate x then 1 else 0)

module Tuple =

  let inline add (x1, y1) (x2, y2) = x1 + x2, y1 + y2

  let max (x1, y1) (x2, y2) = max x1 x2, max y1 y2

  let min (x1, y1) (x2, y2) = min x1 x2, min y1 y2

module String =

  let splitAt (char: char) (s: string) = s.Split(char)

  let cutAt (char: char) (s: string) =
    let i = s.IndexOf(char)
    s.Substring(0, i), s.Substring(i + 1)

module Map =
  let addTuple (key, value) map =
    Map.add key value map

type IDictionary<'TKey, 'TValue> with
  member this.TryGet(key) =
    match this.TryGetValue(key) with
    | true, value -> Some value
    | _ -> None

