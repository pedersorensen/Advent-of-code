open System
open System.IO

let mutable Year = 0

let readInput (day: string) = File.ReadAllLines($"input{Year}/{day}.txt")
let readsInts day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int
let readsInt64s day = (readInput day |> Array.exactlyOne).Split(',') |> Array.map int64

/// Greatest Common Divisor
let rec gcd a b = if b = 0 then a else gcd b (a % b)
/// Greatest Common Divisor
let rec gcd64 a b = if b = 0L then a else gcd64 b (a % b)
/// Lowest Common Multiple
let lcm64 a b = a * b / gcd64 a b

module Seq =
  let print (s : seq<_>) = s |> Seq.iter(printfn "%A")
  let printS (s : seq<_>) = s |> Seq.iter(printfn "%s")

module Tuple =
  let max tuple1 tuple2 =
    let (x1, y1) = tuple1
    let (x2, y2) = tuple2
    max x1 x2, max y1 y2

  let min tuple1 tuple2 =
    let (x1, y1) = tuple1
    let (x2, y2) = tuple2
    min x1 x2, min y1 y2
