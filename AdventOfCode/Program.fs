module Program

open Excercises2024
open Xunit.Sdk
open System.IO

[<EntryPoint>]
let main args =
  let year = 2024
  let day  = 22

  let input = File.ReadAllLines(ensureExists year day)
  try
    Day22.part2 input 1696
  with
  | :? XunitException as e -> printfn "%s" e.Message
  0

