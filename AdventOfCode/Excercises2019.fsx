open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readInput file = File.ReadAllLines("input2019/" + file + ".txt")

module Day1 =
  // Part 1
  let getFuel mass = max 0 (mass / 3 - 2)
  getFuel 12
  getFuel 14
  getFuel 1969
  getFuel 100756

  let totalFuel =
    readInput "day1"
    |> Array.sumBy(int >> getFuel)

  // Part 2
  let rec getFuel' mass =
    let fuel = getFuel mass
    if fuel > 0 then
      fuel + getFuel' fuel
    else fuel

  getFuel' 12
  getFuel' 14
  getFuel' 1969
  getFuel' 100756

  let totalFuel' =
    readInput "day1"
    |> Array.sumBy(int >> getFuel')

module Day2 =
  module OpCodes =
    let [<Literal>] Add = 1
    let [<Literal>] Mult = 2
    let [<Literal>] Stop = 99

  let run (program : int []) =
    let program = program |> Array.copy
    let rec inner i =
      let code = program.[i]
      if code = OpCodes.Stop then program else
      let pos1 = program.[i+1]
      let pos2 = program.[i+2]
      let pos3 = program.[i+3]
      program.[pos3] <- 
        if   code = OpCodes.Add  then program.[pos1] + program.[pos2]
        elif code = OpCodes.Mult then program.[pos1] * program.[pos2]
        else failwith ""
      inner (i+4)
    inner 0

  let example = "1,9,10,3,2,3,11,0,99,30,40,50".Split(',') |> Array.map int
  run example

  run [|1;0;0;0;99|] //becomes 2,0,0,0,99 (1 + 1 = 2).
  run [|2;3;0;3;99|] //becomes 2,3,0,6,99 (3 * 2 = 6).
  run [|2;4;4;5;99;0|] //becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
  run [|1;1;1;4;99;5;6;0;99|] //becomes 30,1,1,4,2,5,6,0,99.

  // Part 1
  let input = (readInput "day2" |> Array.exactlyOne).Split(',') |> Array.map int
  input.[1] <- 12
  input.[2] <- 2
  run input

  // Part 2
  for noun in { 0 .. 99 } do
    for verb in {0 .. 99 } do
      input.[1] <- noun
      input.[2] <- verb
      let res = run input
      if res.[0] = 19690720 then
        printfn "Noun: %i, verb: %i, total: %i" noun verb (100 * noun + verb)

