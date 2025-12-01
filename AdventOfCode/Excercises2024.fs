#if INTERACTIVE
#r "nuget: System.Drawing.Common"
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2024
#endif

open Xunit
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Runtime.InteropServices

#if INTERACTIVE
makeTemplate 2024 25 |> clip
#endif

module Day25 =

  let input = [|
    "#####"
    ".####"
    ".####"
    ".####"
    ".#.#."
    ".#..."
    "....."
    ""
    "#####"
    "##.##"
    ".#.##"
    "...##"
    "...#."
    "...#."
    "....."
    ""
    "....."
    "#...."
    "#...."
    "#...#"
    "#.#.#"
    "#.###"
    "#####"
    ""
    "....."
    "....."
    "#.#.."
    "###.."
    "###.#"
    "###.#"
    "#####"
    ""
    "....."
    "....."
    "....."
    "#...."
    "#.#.."
    "#.#.#"
    "#####"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 25, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, 25, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day24 =

  let input = [|
    "x00: 1"
    "x01: 1"
    "x02: 1"
    "y00: 0"
    "y01: 1"
    "y02: 0"
    ""
    "x00 AND y00 -> z00"
    "x01 XOR y01 -> z01"
    "x02 OR y02 -> z02"
  |]

  let input2 = [|
    "x00: 1"
    "x01: 0"
    "x02: 1"
    "x03: 1"
    "x04: 0"
    "y00: 1"
    "y01: 1"
    "y02: 1"
    "y03: 1"
    "y04: 1"
    ""
    "ntg XOR fgs -> mjb"
    "y02 OR x01 -> tnw"
    "kwq OR kpj -> z05"
    "x00 OR x03 -> fst"
    "tgd XOR rvg -> z01"
    "vdt OR tnw -> bfw"
    "bfw AND frj -> z10"
    "ffh OR nrd -> bqk"
    "y00 AND y03 -> djm"
    "y03 OR y00 -> psh"
    "bqk OR frj -> z08"
    "tnw OR fst -> frj"
    "gnj AND tgd -> z11"
    "bfw XOR mjb -> z00"
    "x03 OR x00 -> vdt"
    "gnj AND wpb -> z02"
    "x04 AND y00 -> kjc"
    "djm OR pbm -> qhw"
    "nrd AND vdt -> hwm"
    "kjc AND fst -> rvg"
    "y04 OR y02 -> fgs"
    "y01 AND x02 -> pbm"
    "ntg OR kjc -> kwq"
    "psh XOR fgs -> tgd"
    "qhw XOR tgd -> z09"
    "pbm OR djm -> kpj"
    "x03 XOR y03 -> ffh"
    "x00 XOR y04 -> ntg"
    "bfw OR bqk -> z06"
    "nrd XOR fgs -> wpb"
    "frj XOR qhw -> z04"
    "bqk OR frj -> z07"
    "y03 OR x01 -> nrd"
    "hwm AND bqk -> z03"
    "tgd XOR rvg -> z12"
    "tnw OR pbm -> gnj"
  |]

  type Operation =
  | And
  | Or
  | Xor

  let parse (input: string array) =
    let initialValues, connections = Dictionary(), SortedDictionary()
    for line in input do
      match line.Split(':', StringSplitOptions.TrimEntries) with
      | [| |]
      | [| "" |] -> ()
      | [| name ; value |] -> initialValues.Add(name, int value)
      | _ ->
        match line.Split(' ', StringSplitOptions.TrimEntries) with
        | [| a ; op ; b ; "->" ; c |] ->
          let op =
            match op with
            | "AND" -> (&&&)
            | "OR"  -> (|||)
            | "XOR" -> (^^^)
            | _ -> failwith "Invalid operation"
          connections.Add(c, (a, op, b))
        | l -> failwithf "Invalid input: %A" l
    initialValues, connections

  let sample (result: int) = makeSample result input
  let sample2 (result: int) = makeSample result input2

  [<Theory>]
  [<FileData(2024, 24, 57270694330992L)>]
  [<MemberData(nameof sample, 4)>]
  [<MemberData(nameof sample2, 2024)>]
  let part1 (input: string array) expected =
    let values, connections = parse input
    let rec getValue name =
      match values.TryGetValue(name) with
      | true, value -> value
      | _ ->
        let (a, (++), b) = connections[name]
        let value = getValue a ++ getValue b
        values.Add(name, value)
        value
    ((0L, 1L), connections)
    ||> Seq.fold(fun (sum, powerOfTwo) kvp ->
      if kvp.Key.StartsWith("z")
      then sum + powerOfTwo * int64(getValue kvp.Key), 2L * powerOfTwo
      else sum, powerOfTwo
    )
    |> fst
    =! expected

  [<Theory>]
  [<FileData(2024, 24, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day23 =

  let input = [|
    "kh-tc"
    "qp-kh"
    "de-cg"
    "ka-co"
    "yn-aq"
    "qp-ub"
    "cg-tb"
    "vc-aq"
    "tb-ka"
    "wh-tc"
    "yn-cg"
    "kh-ub"
    "ta-co"
    "de-co"
    "tc-td"
    "tb-wq"
    "wh-td"
    "ta-ka"
    "td-qp"
    "aq-cg"
    "wq-ub"
    "ub-vc"
    "de-ta"
    "wq-aq"
    "wq-vc"
    "wh-yn"
    "ka-de"
    "kh-ta"
    "co-tc"
    "wh-qp"
    "tb-vc"
    "td-yn"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.map(fun line ->
      match line.Split('-') with
      | [| a; b |] -> a, b
      | _ -> failwith "Invalid input"
    )

  [<Theory>]
  [<FileData(2024, 23, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    let connections = parse input
    let aToB =
      connections
      |> Array.groupBy(fst)
      |> Array.map(fun (k, v) -> k, v |> Array.map snd)
      |> Map.ofArray
    let bToA =
      connections
      |> Array.groupBy(snd)
      |> Array.map(fun (k, v) -> k, v |> Array.map fst)
      |> Map.ofArray

    -1 =! expected

  [<Theory>]
  [<FileData(2024, 23, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day22 =

  let input = [|
    "1"
    "10"
    "100"
    "2024"
  |]

  let sample (result: int) = makeSample result input

  let mixAndPrune secret value = (value ^^^ secret) % 16777216L

  let evolve secret =
    let secret = secret * 64L   |> mixAndPrune secret
    let secret = secret / 32L   |> mixAndPrune secret
    let secret = secret * 2048L |> mixAndPrune secret
    secret

  let rec evolveN n secret =
    if n = 0
    then secret
    else evolveN (n - 1) (evolve secret)

  [<Theory>]
  [<FileData(2024, 22, 15335183969L)>]
  [<MemberData(nameof sample, 37327623)>]
  let part1 (input: string array) expected =
    input
    |> Array.sumBy(int64 >> evolveN 2000)
    =! expected

  let input2 = [|
    "1"
    "2"
    "3"
    "2024"
  |]

  let sample2 (result: int) = makeSample result input2

  [<Theory>]
  [<FileData(2024, 22, 1696)>]
  [<MemberData(nameof sample2, 23)>]
  let part2 (input: string array) expected =
    let total = Dictionary()
    let mutable result = 0L
    let mutable exists = false

    let counts = Dictionary()
    for secret in input do
      counts.Clear()
      let rec loop i s1 s2 s3 d3 secret3 =
        if i > 0 then
          let secret4 = evolve secret3
          let d4 = secret4 % 10L
          let s4 = d4 - d3
          let window = struct(s1, s2, s3, s4)
          counts.TryAdd(window, d4) |> ignore
          loop (i - 1) s2 s3 s4 d4 secret4
      let secret0 = int64 secret
      let secret1 = evolve secret0
      let secret2 = evolve secret1
      let secret3 = evolve secret2
      let d0 = secret0 % 10L
      let d1 = secret1 % 10L
      let d2 = secret2 % 10L
      let d3 = secret3 % 10L
      let s1 = d1 - d0
      let s2 = d2 - d1
      let s3 = d3 - d2
      loop (2000 - 3) s1 s2 s3 d3 secret3

      for kvp in counts do
        let mutable valueRef = &CollectionsMarshal.GetValueRefOrAddDefault(total, kvp.Key, &exists)
        let value = valueRef + kvp.Value
        valueRef <- value
        result <- max result value

    result =! expected

module Day21 =

  let input = [|
    "+---+---+---+"
    "| 7 | 8 | 9 |"
    "+---+---+---+"
    "| 4 | 5 | 6 |"
    "+---+---+---+"
    "| 1 | 2 | 3 |"
    "+---+---+---+"
    "    | 0 | A |"
    "    +---+---+"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 21, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, 21, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day20 =

  let input = [|
    "###############"
    "#...#...#.....#"
    "#.#.#.#.#.###.#"
    "#S#...#.#.#...#"
    "#######.#.#.###"
    "#######.#.#...#"
    "#######.#.###.#"
    "###..E#...#...#"
    "###.#######.###"
    "#...###...#...#"
    "#.#####.#.###.#"
    "#.#...#.#.#...#"
    "#.#.#.#.#.#.###"
    "#...#...#...###"
    "###############"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 20, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, 20, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day19 =

  let input = [|
    "r, wr, b, g, bwu, rb, gb, br"
    ""
    "brwrr"
    "bggr"
    "gbbr"
    "rrbgbr"
    "ubwu"
    "bwurrg"
    "brgr"
    "bbrgwb"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 19, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2024, 19, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day18 =

  let input = [|
    "5,4"
    "4,2"
    "4,5"
    "3,0"
    "2,1"
    "6,3"
    "2,4"
    "1,5"
    "0,6"
    "3,3"
    "2,6"
    "5,1"
    "1,2"
    "5,5"
    "2,5"
    "6,5"
    "1,4"
    "0,4"
    "6,4"
    "1,1"
    "6,1"
    "1,0"
    "0,5"
    "1,6"
    "2,0"
  |]

  let sample  (result: int) = makeSample result input
  let sampleS (result: string) = makeSample result input

  let directions = [| Point(0, 1); Point(1, 0); Point(0, -1); Point(-1, 0) |]

  let bfs (grid: char[,]) (start: Point) (end': Point) =
    let p0, pMax = Point(0, 0), Point(grid.GetLength(0), grid.GetLength(1))

    let isValid (point: Point) =
      p0.CompareTo(point) <= 0 && point.CompareTo(pMax) < 0 && grid[point.X, point.Y] <> '#'

    let queue   = Queue<Point * int>()
    let visited = HashSet<Point>([|start|])
    queue.Enqueue((start, 0))
    let mutable ret = None
    let mutable pair = Unchecked.defaultof<_>

    while queue.TryDequeue(&pair) && ret.IsNone do
      let (current, distance) = pair
      if current.Equals(end') then
        ret <- Some distance

      for dx in directions do
        let next = current + dx
        if isValid next && visited.Add(next) then
          queue.Enqueue((next, distance + 1))
    ret

  let parse input =
    input
    |> Array.map(fun line ->
      match parseNumbers<int> line with
      | [| x; y |] -> x, y
      | _ -> failwith "Invalid input"
    )

  [<Theory>]
  [<FileData(2024, 18, 260)>]
  [<MemberData(nameof sample, 22)>]
  let part1 (input: string array) expected =
    let positions = parse input
    let size, take = if expected = 22 then 6, 12 else 70, 1024
    let grid = Array2D.init (size+1) (size+1) (fun _ _ -> '.')
    positions |> Array.take take |> Array.iter(fun (y, x) -> grid[x, y] <- '#')

    let start, end' = Point(0, 0), Point(size, size)
    bfs grid start end' =! Some expected

  [<Theory>]
  [<FileData(2024, 18, "(24, 48)")>]
  [<MemberData(nameof sampleS, "(6, 1)")>]
  let part2 (input: string array) expected =
    let positions = parse input
    let size, take = if expected = "(6, 1)" then 6, 12 else 70, 1024
    let grid = Array2D.init (size+1) (size+1) (fun _ _ -> '.')

    let start, end' = Point(0, 0), Point(size, size)
    let mutable skip = take

    positions
    |> Array.find(fun (y, x) ->
      grid[x, y] <- '#'
      if skip > 0 then
        skip <- skip - 1
        false
      else
        bfs grid start end' |> Option.isNone
    )
    |> string
    =! expected

module Day17 =
  open System.Threading

  let input = [|
    "Register A: 729"
    "Register B: 0"
    "Register C: 0"
    ""
    "Program: 0,1,5,4,3,0"
  |]

  let sample (result: string) = makeSample result input

  let parse (input: string array) =
      let parsed = input |> Array.map parseNumbers<int64>
      Array.exactlyOne parsed[0]
    , Array.exactlyOne parsed[1]
    , Array.exactlyOne parsed[2]
    , parsed[4]

  let run a0 b0 c0 (program: int64 array) stopCondition =

    let combo a b c ip =
      match program[ip + 1] with
      | 0L
      | 1L
      | 2L
      | 3L as l -> l
      | 4L -> a
      | 5L -> b
      | 6L -> c
      | 7L -> failwith "reserved"
      | _ -> failwith "Invalid instruction"

    let div a b c ip =
      let denom = Math.Pow(2., combo a b c ip |> float)
      a / int64 denom

    let rec loop ip out test a b c =
      match stopCondition ip out test with
      | Some result -> result
      | None ->
        match program[ip] with
        | 0L -> loop (ip + 2) out false (div a b c ip) b c
        | 6L -> loop (ip + 2) out false a (div a b c ip) c
        | 7L -> loop (ip + 2) out false a b (div a b c ip)
        | 1L -> loop (ip + 2) out false a (b ^^^ program[ip + 1]) c
        | 2L -> loop (ip + 2) out false a (combo a b c ip % 8L) c
        | 3L ->
          let ip = if a = 0 then ip + 2 else program[ip + 1] |> int
          //let ip =
          //  if a = 0 then ip + 2
          //  else
          //    let ip' = program[ip + 1]
          //    if ip = ip' then ip + 2 else ip'
          loop ip out false a b c
        | 4L -> loop (ip + 2) out false a (b ^^^ c) c
        | 5L -> loop (ip + 2) (combo a b c ip % 8L :: out) true a b c
        | _ -> failwith "Invalid instruction"
    loop 0 [] false a0 b0 c0

  let run2 a0 b0 c0 (program : int64 array) stopCondition =
    let combo a b c ip =
      match program[ip + 1] with
      | 0L
      | 1L
      | 2L
      | 3L as l -> l
      | 4L -> a
      | 5L -> b
      | 6L -> c
      | 7L -> failwith "reserved"
      | _ -> failwith "Invalid instruction"

    let div a b c ip =
      let denom = Math.Pow(2., combo a b c ip |> float)
      a / int64 denom

    let mutable out = []
    let mutable result = None
    let mutable test = false
    let mutable a, b, c, ip = a0, b0, c0, 0

    while result.IsNone do
      test <- false
      match program[ip] with
      | 0L ->
        a <- div a b c ip
        ip <- ip + 2
      | 6L ->
        b <- div a b c ip
        ip <- ip + 2
      | 7L ->
        c <- div a b c ip
        ip <- ip + 2
      | 1L ->
        b <- (b ^^^ program[ip + 1])
        ip <- ip + 2
      | 2L ->
        b <- combo a b c ip % 8L
        ip <- ip + 2
      | 3L ->
        ip <- if a = 0 then ip + 2 else program[ip + 1] |> int
      | 4L ->
        b <- b ^^^ c
        ip <- ip + 2
      | 5L ->
        out <- combo a b c ip % 8L :: out
        test <- true
        ip <- ip + 2
      | _ -> failwith "Invalid instruction"
      result <- stopCondition ip out test
    result.Value

  let run3 a0 b0 c0 stopCondition =
    let mutable o = []
    let mutable r = None
    let mutable a, b, c = a0, b0, c0
    while r.IsNone do
      (*
      2,4 b <- a % 8
      1,1 b <- b ^^^ 1
      7,5 c <- a / 2 ^ b
      1,5 b <- b ^^^ 5
      4,0 b <- b ^^^ c
      0,3 a <- a / 2 ^ 3
      5,5 out <- b % 8
      3,0 jump to 0 if a > 0
      *)
      b <- a &&& 7L
      b <- b ^^^ 1L
      c <- a >>> int b
      b <- b ^^^ 5L
      b <- b ^^^ c
      a <- a >>> 3
      o <- (b &&& 7L) :: o
      r <- stopCondition a o
    r |> Option.defaultValue Unchecked.defaultof<_>

  [<Theory>]
  [<FileData(2024, 17, "6,4,6,0,4,5,7,2,7")>]
  [<MemberData(nameof sample, "4,6,3,5,6,3,5,2,1,0")>]
  let part1A (input: string array) expected =
    let a, b, c, program = parse input
    run a b c program (fun ip out _ ->
      if ip >= program.Length - 1
      then Some(String.Join(',', List.rev out))
      else None
    )
    =! expected

  [<Theory>]
  [<FileData(2024, 17, "6,4,6,0,4,5,7,2,7")>]
  //[<MemberData(nameof sample, "4,6,3,5,6,3,5,2,1,0")>]
  let part1B (input: string array) expected =
    let a, b, c, program = parse input
    run2 a b c program (fun ip out _ ->
      if ip > program.Length - 1
      then Some(String.Join(',', List.rev out))
      else None
    )
    =! expected

  [<Theory>]
  [<FileData(2024, 17, "6,4,6,0,4,5,7,2,7")>]
  //[<MemberData(nameof sample, "4,6,3,5,6,3,5,2,1,0")>]
  let part1C (input: string array) expected =
    let a, b, c, program = parse input
    run3 a b c (fun a out ->
      if a <= 0L
      then Some (String.Join(',', List.rev out))
      else None
    )
    =! expected

  let input2 = [|
    "Register A: 729"
    "Register B: 0"
    "Register C: 0"
    ""
    "Program: 0,3,5,4,3,0"
  |]

  let sample2 (result: int64) = makeSample result input2

  [<Theory>]
  [<FileData(2024, 17, 0L)>]
  //[<MemberData(nameof sample2, 117440L)>]
  let part2B (input: string array) expected =
    let input = [|
      "Register A: 64196994"
      "Register B: 0"
      "Register C: 0"
      ""
      "Program: 2,4,1,1,7,5,1,5,4,0,0,3,5,5,3,0"
    |]

    let _a, b, c, program = parse input
    let cts = new CancellationTokenSource(300)
    Seq.initInfinite id
    |> Seq.find(fun a ->
      let mutable i = 0
      if a % 1000000 = 0 then
        printfn  "%i" a
        cts.Token.ThrowIfCancellationRequested()
      run3 a b c (fun a out ->
        //printfn "%i %A" a out
        if a <= 0 then
          Some false
        else
          if List.head out = program[i] then
            i <- i + 1
            if i = program.Length
            then Some true
            else None
          else
            Some false
      )
    )
    =! expected

  [<Theory>]
  //[<FileData(2024, 17, 0L)>]
  [<MemberData(nameof sample2, 117440L)>]
  let part2 (input: string array) expected =
    let _a, b, c, program = parse input
    //let self = program |> List.ofArray
    //let a = 117435
    //Seq.init 10 (fun i -> a + i)
    Seq.initInfinite id
    |> Seq.find(fun a ->
      let mutable i = 0
      run2 a b c program (fun ip out test ->
        if ip >= program.Length then
          Some false
        else
          if test then
            if List.head out = program[i] then
              i <- i + 1
              if i = program.Length
              then Some true
              else None
            else
              Some false
          else None
      )
    )
    =! expected

module Day16 =

  let input1 = [|
    "###############"
    "#.......#....E#"
    "#.#.###.#.###.#"
    "#.....#.#...#.#"
    "#.###.#####.#.#"
    "#.#.#.......#.#"
    "#.#.#####.###.#"
    "#...........#.#"
    "###.#.#####.#.#"
    "#...#.....#.#.#"
    "#.#.#.###.#.#.#"
    "#.....#...#.#.#"
    "#.###.#.#.#.#.#"
    "#S..#.....#...#"
    "###############"
  |]

  let input2 = [|
    "#################"
    "#...#...#...#..E#"
    "#.#.#.#.#.#.#.#.#"
    "#.#.#.#...#...#.#"
    "#.#.#.#.###.#.#.#"
    "#...#.#.#.....#.#"
    "#.#.#.#.#.#####.#"
    "#.#...#.#.#.....#"
    "#.#.#####.#.###.#"
    "#.#.#.......#...#"
    "#.#.###.#####.###"
    "#.#.#...#.....#.#"
    "#.#.#.#####.###.#"
    "#.#.#.........#.#"
    "#.#.#.#########.#"
    "#S#.............#"
    "#################"
  |]

  let sample1 (result: int) = makeSample result input1
  let sample2 (result: int) = makeSample result input2

  let directions = [
    +1, 0
    -1, 0
    0, +1
    0, -1
  ]

  [<Theory>]
  //[<FileData(2024, 16, 0)>]
  [<MemberData(nameof sample1, 7036)>]
  [<MemberData(nameof sample2, 11048)>]
  let part1 (input: string array) expected =
    let input = input1
    //let input = IO.File.ReadAllLines(ensureExists 2024 16)

    let p0 =
      let x = input |> Array.findIndex(fun line -> line.Contains('S'))
      x, input[x].IndexOf('S')

    let w, h = input.Length, input[0].Length

    let rec getPaths (input: string array) list acc set ((x, y) as p) : (int*int) list list =
      if Set.contains p set || x < 0 || x >= w || y < 0 || y >= h || input[x][y] = '#' then
        []
      elif input[x][y] = 'E' then
        printfn "Got path"
        (p :: acc) :: list
      else
        let set = set.Add(p)
        let acc = p :: acc
        directions |> List.collect(fun (dx, dy) -> getPaths input list acc set (x + dx, y + dy))

    let paths = getPaths input [] [] Set.empty p0

    paths
    |> List.map(fun path ->

      let directions =
        ((fst p0, 0) :: path)
        |> List.pairwise
        |> List.map(fun ((x1, y1), (x2, y2)) ->
          match x2 - x1, y2 - y1 with
          | 1, 0 -> 'v'
          | -1, 0 -> '^'
          | 0, 1 -> '>'
          | 0, -1 -> '<'
          | _ -> failwith "Invalid direction"
        )

      let turns = directions |> List.toArray |> Array.chunkWhen (=) |> Array.length
      let steps = path.Length

      //let g = input |> Array.map(fun line -> line.ToCharArray())
      //for ((x, y), d) in List.zip path directions do
      //  g[x][y] <- d
      //let s = g |> Array.map String |> String.concat "\r\n"
      //printfn "%s" s
      //printfn ""
      1000 * (turns - 1) + steps - 1
    )
    |> List.min
    //|> List.sort
    //|> Seq.print
    =! expected

  [<Theory>]
  [<FileData(2024, 16, 0)>]
  [<MemberData(nameof sample1, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day15 =

  let input = [|
    "##########"
    "#..O..O.O#"
    "#......O.#"
    "#.OO..O.O#"
    "#..O@..O.#"
    "#O#..O...#"
    "#O..O..O.#"
    "#.OO.O.OO#"
    "#....O...#"
    "##########"
    ""
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
  |]

  let input2 = [|
    "########"
    "#..O.O.#"
    "##@.O..#"
    "#...O..#"
    "#.#.O..#"
    "#...O..#"
    "#......#"
    "########"
    ""
    "<^^>>>vv<v>>v<<"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    let split =
      input
      |> Array.chunkBy(String.IsNullOrEmpty)
    split[0] |> Array.map(fun s -> s.ToCharArray()), String.Concat split[1]

  let printMap (map: char array array) =
    for line in map do
      for ch in line do
        printf "%c" ch
      printfn ""

  let getStartingPoint (map: char array array) =
    let x = map |> Array.findIndex(fun i -> i |> Array.contains '@')
    let y = Array.IndexOf(map[x], '@')
    x, y

  [<Theory>]
  [<FileData(2024, 15, 1421727)>]
  [<MemberData(nameof sample, 10092)>]
  let part1 (input: string array) expected =

    let tryPush (x, y) direction (map : char array array ) =
      let dx, dy =
        match direction with
        | '^' -> -1, 0
        | 'v' -> +1, 0
        | '<' ->  0, -1
        | '>' ->  0, +1
        | _ -> failwith "Invalid direction"
      if map[x + dx][y + dy] <> 'O' then failwith "Invalid move"
      (x, y)
      |> Seq.unfold(fun (x, y) ->
        let x2, y2 = x + dx, y + dy
        if x2 >= 1 && x2 < map.Length - 1 && y2 >= 1 && y2 < map[0].Length - 1 && map[x2][y2] <> '#' then
          Some((x2, y2), (x2, y2))
        else
          None
      )
      |> Seq.exists(fun (x2, y2) ->
        if map[x2][y2] = '.' then
          map[x][y]           <- '.'
          map[x2][y2]         <- 'O'
          map[x + dx][y + dy] <- '@'
          true
        else
          false
      )

    let map, moves = parse input
    printMap map

    (getStartingPoint map, moves)
    ||> Seq.fold(fun (x, y) move ->
      let x2, y2 =
        match move with
        | '^' -> x - 1, y
        | 'v' -> x + 1, y
        | '<' -> x, y - 1
        | '>' -> x, y + 1
        | _ -> failwith "Invalid move"
      if x2 >= 1 && x2 < map.Length - 1 && y2 >= 1 && y2 < map[0].Length - 1 then
        let ch = map[x2][y2]
        if ch = '#' then
          x, y
        elif ch = '.' then
          map[x][y] <- '.'
          map[x2][y2] <- '@'
          x2, y2
        elif ch = 'O' && tryPush (x, y) move map then
          x2, y2
        else x, y
      else x, y
    )
    |> ignore

    let mutable sum = 0
    for i = 0 to map.Length - 1 do
      let line = map[i]
      for j = 0 to line.Length - 1 do
        if line[j] = 'O' then
          sum <- sum + 100 * i + j

    sum =! expected

  [<Theory>]
  [<FileData(2024, 15, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected

module Day14 =

  let input = [|
    "p=0,4 v=3,-3"
    "p=6,3 v=-1,-3"
    "p=10,3 v=-1,2"
    "p=2,0 v=2,-1"
    "p=0,0 v=1,3"
    "p=3,0 v=-2,-2"
    "p=7,6 v=-1,-3"
    "p=3,0 v=-1,-2"
    "p=9,3 v=2,3"
    "p=7,3 v=-1,2"
    "p=2,4 v=2,-3"
    "p=9,5 v=-3,-3"
  |]

  let sample (result: int) = makeSample result input

  let printPoints w h ps =
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let c = ps |> Array.countTrue((=) (Point(x, y)))
        printf "%s" (if c > 0 then c.ToString() else ".")
      printfn ""

  let parse input max =
    input
    |> Array.map(fun line ->
      let values = parseNumbers<int> line
      let p = Point(values[0], values[1])
      let v = Point(values[2], values[3]) + max
      p, v
    )
    |> Array.unzip

  [<Theory>]
  [<FileData(2024, 14, 216027840)>]
  [<MemberData(nameof sample, 12)>]
  let part1 (input: string array) expected =
    let w, h = if expected = 12 then 7, 11 else 101, 103
    let max = Point(w, h)

    let getQuadrant (p: Point) =
      if p.X < w / 2 && p.Y < h / 2 then 0
      elif p.X >= w / 2 && p.Y < h / 2 then 1
      elif p.X < w / 2 && p.Y >= h / 2 then 2
      else 3

    parse input max
    ||>Array.map2 (fun p v -> (p + 100 * v) % max)
    |> Array.filter(fun p -> p.X <> w / 2 && p.Y <> h / 2)
    |> Array.countBy getQuadrant
    |> Array.fold(fun acc (_, count) -> acc * count) 1
    =! expected

  [<Theory(
    Skip = "Unskip to write images to disk"
  )>]
  [<FileData(2024, 14, 6876)>]
  //[<MemberData(nameof sample, 0)>] // Does not apply to the sample
  let part2 (input: string array) expected =
    let max = Point(101, 103)
    let w_count, h_count, d = 30, 30, 10
    let wd, hd = max.X + d, max.Y + d
    let points, velocities = parse input max
    for i = 0 to 7 do
      use bitmap = new Drawing.Bitmap(w_count * wd, h_count * hd)
      use graphics = Drawing.Graphics.FromImage(bitmap)
      for dy = 0 to w_count - 1 do
        for dx = 0 to h_count - 1 do
          (points, velocities) ||> Array.iteri2(fun i p v -> points[i] <- (p + v) % max)
          let dx, dy = wd * dx, hd * dy
          for p in points do
            graphics.FillRectangle(Drawing.Brushes.White, p.X + dx, p.Y + dy, 1, 1)
      bitmap.Save(@$"C:\temp\day_14_{i}.bmp")
    7 * 900 + 19 * 30 + 6 =! expected

module Day13 =

  let input = [|
    "Button A: X+94, Y+34"
    "Button B: X+22, Y+67"
    "Prize: X=8400, Y=5400"
    ""
    "Button A: X+26, Y+66"
    "Button B: X+67, Y+21"
    "Prize: X=12748, Y=12176"
    ""
    "Button A: X+17, Y+86"
    "Button B: X+84, Y+37"
    "Prize: X=7870, Y=6450"
    ""
    "Button A: X+69, Y+23"
    "Button B: X+27, Y+71"
    "Prize: X=18641, Y=10279"
  |]

  let sample (result: float) = makeSample result input

  let solveLinearEquations (A: float) (B: float) (C: float) (D: float) (X: float) (Y: float) =
    let determinant = A * D - B * C
    if determinant = 0.0 then
      failwith "No unique solution exists"
    else
      let t1 = (X * D - B * Y) / determinant
      let t2 = (Y * A - C * X) / determinant
      (t1, t2)

  let solve offset input =
    input
    |> Array.chunkBy(String.IsNullOrEmpty)
    |> Array.sumBy(fun chunk ->
      let parsed = chunk |> Array.map parseNumbers<float>
      let a, b, p = parsed[0], parsed[1], parsed[2]
      let ax, ay = a[0], a[1]
      let bx, by = b[0], b[1]
      let px, py = p[0] + offset, p[1] + offset
      let (t1, t2) = solveLinearEquations ax bx ay by px py
      if Math.Round(t1) = t1 && Math.Round(t2) = t2 then 3. * t1 + t2 else 0.
    )

  [<Theory>]
  [<FileData(2024, 13, 34787.)>]
  [<MemberData(nameof sample, 480.)>]
  let part1 (input: string array) expected =
    solve 0. input =! expected

  [<Theory>]
  [<FileData(2024, 13, 85644161121698.)>]
  [<MemberData(nameof sample, 875318608908.)>]
  let part2 (input: string array) expected =
    solve 10000000000000. input =! expected

module Day12 =

  let input1 = [|
    "AAAA"
    "BBCD"
    "BBCC"
    "EEEC"
  |]

  let input2 = [|
    "OOOOO"
    "OXOXO"
    "OOOOO"
    "OXOXO"
    "OOOOO"
  |]

  let input3 = [|
    "RRRRIICCFF"
    "RRRRIICCCF"
    "VVRRRCCFFF"
    "VVRCCCJFFF"
    "VVVVCJJCFE"
    "VVIVCCJJEE"
    "VVIIICJJEE"
    "MIIIIIJJEE"
    "MIIISIJEEE"
    "MMMISSJEEE"
  |]

  let circumference (points: HashSet<_>) =
    let oneIfDifferent p = if points.Contains(p) then 0 else 1
    points
    |> Seq.sumBy(fun (i, j) ->
      let above = oneIfDifferent (i - 1, j    )
      let below = oneIfDifferent (i + 1, j    )
      let left  = oneIfDifferent (i    , j - 1)
      let right = oneIfDifferent (i    , j + 1)
      above + below + left + right
    )

  let regions (array: string array) =
    let w, h      = array.Length, array[0].Length
    let p0, pMax  = Point(0, 0), Point(w, h)
    let regions   = Dictionary()
    let regionMap = Array2D.zeroCreate w h
    let mutable regionId = 0
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        if regionMap[i, j] = 0 then
          let set = HashSet()
          regionId <- regionId + 1
          regions.Add(regionId, set)
          let rec fill (i, j) ch =
            let p = Point(i, j)
            if p0 <= p && p < pMax && array[i][j] = ch && set.Add((i, j)) then
              regionMap[i, j] <- regionId
              fill (i - 1, j) ch
              fill (i + 1, j) ch
              fill (i, j - 1) ch
              fill (i, j + 1) ch
          fill (i, j) (array[i][j])
    regions.Values

  let sample1 (result: int) = makeSample result input1
  let sample2 (result: int) = makeSample result input2
  let sample3 (result: int) = makeSample result input3

  [<Theory>]
  [<FileData(2024, 12, 1471452)>]
  [<MemberData(nameof sample1, 140)>]
  [<MemberData(nameof sample2, 772)>]
  [<MemberData(nameof sample3, 1930)>]
  let part1 (input: string array) expected =
    regions input
    |> Seq.sumBy(fun set -> set.Count * circumference set)
    =! expected

  let input4 = [|
    "EEEEE"
    "EXXXX"
    "EEEEE"
    "EXXXX"
    "EEEEE"
  |]

  let input5 = [|
    "AAAAAA"
    "AAABBA"
    "AAABBA"
    "ABBAAA"
    "ABBAAA"
    "AAAAAA"
  |]

  let sample4 (result: int) = makeSample result input4
  let sample5 (result: int) = makeSample result input5

  let countSegments points contains =
    points
    |> Array.filter(contains >> not)
    |> Array.chunkWhen(fun a b -> b = a + 1)
    |> Array.length

  [<Theory>]
  [<FileData(2024, 12, 863366)>]
  [<MemberData(nameof sample1, 80)>]
  [<MemberData(nameof sample2, 436)>]
  [<MemberData(nameof sample4, 236)>]
  [<MemberData(nameof sample5, 368)>]
  let part2 (input: string array) expected =
    regions input
    |> Seq.sumBy(fun points ->
      let values = Seq.toArray points
      let aboveBelow =
        values
        |> Array.groupBy fst
        |> Array.sumBy(fun (i, row) ->
          let js = row |> Array.map snd |> Array.sort
          + countSegments js (fun j -> points.Contains(i + 1, j))
          + countSegments js (fun j -> points.Contains(i - 1, j))
        )
      let leftRight =
        values
        |> Array.groupBy snd
        |> Array.sumBy(fun (j, col) ->
          let is = col |> Array.map fst |> Array.sort
          + countSegments is (fun i -> points.Contains(i, j + 1))
          + countSegments is (fun i -> points.Contains(i, j - 1))
        )
      values.Length * (aboveBelow + leftRight)
    ) =! expected

module Day11 =

  let input = [|
    "125 17"
  |]

  let sample (result: int) = makeSample result input

  let hasEvenNumberOfDigits (value: int64) =
    let mutable count = 0
    let mutable value = value
    while value > 0 do
      count <- count + 1
      value <- value / 10L
    count

  let splitEvenDigitsIntoPairs count (value: int64) (left: outref<int64>) (right: outref<int64>) =
    left <- value
    right <- 0L
    let mutable rem   = 0L
    let mutable tens  = 1L
    let mutable count = count
    while count > 0 do
      left  <- Math.DivRem(left, 10L, &rem)
      right <- tens * rem + right
      tens  <- tens * 10L
      count <- count - 1

  let trySplit(value: int64) (left: outref<int64>) (right: outref<int64>) =
    let count = hasEvenNumberOfDigits value
    if count % 2 = 0 then
      splitEvenDigitsIntoPairs (count / 2) value &left &right
      true
    else false

  let applyRule (value: int64) = [|
    let mutable left, right = 0L, 0L
    if value = 0L
    then 1L
    elif trySplit value &left &right then
      left
      right
    else value * 2024L
  |]

  [<Theory>]
  [<FileData(2024, 11, 199753)>]
  [<MemberData(nameof sample, 55312)>]
  let part1 (input: string array) expected =
    let mutable stones = Seq.empty
    stones <- input |> Array.exactlyOne |> parseNumbers<int64>
    for _ = 1 to 25 do
      stones <- stones |> Seq.collect(applyRule)
    Seq.length stones =! expected

  let rec countAfterN (cache: Dictionary<_, _>) n value =
    if n < 1 then
      1L
    else
      applyRule value
      |> Array.sumBy(fun value ->
        let mutable count = 0L
        if cache.TryGetValue((n - 1, value), &count) then
          count
        else
          let count = countAfterN cache (n - 1) value
          cache.Add((n - 1, value), count)
          count
      )

  [<Theory>]
  [<FileData(2024, 11, 199753)>]
  [<MemberData(nameof sample, 55312)>]
  let part1_optimized (input: string array) expected =
    let cache = Dictionary()
    input
    |> Array.sumBy(parseNumbers >> Array.sumBy(countAfterN cache 25))
    =! int64 expected

  [<Theory>]
  [<FileData(2024, 11, 239413123020116L)>]
  let part2 (input: string array) expected =
    let cache = Dictionary()
    input
    |> Array.sumBy(parseNumbers >> Array.sumBy(countAfterN cache 75))
    =! expected

module Day10 =

  let input = [|
    "89010123"
    "78121874"
    "87430965"
    "96549874"
    "45678903"
    "32019012"
    "01329801"
    "10456732"
  |]

  let sample (result: int) = makeSample result input

  let directions = [|
    1,   0
    0,  -1
    -1,  0
    0,   1
  |]

  let get (input: string array) (i, j) = directions |> Array.choose(fun (dx, dy) ->
    let one = '1' - '0'
    try
      let i', j' = i + dx, j + dy
      if input[i'][j'] - input[i][j] = one
      then Some(i', j')
      else None
    with :? IndexOutOfRangeException -> None
  )

  let run distinct (input: string array) =
    let mutable sum = 0
    for i = 0 to input.Length - 1 do
      let line = input[i]
      for j = 0 to line.Length - 1 do
        if line[j] = '0' then
          let rec loop p = seq {
            for p in get input p do
              yield p
              yield! loop p
          }
          let count =
            loop (i, j)
            |> fun path -> if distinct then path |> Seq.distinct else path
            |> Seq.countTrue(fun (i, j) -> input[i][j]  = '9')
          sum <- sum + count
    sum

  [<Theory>]
  [<FileData(2024, 10, 816)>]
  [<MemberData(nameof sample, 36)>]
  let part1 (input: string array) expected =
    run true input=! expected

  [<Theory>]
  [<FileData(2024, 10, 1960)>]
  [<MemberData(nameof sample, 81)>]
  let part2 (input: string array) expected =
    run false input=! expected

module Day09 =

  let input = [|
    "2333133121414131402"
  |]

  let sample (result: int64) = makeSample result input

  let parse (input: string array) =
    input
    |> Array.exactlyOne
    |> Seq.mapi(fun i ch ->
      let fileId = if i % 2 = 0 then int64 i / 2L else -1L
      let count = int ch - int '0'
      Seq.init count (fun _ -> fileId)
    )
    |> Seq.collect id
    |> Seq.toArray

  let checksum expanded =
    let mutable i = -1L
    expanded
    |> Array.sumBy(fun x ->
      i <- i + 1L
      if x > -1L then x * i else 0L
    )

  let compact disk =
    let empty = -1L
    let mutable hole = Array.IndexOf(disk, empty)

    let span = disk.AsSpan()
    let mutable file = span.LastIndexOfAnyExcept(empty)

    while hole < file do
      disk[hole] <- disk[file]
      disk[file] <- empty
      hole <- Array.IndexOf(disk, empty, hole + 1)
      file <- span.LastIndexOfAnyExcept(empty)
    disk

  [<Theory>]
  [<FileData(2024, 9, 6359213660505L)>]
  [<MemberData(nameof sample, 1928L)>]
  let part1 (input: string array) expected =
    parse input
    |> compact
    |> checksum
    =! expected

  let compact2 (disk: int64 array) =
    let empty = -1L

    let rec run length =
      let mutable span = disk.AsSpan(0, length)

      let fileStart, fileEnd =
        let fileEnd = span.LastIndexOfAnyExcept(empty)
        if fileEnd < 0
        then -1, 1
        else span.Slice(0, fileEnd).LastIndexOfAnyExcept(span[fileEnd]) + 1, fileEnd + 1

      let fileSize = fileEnd - fileStart
      if fileSize > 0 then
        let fileSpan = span.Slice(fileStart, fileSize)

        let mutable holeStart, holeEnd = span.IndexOf(empty), 0

        while 0 < holeStart && holeEnd < fileStart do
          holeEnd <- holeStart + span.Slice(holeStart).IndexOfAnyExcept(empty)
          let freeSpace = holeEnd - holeStart
          if freeSpace > 0 then
            if fileSize <= freeSpace then
              fileSpan.CopyTo(span.Slice(holeStart))
              fileSpan.Fill(empty)
              holeEnd <- fileStart
            else
              span <- span.Slice(holeEnd)
            holeStart <- span.IndexOf(empty)
          else
            holeStart <- 0

      if fileStart > 0 then run fileStart

    run disk.Length
    disk

  [<Theory>]
  [<FileData(2024, 9, 6381624803796L)>]
  [<MemberData(nameof sample, 2858L)>]
  let part2 (input: string array) expected =
    parse input
    |> compact2
    |> checksum
     =! expected

module Day08 =

  let input = [|
    "............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"
  |]

  let sample (result: int) = makeSample result input

  let getNodes (input: string array) =
    let nodes = Dictionary()
    for i = 0 to input.Length - 1 do
      let line = input[i]
      for j = 0 to line.Length - 1 do
        let ch = line[j]
        if ch <> '.' then
          let values =
            match nodes.TryGetValue(ch) with
            | true, values -> values
            | false, _     -> [ ]
          nodes[ch] <- Point(i, j) :: values
    nodes.Values

  [<Theory>]
  [<FileData(2024, 8, 423)>]
  [<MemberData(nameof sample, 14)>]
  let part1 (input: string array) expected =
    let min, max = Point(0, 0), Point(input.Length, input[0].Length)
    getNodes input
    |> Seq.collect(fun nodes ->
      Seq.allPairs nodes nodes
      |> Seq.collect(fun (p1, p2) ->
        if p1 <= p2 then
          Seq.empty
        else
          let dx = p2 - p1
          [|
            p1 - dx
            p2 + dx
          |]
      )
    )
    |> Seq.filter(fun p -> min <= p && p < max)
    |> Seq.countDistinct
    =! expected

  [<Theory>]
  [<FileData(2024, 8, 1287)>]
  [<MemberData(nameof sample, 34)>]
  let part2 (input: string array) expected =
    let min, max = Point(0, 0), Point(input.Length, input[0].Length)
    getNodes input
    |> Seq.collect(fun nodes ->
      Seq.allPairs nodes nodes
      |> Seq.collect(fun (p1, p2) ->
        if p1 <= p2 then
          Seq.empty
        else
          let dx = p2 - p1
          let first =
            Seq.initInfinite (fun i -> p1 - i * dx)
            |> Seq.takeWhile(fun p -> min <= p && p < max)
          let second =
            Seq.initInfinite (fun i -> p2 + i * dx)
            |> Seq.takeWhile(fun p -> min <= p && p < max)
          Seq.append first second
      )
    )
    |> Seq.countDistinct
    =! expected

module Day07 =

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

module Day06 =

  let input = [|
    "....#....."
    ".........#"
    ".........."
    "..#......."
    ".......#.."
    ".........."
    ".#..^....."
    "........#."
    "#........."
    "......#..."
  |]

  let sample (result: int) = makeSample result input

  let getStartingPoint (input: string array) =
    let x = input |> Array.findIndex(fun x -> x.Contains('^'))
    let y = input[x].AsSpan().IndexOf('^')
    x, y

  let move (map: char array array) p0 =
    let set = HashSet()
    let rec loop direction (x, y) =
      if set.Add(x, y, direction) then
        map[x][y] <- 'X'
        let x2, y2, direction2 =
          match direction with
          | 'u' -> x - 1, y, 'l'
          | 'd' -> x + 1, y, 'r'
          | 'l' -> x, y + 1, 'd'
          | 'r' -> x, y - 1, 'u'
          | _ -> failwith "Invalid direction"
        if x2 >= 0 && x2 < map.Length && y2 >= 0 && y2 < map[0].Length then
          if map[x2][y2] = '#'
          then loop direction2 (x, y)
          else loop direction (x2, y2)
        else 0
      else 1
    loop 'u' p0

  [<Theory>]
  [<FileData(2024, 6, 4722)>]
  [<MemberData(nameof sample, 41)>]
  let part1 (input: string array) expected =
    let map = input |> Array.map(fun x -> x.ToCharArray())
    getStartingPoint input |> move map =! 0
    map
    |> Array.sumBy(Array.countTrue ((=) 'X'))
    =! expected

  [<Theory>]
  [<FileData(2024, 6, 1602)>]
  [<MemberData(nameof sample, 6)>]
  let part2 (input: string array) expected =
    let p0 = getStartingPoint input
    let map0 = input |> Array.map(fun x -> x.ToCharArray())
    move map0 p0 =! 0
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[0].Length - 1 do
        if input[i][j] = '.' && map0[i][j] = 'X' then
          let map = input |> Array.map(fun x -> x.ToCharArray())
          map[i][j] <- '#'
          count <- count + move map p0
    count =! expected

module Day05 =

  let input = [|
    "47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
    ""
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"
  |]

  let sample (result: int) = makeSample result input

  let parse (input: string array) =
    let rules, updates =
      input
      |> Array.map parseNumbers<int>
      |> Array.partition(fun a -> a.Length = 2)
    let postRules = rules |> Array.groupBy(Array.item 0) |> Array.map(fun (ch, grp) -> ch, grp |> Array.map(Array.item 1) |> Set.ofArray) |> Map.ofArray
    let preRules  = rules |> Array.groupBy(Array.item 1) |> Array.map(fun (ch, grp) -> ch, grp |> Array.map(Array.item 0) |> Set.ofArray) |> Map.ofArray
    preRules, postRules, updates

  let (|||) option defaultValue = option |> Option.defaultValue defaultValue

  [<Theory>]
  [<FileData(2024, 5, 5732)>]
  [<MemberData(nameof sample, 143)>]
  let part1 (input: string array) expected =
    let preRules, postRules, updates = parse input
    updates
    |> Array.sumBy(fun update ->
      let isValid =
        update.Length > 0 &&
        update
        |> Array.indexed
        |> Array.forall(fun (i, ch) ->
          let preRule  =  preRules.TryFind(ch) ||| Set.empty
          let postRule = postRules.TryFind(ch) ||| Set.empty
          Seq.init update.Length id
          |> Seq.forall(fun j ->
               i = j
            || j < i &&  preRule.Contains(update[j])
            || j > i && postRule.Contains(update[j])
          )
        )
      if isValid then update[update.Length / 2] else 0
    ) =! expected

  [<Theory>]
  [<FileData(2024, 5, 4716)>]
  [<MemberData(nameof sample, 123)>]
  let part2 (input: string array) expected =
    let preRules, postRules, updates = parse input
    updates
    |> Array.sumBy(fun update ->
      let isValid =
        update
        |> Array.indexed
        |> Array.forall(fun (i, ch) ->
          let preRule  =  preRules.TryFind(ch) ||| Set.empty
          let postRule = postRules.TryFind(ch) ||| Set.empty
          Seq.init update.Length id
          |> Seq.forall(fun j ->
            i = j
            || j < i &&  preRule.Contains(update[j])
            || j > i && postRule.Contains(update[j])
          )
        )
      if isValid then 0 else
        update
        |> Array.sortInPlaceWith(fun a b ->
          if (postRules.TryFind(a) ||| Set.empty).Contains(b) && (preRules.TryFind(b) ||| Set.empty).Contains(a) then -1 else 1
        )
        update[update.Length / 2]
    ) =! expected

module Day04 =

  let input = [|
    "MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"
  |]

  let directions = [|
    0,  1
    1,  1
    1,  0
    1, -1
    0, -1
    -1, -1
    -1,  0
    -1,  1
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2024, 4, 2496)>]
  [<MemberData(nameof sample, 18)>]
  let part1 (input: string array) expected =
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[i].Length - 1 do
        if input[i][j] = 'X' then
          let subCount =
            directions
            |> Array.countTrue(fun (dx, dy) ->
              try  input[i + 1 * dx][j + 1 * dy] = 'M'
                && input[i + 2 * dx][j + 2 * dy] = 'A'
                && input[i + 3 * dx][j + 3 * dy] = 'S'
              with :? IndexOutOfRangeException -> false
            )
          count <- count + subCount
    count =! expected

  [<Theory>]
  [<FileData(2024, 4, 1967)>]
  [<MemberData(nameof sample, 9)>]
  let part2 (input: string array) expected =
    let mutable count = 0
    for i = 0 to input.Length - 1 do
      for j = 0 to input[i].Length - 1 do
        if input[i][j] = 'A' then
          try
            let xp, xm = input[i + 1], input[i - 1]
            let ch1, ch2  = xp[j + 1], xm[j - 1]
            if (ch1 = 'M' && ch2 = 'S') || (ch1 = 'S' && ch2 = 'M') then
              let ch3, ch4  = xm[j + 1], xp[j - 1]
              if (ch3 = 'M' && ch4 = 'S') || (ch3 = 'S' && ch4 = 'M') then
                count <- count + 1
          with :? IndexOutOfRangeException -> ()
    count =! expected

module Day03 =

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
    Regex.Matches(String.Concat input, "mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)")
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
    ((0, true), Regex.Matches(String.Concat input, "(mul\((?<d1>\d{1,3}),(?<d2>\d{1,3})\)|do\(\)|don't\(\))"))
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

module Day02 =

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

module Day01 =

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
