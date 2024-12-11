#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: XUnit"
#load "Utils.fs"
#else
namespace Excercises2023
#endif

open Xunit
open System
open System.Buffers
open System.Text.RegularExpressions
open System.Collections.Generic

module Day11 =

  let input = [|
    "...#......"
    ".......#.."
    "#........."
    ".........."
    "......#..."
    ".#........"
    ".........#"
    ".........."
    ".......#.."
    "#...#....."
  |]

  let sample (result: int64) = makeSample result input

  let expand (input: string []) =
    let noGalaxies =
      input
      |> Array.map(fun line -> line.ToCharArray())
      |> Array.reduce(Array.map2 min)

    let horizontalExpanded =
      input
      |> Array.map(fun line ->
        (line.ToCharArray(), noGalaxies)
        ||> Array.zip
        |> Array.collect(fun (line, noGalaxies) ->
          if line = '.' && noGalaxies = '.' then [| '.' ; '.' |] else [| line|]
        ) |> String
      )

    [|
      for line in horizontalExpanded do
        line
        if line.IndexOf('#') < 0 then line
    |]

  let getGalaxies (input: string array) =
    input
    |> Array.mapi(fun i line ->
      line.ToCharArray() |> Seq.mapi(fun j ch -> (i, j), ch)
    )
    |> Seq.collect(Seq.filter(fun (_, ch) -> ch = '#'))
    |> Seq.mapi(fun i (pos, _) -> i, pos)
    |> Seq.toArray

  [<Theory>]
  [<FileData(2023, 11, 9724940L)>]
  [<MemberData(nameof sample, 374L)>]
  let part1 (input: string []) expected =
    let galaxies = getGalaxies (expand input)

    Array.allPairs galaxies galaxies
    |> Array.filter(fun ((g1, _), (g2, _)) -> g1 < g2)
    |> Array.sumBy(fun ((_, (x1, y1)), (_, (x2, y2))) -> abs(x2 - x1) + abs(y2 - y1))
    =! expected

  [<Theory>]
  [<FileData(2023, 11, 569052586852L)>]
  [<MemberData(nameof sample, 82000210L)>]
  let part2 (input: string []) expected =

    let noGalaxyColumns =
      input
      |> Array.map(fun line -> line.ToCharArray())
      |> Array.reduce(Array.map2 min)
      |> Array.indexed
      |> Array.choose(fun (i, ch) -> if ch <> '#' then Some i else None)

    let noGalaxyRows =
      input
      |> Array.indexed
      |> Array.choose(fun (i, ch) -> if ch.Contains('#') then None else Some i)

    let galaxies = getGalaxies input

    let F1 = 1_000_000L - 1L

    Array.allPairs galaxies galaxies
    |> Array.filter(fun ((g1, _), (g2, _)) -> g1 < g2)
    |> Array.sumBy(fun ((_, (x1, y1)), (_, (x2, y2))) ->
      let fx = noGalaxyRows    |> Array.sumBy(fun x -> if (x1 < x && x < x2) || (x2 < x && x < x1) then F1 else 0L)
      let fy = noGalaxyColumns |> Array.sumBy(fun y -> if (y1 < y && y < y2) || (y2 < y && y < y1) then F1 else 0L)
      let dx = int64(abs(x2 - x1)) + fx
      let dy = int64(abs(y2 - y1)) + fy
      dx + dy
    ) =! expected

module Day10 =

  let input = [|
    "-L|F7"
    "7S-7|"
    "L|7||"
    "-L-J|"
    "L|-JF"
  |]

  let input2 = [|
    "7-F7-"
    ".FJ|7"
    "SJLL7"
    "|F--J"
    "LJ.LJ"
  |]

  let sample (result: int) = makeSample result input
  let sample2 (result: int) = makeSample result input2

  type Direction = Up | Down | Left | Right

  [<NoEquality;NoComparison>]
  type Automaton =
    {
      mutable x: int
      mutable y: int
      mutable direction: Direction
      map: string array
    }
    static member Create(x, y, direction, map) =
      {
        x = x
        y = y
        direction = direction
        map = map
      }
    member this.Location = this.x, this.y
    member this.Char = this.map[this.x][this.y]
    member this.Advance() =
      this.direction <-
        match this.direction with
        | Up    -> this.x <- this.x - 1 ; match this.Char with 'F' -> Right | '7' -> Left | '|' -> Up    | _ -> failwithf "%A" this.Char
        | Down  -> this.x <- this.x + 1 ; match this.Char with 'L' -> Right | 'J' -> Left | '|' -> Down  | _ -> failwithf "%A" this.Char
        | Left  -> this.y <- this.y - 1 ; match this.Char with 'F' -> Down  | 'L' -> Up   | '-' -> Left  | _ -> failwithf "%A" this.Char
        | Right -> this.y <- this.y + 1 ; match this.Char with '7' -> Down  | 'J' -> Up   | '-' -> Right | _ -> failwithf "%A" this.Char

  let findStartPositionAndDirections (input: string []) =
    let x = input |> Array.findIndex(fun line -> line.Contains('S'))
    let y = input[x].IndexOf('S')
    let directions =
      [|
        if x < input.Length - 1     then match input[x + 1][y] with | 'L' | 'J' | '|' -> Down  | _ -> ()
        if x > 0                    then match input[x - 1][y] with | 'F' | '7' | '|' -> Up    | _ -> ()
        if y < input.[x].Length - 1 then match input[x][y + 1] with | '7' | 'J' | '-' -> Right | _ -> ()
        if y > 0                    then match input[x][y - 1] with | 'L' | 'F' | '-' -> Left  | _ -> ()
      |]
    let dir1, dir2 =
      match directions with
      | [| dir1 ; dir2 |] -> dir1, dir2
      | _ -> failwithf "Expected exactly two directions, got: %A" directions
    x, y, dir1, dir2

  [<Theory>]
  [<FileData(2023, 10, 6897)>]
  [<MemberData(nameof sample, 4)>]
  [<MemberData(nameof sample2, 8)>]
  let part1 (input: string []) expected =
    let x, y, dir1, dir2 = findStartPositionAndDirections input

    let ant1 = Automaton.Create(x, y, dir1, input)
    let ant2 = Automaton.Create(x, y, dir2, input)

    ant1.Advance()
    ant2.Advance()
    let mutable count = 1
    while ant1.Location <> ant2.Location do
      ant1.Advance()
      ant2.Advance()
      count <- count + 1

    count =! expected

  [<Theory(Skip = "Not completed.")>]
  [<FileData(2023, 10, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    -1 =! expected

module Day9 =

  let input = [|
    "0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2023, 9, 1853145119)>]
  [<MemberData(nameof sample, 114)>]
  let part1 (input: string []) expected =

    let rec loop numbers =
      let numbers' =
        numbers
        |> Array.windowed 2
        |> Array.map(fun a -> a.[1] - a.[0])
      if numbers' |> Array.forall(fun n -> n = 0)
      then numbers[^0]
      else numbers[^0] + loop numbers'

    input
    |> Array.sumBy(parseNumbers >> loop)
    =! expected

  [<Theory>]
  [<FileData(2023, 9, 1853145119)>]
  [<MemberData(nameof sample, 114)>]
  let part1B (input: string []) expected =
    let findNextNumber (numbers : _ array) =
      let mutable max = numbers.Length - 2
      let mutable keepGoing = true
      let mutable sum = numbers[^0]
      while keepGoing do
        keepGoing <- false
        for i = 0 to max do
          numbers[i] <- numbers[i + 1] - numbers[i]
          if numbers[i] <> 0 then keepGoing <- true
        sum <- sum + numbers[max]
        max <- max - 1
      sum

    input
    |> Array.sumBy(parseNumbers >> findNextNumber)
    =! expected

  [<Theory>]
  [<FileData(2023, 9, 923)>]
  [<MemberData(nameof sample, 2)>]
  let part2 (input: string []) expected =

    let rec loop numbers =
      let numbers' =
        numbers
        |> Array.windowed 2
        |> Array.map(fun a -> a.[1] - a.[0])
      if numbers' |> Array.forall(fun n -> n = 0)
      then numbers[0]
      else numbers[0] - loop numbers'

    input
    |> Array.sumBy(parseNumbers >> loop)
    =! expected

  [<Theory>]
  [<FileData(2023, 9, 923)>]
  [<MemberData(nameof sample, 2)>]
  let part2B (input: string []) expected =

    let findFirstNumber (numbers : _ array) =
      let mutable min = 1
      let mutable keepGoing = true
      while keepGoing do
        keepGoing <- false
        for i = numbers.Length - 1 downto min do
          numbers[i] <- numbers[i] - numbers[i - 1]
          if numbers[i] <> 0 then keepGoing <- true
        min <- min + 1
      numbers |> Array.reduceBack (-)

    input
    |> Array.sumBy(parseNumbers >> findFirstNumber)
    =! expected

module Day08 =

  let input1 = [|
    "RL"
    ""
    "AAA = (BBB, CCC)"
    "BBB = (DDD, EEE)"
    "CCC = (ZZZ, GGG)"
    "DDD = (DDD, DDD)"
    "EEE = (EEE, EEE)"
    "GGG = (GGG, GGG)"
    "ZZZ = (ZZZ, ZZZ)"
  |]

  let input2 = [|
    "LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)"
  |]

  let sample1 (result: int) = makeSample result input1
  let sample2 (result: int) = makeSample result input2

  let parseNodes (input: string array) =
    input
    |> Array.skip 2
    |> Array.map(fun line ->
      match line.Split([| '=' ; ',' ; ' ' ; '(' ; ')' |], StringSplitOptions.RemoveEmptyEntries) with
      | [| name ; left ; right |] -> name, (left, right)
      | _ -> failwithf "%A" line
    )
    |> Map.ofArray

  let parseNodes2 (input: string array) =
    let left, right =
      input
      |> Array.skip 2
      |> Array.map(fun line ->
        match line.Split([| '=' ; ',' ; ' ' ; '(' ; ')' |], StringSplitOptions.RemoveEmptyEntries) with
        | [| name ; left ; right |] -> (name, left), (name, right)
        | _ -> failwithf "%A" line
      )
      |> Array.unzip
    Map.ofArray left, Map.ofArray right

  [<Theory>]
  [<FileData(2023, 8, 19241)>]
  [<MemberData(nameof sample1, 2)>]
  [<MemberData(nameof sample2, 6)>]
  let part1 (input: string []) expected =
    let instructions = input[0]
    let left, right = parseNodes2 input

    let rec loop (i: int) (node: string) =
      let node =
        match instructions[i % instructions.Length] with
        | 'L' -> left[node]
        | 'R' -> right[node]
        | instruction -> failwithf "%A" instruction
      if node = "ZZZ" then i else
      loop (i + 1) node

    loop 0 "AAA" + 1 =! expected

  [<Theory(Skip = "Not done yet")>]
  [<FileData(2023, 8, 0)>]
  [<MemberData(nameof sample1, 0)>]
  [<MemberData(nameof sample2, 0)>]
  let part2 (input: string []) expected =
    -1 =! expected

module Day07 =

  let input = [|
    "32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"
  |]

  let sample (result: int) = makeSample result input

  type Types =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

  let getType (hand: string) =
    let counts = hand.ToCharArray() |> Array.countBy id |> Array.sortBy snd
    match counts with
    | [| (_, 1) ; (_, 1) ; (_, 1) ; (_, 1) ; (_, 1) |] -> HighCard
    | [| (_, 1) ; (_, 1) ; (_, 1) ; (_, 2) |]          -> OnePair
    | [| (_, 1) ; (_, 2) ; (_, 2) |]                   -> TwoPair
    | [| (_, 1) ; (_, 1) ; (_, 3) |]                   -> ThreeOfAKind
    | [| (_, 2) ; (_, 3) |]                            -> FullHouse
    | [| (_, 1) ; (_, 4) |]                            -> FourOfAKind
    | [| (_, 5) |]                                     -> FiveOfAKind
    | _ -> failwithf "%A" counts

  let cardRanks = Map [|
    '2', 1
    '3', 2
    '4', 3
    '5', 4
    '6', 5
    '7', 6
    '8', 7
    '9', 8
    'T', 9
    'J', 10
    'Q', 11
    'K', 12
    'A', 13
  |]

  [<Theory>]
  [<FileData(2023, 7, 248113761)>]
  [<MemberData(nameof sample, 6440)>]
  let part1 (input: string []) expected =
    input
    |> Array.map(fun line ->
      let hand, bid =
        match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
        | [| hand ; bid |] ->
          hand, int bid
        | _ -> failwithf "%A" line
      hand, bid, getType hand
    )
    |> Array.sortWith(fun (hand1, _, type1) (hand2, _, type2) ->
      let c = compare type1 type2

      let compare (ch1: char) (ch2: char) =
        compare cardRanks[ch1] cardRanks[ch2]

      if c <> 0 then -c else
        let c1 = compare hand1[0] hand2[0]
        if c1 <> 0 then c1 else
          let c2 = compare hand1[1] hand2[1]
          if c2 <> 0 then c2 else
            let c3 = compare hand1[2] hand2[2]
            if c3 <> 0 then c3 else
              let c4 = compare hand1[3] hand2[3]
              if c4 <> 0 then c4 else
                let c5 = compare hand1[4] hand2[4]
                c5
    )
    |> Array.mapi(fun i (_, bid, _) -> (i + 1) * bid)
    |> Array.sum
    =! expected

  [<Theory>]
  [<FileData(2023, 7, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    0 =! expected

module Day06 =

  let input = [|
    "Time:      7  15   30"
    "Distance:  9  40  200"
  |]

  let sample (result: int) = makeSample result input

  let countDistances (totalTime: int64) record =
    let mutable count = 0
    for time = 0 to int totalTime - 1 do
      let speed = int64 time
      let distance = speed * (totalTime - int64 time)
      if distance > record then count <- count + 1
    count

  [<Theory>]
  [<FileData(2023, 6, 170000)>]
  [<MemberData(nameof sample, 288)>]
  let part1 (input: string []) expected =
    let times   = parseNumbers input[0]
    let records = parseNumbers input[1]

    (times, records)
    ||> Array.map2 countDistances
    |> Array.reduce (*) =! expected

  [<Theory>]
  [<FileData(2023, 6, 20537782)>]
  [<MemberData(nameof sample, 71503)>]
  let part2 (input: string []) expected =
    let times   = parseNumbers (input[0].Replace(" ", ""))
    let records = parseNumbers (input[1].Replace(" ", ""))
    (times, records)
    ||> Array.map2 countDistances
    |> Array.reduce (*) =! expected

module Day05 =

  let input = [|
    "seeds: 79 14 55 13"
    ""
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"
  |]

  let sample (result: int) = makeSample result input

  let parse input =
    ((Array.empty, List.empty, List.empty), input)
    ||> Array.fold(fun (seeds, maps, entries) (line: string) ->
      match line.Split([| ':' ;  ' ' |], StringSplitOptions.RemoveEmptyEntries) with
      | [||] -> (seeds, maps, entries)
      | a when a[0] = "seeds" ->
        let seeds = Array.skip 1 a |> Array.map int64
        seeds, maps, entries
      | [| _ ; "map" |] ->
        let maps' = if entries.IsEmpty then maps else entries :: maps
        seeds, maps', List.empty
      | [| dst ; src ; range |] ->
        seeds, maps, (int64 dst, int64 src, int64 range) :: entries
      | a -> failwithf "%A" a
    )
    |> fun (seeds, maps, list) -> seeds, List.rev(list :: maps)

  let findEntry n entries : int64 =
    entries
    |> List.tryPick(fun (dst, src, range) ->
      let d = n - src
      if src <= n && d < range then Some (d + dst) else None
    )
    |> Option.defaultValue n

  [<Theory>]
  [<FileData(2023, 5, 650599855)>]
  [<MemberData(nameof sample, 35)>]
  let part1 (input: string []) expected =
    let seeds, maps = parse input
    (Int64.MaxValue, seeds)
    ||> Array.fold(fun i seed ->
      (seed, maps)
      ||> List.fold(fun src entries -> findEntry src entries)
      |> min i
    )
    =! expected

  [<Theory>]
  [<FileData(2023, 5, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    0 =! expected

module Day04 =

  let input = [|
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(2023, 4, 25183)>]
  [<MemberData(nameof sample, 13)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      match line.Split([| ':' ;  '|' |], StringSplitOptions.RemoveEmptyEntries) with
      | [| _card ; winners ; numbers|] ->
        let winners = winners.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let numbers = numbers.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let count = Set.intersect winners numbers |> Set.count
        pown 2 (count - 1)
      | _ -> failwithf ""
    ) =! expected

  [<Theory>]
  [<FileData(2023, 4, 5667240)>]
  [<MemberData(nameof sample, 30)>]
  let part2 (input: string []) expected =
    let cardsWon = Dictionary<string, int>()
    let increment n card =
      match cardsWon.TryGetValue(card) with
      | true, count -> cardsWon.[card] <- count + n
      | _ -> cardsWon.[card] <- n
      cardsWon.[card]

    input
    |> Array.sumBy(fun line ->
      match line.Split([| ':' ;  '|' |], StringSplitOptions.RemoveEmptyEntries) with
      | [| card ; winners ; numbers|] ->
        let cardId = card.Substring(5) |> int
        let card = $"Card {cardId}"
        let cardCount = increment 1 card
        let winners = winners.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let numbers = numbers.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Set
        let count = Set.intersect winners numbers |> Set.count
        for i = 1 to count do
          let card = $"Card {cardId + i}"
          increment cardCount card |> ignore
        cardCount
      | _ -> failwithf ""
    ) =! expected

module Day03 =

  let input = [|
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
  |]

  let sample (result: int) = makeSample result input

  let getNumbers input =
    input
    |> Array.mapi(fun i line -> [|
        for m in Regex.Matches(line, "[0-9]+") do
          let partNumberId = (i, m.Index)
          let value = (Int32.Parse(m.ValueSpan), partNumberId)
          for j = 0 to m.Length - 1 do
            (i, m.Index + j), value
      |]
    )
    |> Array.collect id
    |> Map.ofArray

  let allDirections = [|
    (-1, -1); (-1, 0); (-1, 1)
    ( 0, -1);          ( 0, 1)
    ( 1, -1); ( 1, 0); ( 1, 1)
  |]

  [<Theory>]
  [<FileData(2023, 3, 553079)>]
  [<MemberData(nameof sample, 4361)>]
  let part1 (input: string []) expected =
    let numbers       = getNumbers input
    let notPartNumber = SearchValues.Create("0123456789.")
    let seen          = HashSet()
    let mutable sum = 0
    for i = 1 to input.Length - 2 do
      for j = 1 to input.[i].Length - 2 do
        if notPartNumber.Contains(input[i].[j]) |> not then
          for (di, dj) in allDirections do
            match numbers.TryFind(i + di, j + dj) with
            | Some(value, partNumberId) when seen.Add(partNumberId) ->
              sum <- sum + value
            | _ -> ()
    sum =! expected

  [<Theory>]
  [<FileData(2023, 3, 84363105)>]
  [<MemberData(nameof sample, 467835)>]
  let part2 (input: string []) expected =
    let numbers     = getNumbers input
    let mutable sum = 0
    for i = 1 to input.Length - 2 do
      for j = 1 to input.[i].Length - 2 do
        if input[i].[j] = '*' then
          allDirections
          |> Array.choose(fun (di, dj) -> numbers.TryFind(i + di, j + dj))
          |> Array.distinctBy snd
          |> function
          | [| (v1, _); (v2, _) |] -> sum <- sum + v1 * v2
          | _ -> ()
    sum =! expected

module Day02 =

  let input = [|
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  |]

  let sample (result: int) = makeSample result input

  let bag = Map [|
    "red", 12
    "green", 13
    "blue", 14
  |]

  [<Theory>]
  [<FileData(2023, 2, 2447)>]
  [<MemberData(nameof sample, 8)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let split =
        line.Split([| ':' ;  ';' |], StringSplitOptions.RemoveEmptyEntries)
      let gameId = split[0].Substring(5) |> int
      let a =
        split
        |> Array.forall(fun l ->
          l.Split([| ',' ; ' ' |], StringSplitOptions.RemoveEmptyEntries)
          |> Array.chunkBySize 2
          |> Array.forall(fun a ->
            if a[0] = "Game" then true else
            let color = a[1]
            let count = int a[0]
            count <= bag[color]
          )
        )

      if a then gameId else 0
    )
     =! expected

  [<Theory>]
  [<FileData(2023, 2, 56322)>]
  [<MemberData(nameof sample, 2286)>]
  let part2 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let rounds =
        line.Split([| ':' ;  ';' ; ',' ; ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.chunkBySize 2

      (rounds, Map.empty)
      ||> Array.foldBack(fun a map ->
        if a[0] = "Game" then map else

        let color = a[1]
        let count = int a[0]

        map
        |> Map.change color (function
          | None -> Some count
          | Some c -> Some (max c count)
        )
      )
      |> Seq.fold(fun prod kvp -> prod * kvp.Value) 1
    ) =! expected

module Day01 =

  let sample1 (result: int) = makeSample result [|
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
  |]

  let digits = SearchValues.Create("0123456789")

  [<Theory>]
  [<FileData(2023, 1, 54_634)>]
  [<MemberData(nameof sample1, 142)>]
  let part1 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)
      10 * int(line[i] - '0') + int(line[j] - '0')
    ) =! expected

  let sample2 (result: int) = makeSample result [|
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
  |]

  let digitsAsWords =
    [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

  [<Theory>]
  [<FileData(2023, 1, 53855)>]
  [<MemberData(nameof sample2, 281)>]
  let part2 (input: string []) expected =
    input
    |> Array.sumBy(fun line ->
      let span = line.AsSpan()
      let i = span.IndexOfAny(digits)
      let j = span.LastIndexOfAny(digits)

      let indexes =
        digitsAsWords
        |> Array.mapi(fun value digit -> value, line.IndexOf(digit), line.LastIndexOf(digit))

      let minWordValue, i2, _ =
        indexes
        |> Array.minBy(fun (_, idx, _) ->
          if idx < 0 then Int32.MaxValue else idx
        )

      let minValue =
        if i < 0 || (-1 < i2 && i2 < i)
        then minWordValue
        else int(line[i] - '0')

      let maxWordValue, _, j2 = indexes |> Array.maxBy(fun (_, _, idx) -> idx)

      let maxValue =
        if j < 0 || (-1 < j2 && j2 > j)
        then maxWordValue
        else int(line[j] - '0')

      10 * minValue + maxValue
    )
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
  [<FileData(2023, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string []) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(2023, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string []) expected =
    -1 =! expected

"""

makeTemplate |> clip

#endif
