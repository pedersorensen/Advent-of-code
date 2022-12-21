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
open System.Text
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

  let sample (result: string) = makeSample result [|
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
  let part1 (input: string []) expected =
    let sep = Array.IndexOf(input, "")
    let stacks = getStacks sep input
    getInstructions sep input |> Array.iter(move stacks)
    String.Join("", stacks |> Array.map Seq.head) =! expected

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
    String.Join("", stacks |> Array.map Seq.head) =! expected

 module Day06 =

  let sample (result: int) = makeSample result [|
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb" // first marker after character 7
    "bvwbjplbgvbhsrlpgdmjqwftvncz" // first marker after character 5
    "nppdvjthqldpwncqszvftbrmjlhg" // first marker after character 6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" // first marker after character 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" // first marker after character 11
  |]

  let findStartOfPacket (s: string) =
    let rec loop1 a b c d i =
      if
        a <> b && a <> c && a <> d &&
        b <> c && b <> d &&
        c <> d
      then i else
        loop1 b c d s[i] (i + 1)

    loop1 s[0] s[1] s[2] s[3] 4

  let findStartOfPacket2 count (s: string) =
    let queue = Queue(Seq.take count s)
    let rec loop i =
      if queue |> Seq.distinct |> Seq.length = count
      then i else
        queue.Dequeue() |> ignore
        queue.Enqueue(s[i])
        loop (i + 1)
    loop count

  [<Theory>]
  [<FileData(2022, 6, 1920)>]
  [<MemberData(nameof sample, 7)>]
  let part1 (input: string []) expected =
    findStartOfPacket2 4 input[0] =! expected

  let findStartOfMessage (s: string) =
    let rec loop a b c d i =
      if
        a <> b && a <> c && a <> d &&
        b <> c && b <> d &&
        c <> d
      then i else loop b c d s[i] (i + 1)

    loop s[0] s[1] s[2] s[3] 4

  [<Theory>]
  [<FileData(2022, 6, 2334)>]
  [<MemberData(nameof sample, 19)>]
  let part2 (input: string []) expected =
    findStartOfPacket2 14 input[0] =! expected

 module Day07 =

  let input = [|
    "$ cd /"
    "$ ls"
    "dir a"
    "14848514 b.txt"
    "8504156 c.dat"
    "dir d"
    "$ cd a"
    "$ ls"
    "dir e"
    "29116 f"
    "2557 g"
    "62596 h.lst"
    "$ cd e"
    "$ ls"
    "584 i"
    "$ cd .."
    "$ cd .."
    "$ cd d"
    "$ ls"
    "4060174 j"
    "8033020 d.log"
    "5626152 d.ext"
    "7214296 k"
  |]

  let sample (result: int) = makeSample result input

  type Directory =
    { Name        : string
      Files       : File list
      Directories : Map<string, Directory> }
    static member Create(name)  =
      { Name        = name
        Files       = []
        Directories = Map.empty }
  and File = File of name : string * size : int

  module Directory =

    let withFile file directory =
      { directory with Files = file :: directory.Files }

    let withSubDir subDir directory =
      { directory with Directories = directory.Directories.Add(subDir.Name, subDir) }

  let printDir (dir: Directory) =
    let sb = StringBuilder()
    let rec printDir indent (d: Directory) =
      Printf.bprintf sb $"{indent} - {d.Name} (dir)"
      sb.AppendLine() |> ignore
      let indent = indent + "  "
      for kvp in d.Directories do
        printDir indent kvp.Value
      for File(name, size) in d.Files do
        Printf.bprintf sb $"{indent} - {name} (file, size={size})"
        sb.AppendLine() |> ignore
    printDir "" dir
    sb.ToString()

  #if INTERACTIVE
  fsi.AddPrinter printDir
  #endif

  let handle (dir: Directory, stack: Directory list) (l: string) =
    let mutable fileSize = 0
    match l.Split(' ') with
    | [| "$" ; "cd" ; "/"  |] -> dir, stack
    | [| "$" ; "cd" ; ".." |] ->
      match stack with
      | [] -> dir, []
      | parent :: stack ->
        parent |> Directory.withSubDir dir, stack
    | [| "$" ; "cd" ; folder |] -> dir.Directories[folder], dir :: stack
    | [| "$" ; "ls" |] -> dir, stack
    | [| "dir" ; subDir |]      ->
      let subDir = Directory.Create(subDir)
      dir |> Directory.withSubDir subDir, stack
    | [| size ; name |] when Int32.TryParse(size, &fileSize) ->
      let file = File(name, fileSize)
      dir |> Directory.withFile file, stack
    | _ -> failwith $"Don't know: {l}"

  let buildRoot input =
    match input |> Array.fold handle (Directory.Create("/"), []) with
    | dir, [] -> dir
    | dir, stack -> stack |> List.last |> Directory.withSubDir dir

  let rec getSize (d: Directory) =
    let totalFileSize = d.Files |> List.sumBy(fun (File(_, s)) -> s)
    let totalFolderSize = d.Directories |> Seq.sumBy(fun kvp -> getSize kvp.Value)
    totalFileSize + totalFolderSize

  let getFolderSizes directory =
    let rec loop sizes directory =
      (getSize directory :: sizes, directory.Directories.Values)
      ||> Seq.fold loop
    loop [] directory

  [<Theory>]
  [<FileData(2022, 7, 1844187)>]
  [<MemberData(nameof sample, 95437)>]
  let part1 (input: string []) expected =
    let limit = 100_000
    buildRoot input
    |> getFolderSizes
    |> List.sumBy(fun s -> if s < limit then s else 0)
    =! expected

  [<Theory>]
  [<FileData(2022, 7, 4978279)>]
  [<MemberData(nameof sample, 24933642)>]
  let part2 (input: string []) expected =
    let required       = 30_000_000
    let totalAvailable = 70_000_000
    let sizes = buildRoot input |> getFolderSizes
    let totalSize = sizes |> List.last
    let totalFree = totalAvailable - totalSize
    let free = required - totalFree
    (totalSize, sizes)
    ||> List.fold(fun total size ->
      if size > free && size < total then size else total
    )
    // Alternatively, sort the list and find the first size that's above the limit, but that's a lot of extra work.
    //sizes |> List.sort |> List.find(fun s -> s > free) =! expected

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
