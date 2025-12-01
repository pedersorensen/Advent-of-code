[<AutoOpen>]
module Utils

open System
open System.IO
open System.Buffers
open System.Collections.Generic
open FSharp.Data
open Xunit

#if INTERACTIVE

open System.Text.RegularExpressions

let getHtml (year: int) (day: int) =
  let cookie =
    if File.Exists("cookie.txt")
    then File.ReadAllText("cookie.txt")
    else Environment.GetEnvironmentVariable("cookie")
  let cookies = [| "session", cookie |]
  Directory.CreateDirectory($"input{year}") |> ignore
  let url = $"https://adventofcode.com/{year}/day/{day}"
  Http.RequestString(url, cookies = cookies)

let extractSamples (html: string) =
  let regex = Regex(@"<pre><code>(?<sample>[\s\S]*?)</code></pre>", RegexOptions.Multiline)
  regex.Matches(html)
  |> Seq.cast<Match>
  |> Seq.map(fun m -> m.Groups["sample"].Value)

let makeTemplate year day =
  let html   = getHtml year day
  let sample = extractSamples html |> Seq.tryHead |> Option.defaultValue "Sample not found"
  let sample = sample.Trim().Replace("\n", "\"\n    \"").Replace("&gt;", ">")

  $"""
module Day%02i{day} =

  let input = [|
    "{sample}"
  |]

  let sample (result: int) = makeSample result input

  [<Theory>]
  [<FileData(%i{year}, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part1 (input: string array) expected =
    -1 =! expected

  [<Theory>]
  [<FileData(%i{year}, %i{day}, 0)>]
  [<MemberData(nameof sample, 0)>]
  let part2 (input: string array) expected =
    -1 =! expected
"""

#endif

let ensureExists (year: int) (day: int) =
  let path = $"input{year}/day{day}.txt"
  if File.Exists path |> not then
    let cookie =
      if File.Exists("cookie.txt")
      then File.ReadAllText("cookie.txt")
      else Environment.GetEnvironmentVariable("cookie")
    let cookies = [| "session", cookie |]
    Directory.CreateDirectory($"input{year}") |> ignore
    printfn $"Input for day {day} does not exists, downloading file {path}."
    let url = $"https://adventofcode.com/{year}/day/{day}/input"
    let response = Http.RequestString(url, cookies = cookies)
    File.WriteAllText(path, response)
  path

type FileDataAttribute(year, day, result: obj) =
  inherit Sdk.DataAttribute()
  override _.GetData(_) =
    let path = ensureExists year day
    [| [| File.ReadAllLines(path) |> box ; result |] |]

let (|Ints|) (data: string[]) = data |> Array.map int
let (|SingeLineInts|) (data: string[]) = (Array.exactlyOne data).Split(',') |> Array.map int

let makeSample result (data: string []) = [| [| box data ; box result |] |]

let inline (=!) (actual : 'T) (expected : 'T) = Assert.Equal<'T>(expected, actual)

let readInput year (day: int) = File.ReadAllLines(ensureExists year day)
let readAllInput year (day: int) = File.ReadAllText(ensureExists year day)

let readsInts year day = (readInput year day |> Array.exactlyOne).Split(',') |> Array.map int
let readsInt64s year day = (readInput year day |> Array.exactlyOne).Split(',') |> Array.map int64

let digits = SearchValues.Create("-0123456789")

let tryParseSlice<'T when 'T :> ISpanParsable<'T>> (span: ReadOnlySpan<char> byref) (value: 'T byref) =
  let mutable i = span.IndexOfAny(digits)
  if i < 0 then false else

  let j = span.Slice(i).IndexOfAnyExcept(digits)

  let slice =
    if j < 0
    then span.Slice(i)
    else span.Slice(i, j)

  if j < 0
  then span <- ReadOnlySpan.Empty
  else span <- span.Slice(i + j)

  'T.TryParse(slice, null, &value)

let parseNumbers<'T when 'T :> ISpanParsable<'T>> (line: string) =
  let results = ResizeArray<'T>()
  let mutable span = line.AsSpan()
  let mutable value = Unchecked.defaultof<_>
  while tryParseSlice &span &value do
    results.Add(value)
  results.ToArray()

/// Greatest Common Divisor
let rec gcd a b = if b = 0 then a else gcd b (a % b)
/// Greatest Common Divisor
let rec gcd64 a b = if b = 0L then a else gcd64 b (a % b)
/// Lowest Common Multiple
let lcm64 a b = a * b / gcd64 a b

let cons head tail = head :: tail

module Seq =

  let countTrue predicate source =
    source |> Seq.sumBy(fun x -> if predicate x then 1 else 0)

  let batchOnNewline combiner accumulator init array =
    (init, array)
    ||> Seq.fold(fun (state, acc) line ->
      if String.IsNullOrWhiteSpace line
      then fst init, accumulator state acc
      else combiner line state, acc)
    |> fun (set, count) -> accumulator set count

  let countDistinct (input: _ seq) =
    let set = HashSet()
    input |> countTrue set.Add

[<RequireQualifiedAccess>]
module Array =

  let countTrue predicate source =
    source |> Array.sumBy(fun x -> if predicate x then 1 else 0)

  let private toArray (array : ResizeArray<_>) =
    if array.Count = 0 then Array.Empty() else array.ToArray()

  /// Splits the collection into two collections, containing the elements for
  /// which the given projection returns 'Choice1Of2' and 'Choice2Of2' respectively.
  let partitionBy projection (array : _[]) =
    let left = new ResizeArray<_>(array.Length)
    let right = new ResizeArray<_>()
    for elem in array do
      match projection elem with
      | Choice1Of2 x -> left.Add x
      | Choice2Of2 x -> right.Add x
    toArray left, toArray right

  /// Splits the collection into two collections, containing the elements for
  /// which the given projection returns 'Ok' and 'Error' respectively.
  let partitionByResult projection (array : _[]) =
    let ok = new ResizeArray<_>(array.Length)
    let error = new ResizeArray<_>()
    for elem in array do
      match projection elem with
      | Ok x -> ok.Add x
      | Error x -> error.Add x
    toArray ok, toArray error

  let chunkBy condition (array: _ []) =
    let result = new ResizeArray<_>()
    let buffer = new ResizeArray<_>()
    for item in array do
      if condition item then
        result.Add(buffer.ToArray())
        buffer.Clear()
      else
        buffer.Add(item)
    if buffer.Count > 0 then
      result.Add(buffer.ToArray())
    result.ToArray()

  /// Splits the collection into chunks when the given condition returns 'true'.
  let chunkWhen condition (array: _ array) =
    let result = ResizeArray()
    let buffer = ResizeArray()
    for item in array do
      if buffer.Count = 0 || condition buffer[buffer.Count - 1] item then
        buffer.Add(item)
      else
        result.Add(buffer.ToArray())
        buffer.Clear()
        buffer.Add(item)
    if buffer.Count > 0 then
      result.Add(buffer.ToArray())
    result.ToArray()

  let chunkWhen2 condition (array: _ array) =
    if   array.Length = 0 then [||]
    elif array.Length = 1 then [| array |]
    else
      let result = ResizeArray()
      let buffer = ResizeArray()
      for item in array do
        if buffer.Count = 0 || condition buffer[buffer.Count - 1] item then
          buffer.Add(item)
        else
          result.Add(buffer.ToArray())
          buffer.Clear()
          buffer.Add(item)
      result.Add(buffer.ToArray())
      result.ToArray()

  let chunkWhen3 condition (array: _ array) =
    let result = ResizeArray()
    let buffer = ResizeArray()
    let mutable previous = Unchecked.defaultof<_>
    for item in array do
      if buffer.Count = 0 || condition previous item then
        buffer.Add(item)
        previous <- item
      else
        result.Add(buffer.ToArray())
        buffer.Clear()
        buffer.Add(item)
        previous <- item
    if buffer.Count > 0 then
      result.Add(buffer.ToArray())
    result.ToArray()

  let chunkWhen4 condition (array: _ array) =
    if   array.Length = 0 then [| |]
    elif array.Length = 1 then [| array |]
    else
      let result = ResizeArray()
      let mutable buffer = CompilerServices.ArrayCollector()
      let mutable first = true
      let mutable previous = Unchecked.defaultof<_>
      for item in array do
        if first || condition previous item then
          buffer.Add(item)
          previous <- item
          first <- false
        else
          result.Add(buffer.Close())
          buffer.Add(item)
          previous <- item
      result.Add(buffer.Close())
      result.ToArray()

  let sumBy2 f (array1: _ array) (array2: _ array) =
    if array1.Length <> array2.Length then
      invalidArg "array2" "Arrays must have the same length"

    let mutable sum = 0
    for i in 0 .. array1.Length - 1 do
      sum <- sum + f array1[i] array2[i]
    sum

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

[<Struct;CustomComparison;CustomEquality>]
[<StructuredFormatDisplay("({X}, {Y})")>]
type Point(x: int, y: int) =
  member _.X = x
  member _.Y = y

  static member (+) (p1: Point, p2: Point) = Point(p1.X + p2.X, p1.Y + p2.Y)
  static member (-) (p1: Point, p2: Point) = Point(p1.X - p2.X, p1.Y - p2.Y)
  static member (%) (p1: Point, p2: Point) = Point(p1.X % p2.X, p1.Y % p2.Y)
  static member (*) (p: Point, f: int) = Point(p.X * f, p.Y * f)
  static member (*) (f: int, p: Point) = Point(p.X * f, p.Y * f)

  override _.Equals(other: obj) =
    match other with
    | :? Point as other -> x = other.X && y = other.Y
    | _ -> false

  override _.GetHashCode() = HashCode.Combine(x, y)

  member _.CompareTo (other: Point) =
    let c1 = x.CompareTo(other.X)
    if c1 > 0 then
      c1
    else
      let c2 = y.CompareTo(other.Y)
      max c1 c2

  interface IComparable with
    member this.CompareTo (other: obj) =
      match other with
      | :? Point as other -> this.CompareTo(other)
      | _ -> failwith "Invalid type"
