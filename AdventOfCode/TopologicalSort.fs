module TopologicalSort

open System
open System.Collections.Generic

type TopologicalSorter(size) =

  let mutable numVerts = 0
  let vertices = Array.zeroCreate size
  let matrix = Array2D.zeroCreate size size

  let moveRowUp(row, length) =
    for col = 0 to length - 1 do
      matrix.[row, col] <- matrix.[row + 1, col]

  let moveColLeft(col, length) =
    for row = 0 to length - 1 do
      matrix.[row, col] <- matrix.[row, col + 1]

  let noSuccessors() =
    let mutable idx = -1
    for row = 0 to numVerts - 1 do
      let mutable isEdge = false
      if idx = -1 then
        for col = 0 to numVerts - 1 do
          if matrix.[row, col] > 0 then
            isEdge <- true
        if not isEdge  then
          idx <- row
    idx

  let deleteVertex delVert =
    if delVert <> numVerts - 1 then
      for j   = delVert to numVerts - 2 do vertices.[j] <- vertices.[j + 1]
      for row = delVert to numVerts - 2 do moveRowUp(row, numVerts)
      for col = delVert to numVerts - 2 do moveColLeft(col, numVerts - 1);
    numVerts <- numVerts - 1

  member __.NumVerts = numVerts
  member __.Vertices = vertices
  member __.Matrix = matrix

  member __.AddVertex vertex =
    vertices.[numVerts] <- vertex
    numVerts <- numVerts + 1
    numVerts - 1

  member __.AddEdge(start, end') =
    if matrix.[start, end'] > 0 then failwith "Edge already added"
    matrix.[start, end'] <- 1

  member __.Sort() =
    for i = 0 to size - 1 do
      matrix.[i, i] <- 0
    let sortedArray = Array.zeroCreate size
    while numVerts > 0 do
      let currentVertex = noSuccessors()
      if currentVertex = -1 then failwith "Graph has cycles"
      sortedArray.[numVerts - 1] <- vertices.[currentVertex]
      deleteVertex(currentVertex)
    sortedArray

type Node<'T> =
  { Name : 'T
    Dependencies : 'T [] }
  override this.ToString() =
    let deps =
      match this.Dependencies with
      | [||] -> "No dependencies"
      | deps -> String.Join(", ", deps)
    $"{this.Name}: {deps}"
  static member Primitive name = { Name = name ; Dependencies = Array.Empty() }

let getTopologicalSortOrder(nodes : Node<_>[]) =
  let sorter = new TopologicalSorter(nodes.Length)

  let mutable vertexCounter = 0
  let indexes = new Dictionary<string, int>()
  for node in nodes do
    if indexes.ContainsKey node.Name then
      failwithf "A node with the name %s has already been added" node.Name
    indexes.Add(node.Name, sorter.AddVertex vertexCounter)
    vertexCounter <- vertexCounter + 1

  for vertex = 0 to nodes.Length - 1 do
    for d in nodes.[vertex].Dependencies do
      let end' =
        match indexes.TryGetValue d with
        | true, value -> value
        | false, _ -> failwithf "The dependency '%s' could not be found" d
      sorter.AddEdge(vertex, end')
  sorter.Sort()
  |> Array.map(fun i -> nodes.[i])

let getTopologicalSortOrderWith nameSelector dependenciesSelector nodes =
  let sorter = new TopologicalSorter(Array.length nodes)

  let mutable vertexCounter = 0
  let indexes = new Dictionary<_, int>()
  for node in nodes do
    let name = nameSelector node
    if indexes.ContainsKey name then
      failwithf $"A node with the name {name} has already been added"
    indexes.Add(name, sorter.AddVertex vertexCounter)
    vertexCounter <- vertexCounter + 1

  for vertex = 0 to nodes.Length - 1 do
    for d in dependenciesSelector nodes.[vertex] do
      let end' =
        match indexes.TryGetValue d with
        | true, value -> value
        | false, _ -> failwithf $"The dependency '{d}' could not be found"
      sorter.AddEdge(vertex, end')
  sorter.Sort()
  |> Array.map(fun i -> nodes.[i])

// https://codereview.stackexchange.com/questions/129244/cyclic-dependency-detection-in-javascript
let findCyclicDependencies initialIdentifier definitions =
  let seen = new HashSet<_>()
  let rec search identifier =
    if seen.Add(identifier) then
      let found = definitions |> Map.find identifier |> Array.exists search
      if not found then
        seen.Remove(identifier) |> ignore
      found
    else identifier = initialIdentifier

  search initialIdentifier |> ignore
  seen |> Seq.toArray
