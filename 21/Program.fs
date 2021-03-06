open System
open FSharp.Data
open FSharpx.Collections

type Edge = (Decimal * string * string)

let toTuples (rows: CsvRow seq) = rows |> Seq.map(fun r -> r.["km"].AsDecimal(), r.["A"], r.["B"])

let addEdge (d, a, b) graph =
    graph
    |> Map.tryFind a
    |> Option.defaultValue []
    |> List.append [(d, b, a)]
    |> Map.add a <| graph

// From tuples to map of nodes and bi-directional edges
let toGraph (tuples) =
    tuples
    |> Seq.fold (fun g (_, a, b) -> g |> Map.add a [] |> Map.add b []) Map.empty
    |> Seq.fold (fun g (d, a, b) -> g |> addEdge (d, a, b) |> addEdge (d, b, a)) <| tuples

let addToFrontier (distance, node, parent) explored (frontier: IPriorityQueue<Edge>) =
    let nodeInFrontier = Heap.toSeq (frontier :?> Heap<Edge>) |> Seq.fold (fun s (_, b, _) -> s || node = b) false
    if not ((Set.contains node explored) || nodeInFrontier) then
        PriorityQueue.insert (distance, node, parent) frontier
    elif nodeInFrontier then
        Heap.toSeq (frontier :?> Heap<Edge>) 
        |> Seq.map (fun (d, b, p) -> if node = b && distance < d then (distance, b, parent) else (d, b, p)) 
        |> Heap.ofSeq false :> IPriorityQueue<Edge>
    else
        frontier

// simplified uniform cost search
let rec find dest graph (frontier: IPriorityQueue<Edge>) explored path =
    if frontier.IsEmpty then
        None
    else
        let (distance, node, parent), frontier' = PriorityQueue.pop frontier

        if node = dest then
            Some ([(node, parent)] @ path)
        else 
            let explored' = Set.add node explored
            let frontier'' = graph |> Map.find node |> List.fold (fun s (d, a, b) -> addToFrontier (distance + d, a, b) explored' s) frontier'
            find dest graph frontier'' explored' ([(node, parent)] @ path)

// follow traversed paths from destination to source to find the optimal solution
let getSolution path =
    let traversedPaths = path |> List.fold (fun s (a, b) -> Map.add a b s) Map.empty
    let rec next a =
        match Map.tryFind a traversedPaths with
        | None -> []
        | Some b -> a :: next b
    
    path 
    |> List.head
    |> fst
    |> next
    |> List.rev

[<EntryPoint>]
let main argv =
    let graph = CsvFile.Load("network.csv").Rows
                |> toTuples
                |> toGraph

    let frontier = PriorityQueue.empty false |> PriorityQueue.insert (Decimal.Zero, argv.[0], "")
    find argv.[1] graph frontier Set.empty [] |> Option.defaultValue List.empty |> getSolution |> printfn "%A"
    0