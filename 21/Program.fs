open System
open FSharp.Data
open FSharpx.Collections

let toTuples (rows: CsvRow seq) = rows |> Seq.map(fun r -> r.["km"].AsDecimal(), r.["A"], r.["B"])

let addEdge (d, a, b) graph =
    graph
    |> Map.tryFind a
    |> Option.defaultValue []
    |> List.append [(d, b, a)]
    |> Map.add a <| graph

let toGraph (tuples) =
    tuples
    |> Seq.fold (fun g (_, a, b) -> g |> Map.add a [] |> Map.add b []) Map.empty
    |> Seq.fold (fun g (d, a, b) -> g |> addEdge (d, a, b) |> addEdge (d, b, a)) <| tuples

let addToFrontier (distance, node, parent) explored (frontier: IPriorityQueue<'B * 'A * 'A>) =
    let nodeInFrontier = Heap.toSeq (frontier :?> Heap<'B * 'A * 'A>) |> Seq.fold (fun s (_, b, _) -> s || node = b) false
    if not ((Set.contains node explored) || nodeInFrontier) then
        PriorityQueue.insert (distance, node, parent) frontier
    elif nodeInFrontier then
        Heap.toSeq (frontier :?> Heap<'B * 'A * 'A>) |> Seq.map (fun (d, b, p) -> if node = b && distance < d then (distance, b, parent) else (d, b, p)) |> Heap.ofSeq false :> IPriorityQueue<'B * 'A * 'A>
    else
        frontier

let rec find dest graph frontier explored path =
    let (_, node, parent), frontier' = PriorityQueue.pop frontier

    if frontier.IsEmpty then
        None
    elif node = dest then
        Some (path @ [(node, parent)])
    else 
        let explored' = Set.add node explored
        let frontier'' = Map.find node graph |> List.fold (fun s n -> addToFrontier n explored' s) frontier'
        find dest graph frontier'' explored' (path @ [(node, parent)])

let getSolution path =
    let traversedPaths = path |> List.fold (fun s (a, b) -> Map.add a b s) Map.empty
    let rec next a =
        match Map.tryFind a traversedPaths with
        | None -> []
        | Some b -> a :: next b
    
    path 
    |> List.rev
    |> List.head
    |> fst
    |> next
    |> List.rev

[<EntryPoint>]
let main argv =
    let graph = CsvFile.Load("network.csv").Rows
                |> toTuples
                |> toGraph

    let frontier = PriorityQueue.empty false
                   |> PriorityQueue.insert (Decimal.Zero, argv.[0], "")
    find argv.[1] graph frontier Set.empty [] |> Option.defaultValue List.empty |> getSolution |> printfn "%A"
    0