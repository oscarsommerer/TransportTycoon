namespace TransportTycoon

open System
open FSharp.Data
open FSharpx.Collections

module Model =
    type Edge = (Decimal * string * string)

    type Road = {
        A: string
        B: string
        Km: Decimal
        Speed: Decimal
    }

    let inverse road = {road with A = road.B; B = road.A}

    let toTuples (rows: CsvRow seq) = rows |> Seq.map(fun r -> (r.["km"].AsDecimal() / r.["speed"].AsDecimal()), r.["A"], r.["B"])
    
    let toRoads (rows: CsvRow seq) = rows |> Seq.map(fun r -> {A = r.["A"]; B = r.["B"]; Km = r.["km"].AsDecimal(); Speed = r.["speed"].AsDecimal()})

    let addEdge ofNode e graph =
        graph
        |> Map.tryFind ofNode
        |> Option.defaultValue []
        |> List.append [e]
        |> Map.add ofNode <| graph

    let toGraph (tuples) =
        tuples
        |> Seq.fold (fun g (_, a, b) -> g |> Map.add a [] |> Map.add b []) Map.empty
        |> Seq.fold (fun g (d, a, b) -> g |> addEdge a (d, b, a) |> addEdge b (d, a, b)) <| tuples

    let toRoadNetwork (tuples) =
        tuples
        |> Seq.fold (fun g r -> g |> Map.add r.A [] |> Map.add r.B []) Map.empty
        |> Seq.fold (fun g r -> g |> addEdge r.A r |> addEdge r.B (inverse r)) <| tuples

module Solution =
    open Model
    let addToFrontier (cost, node, parent) explored (frontier: IPriorityQueue<Edge>) =
        let nodeInFrontier = Heap.toSeq (frontier :?> Heap<Edge>) |> Seq.fold (fun s (_, b, _) -> s || node = b) false
        if not ((Set.contains node explored) || nodeInFrontier) then
            PriorityQueue.insert (cost, node, parent) frontier
        elif nodeInFrontier then
            Heap.toSeq (frontier :?> Heap<Edge>) 
            |> Seq.map (fun (d, b, p) -> if node = b && cost < d then (cost, b, parent) else (d, b, p)) 
            |> Heap.ofSeq false :> IPriorityQueue<Edge>
        else
            frontier

    let rec _find dest graph (frontier: IPriorityQueue<Edge>) explored path =
        if frontier.IsEmpty then
            None
        else
            let (distance, node, parent), frontier' = PriorityQueue.pop frontier

            if node = dest then
                Some ([(node, parent)] @ path)
            else 
                let explored' = Set.add node explored
                let frontier'' = graph |> Map.find node |> List.fold (fun s (d, a, b) -> addToFrontier (distance + d, a, b) explored' s) frontier'
                _find dest graph frontier'' explored' ([(node, parent)] @ path)

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

    // simplified uniform cost search
    let find source dest graph =
        let frontier = PriorityQueue.empty false |> PriorityQueue.insert (Decimal.Zero, source, "")
        _find dest graph frontier Set.empty [] |> Option.defaultValue List.empty |> getSolution

module ETA =
    open Model
    let get roads path =
        path
        |> List.windowed 2
        |> List.map (fun [a; b] ->
            let r = Map.find a roads |> List.find (fun r -> r.B = b)
            let time = r.Km / r.Speed
            (r, time)
        )

module Program =
    open Model
    let print (solution: (Road * decimal) list) =
        solution
        |> List.fold (fun s (r, eta) ->
            match s with
            | 0m ->
                printfn $" 0.00h\tDEPART\t{r.A}"
                printfn $"{eta:F2}h\tARRIVE\t{r.B}"
                eta
            | sum ->
                let waypointTime = sum + eta
                printfn $"{waypointTime:F2}h\tARRIVE\t{r.B}"
                waypointTime
        ) Decimal.Zero
        |> ignore

    [<EntryPoint>]
    let main argv =
        let rows = CsvFile.Load("network.csv").Rows
        let graph = rows |> Model.toTuples |> Model.toGraph
        let network = rows |> Model.toRoads |> Model.toRoadNetwork

        Solution.find argv.[0] argv.[1] graph |> ETA.get network |> print
        0