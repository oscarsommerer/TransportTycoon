open System
open Deedle
open MathNet.Numerics
open Deedle

let normalizedTime (t: DateTime) = (t.DayOfWeek |> float) 

let evaluate model =
    let dfTest = Frame.ReadCsv("/workspaces/transport tycoon/23/s02e03_test.csv")

    dfTest?prediction <-
        dfTest.Rows 
        |> Series.mapValues (fun row -> model row)
    dfTest?error <-
        dfTest.Rows
        |> Series.mapValues (fun row -> Math.Pow(row?SPEED - row?prediction, 2))
    let mse = dfTest?error.Sum() / (double dfTest.RowCount)
    dfTest, mse

let toPoly (f: Frame<int, string>) =
    let xdata =
        f
        |> Frame.mapRows (fun _ row -> row.GetAs<DateTime>("TIME") |> normalizedTime)
        |> Series.values
        |> Seq.toArray

    let ydata = f?SPEED |> Series.values |> Seq.toArray

    Fit.Polynomial(xdata, ydata, 2)

let model = fun (parameters: Frame<string, string>) (r: ObjectSeries<string>) ->
    let a = r.GetAs<string>("A")
    let b = r.GetAs<string>("B")
    let parameters = parameters.[a, b] :?> double array

    let day = r.GetAs<DateTime>("TIME") |> normalizedTime

    parameters
    |> Array.mapi (fun i p -> (i, p))
    |> Array.fold (fun s (i, p) -> 
            s + (p * Math.Pow(day, i))
        ) 0.0


[<EntryPoint>]
let main argv =
    let modelParameters = 
        Frame.ReadCsv("/workspaces/transport tycoon/23/s02e03_train.csv")
        |> Frame.sortRowsBy "TIME" (fun v -> v)
        |> Frame.pivotTable 
            (fun k r -> r.GetAs<string>("A"))
            (fun k r -> r.GetAs<string>("B"))
            toPoly

    let _, mse = evaluate (model modelParameters)

    Console.WriteLine($"MSE is {mse}")

    0