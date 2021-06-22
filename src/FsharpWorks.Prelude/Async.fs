[<RequireQualifiedAccess>]
module FsharpWorks.Prelude.Async

open System.Threading.Tasks
open FSharpx

let awaitAny xs =
    xs
    |> Seq.map Async.StartAsTask
    |> Seq.toArray
    |> Task.WhenAny
    |> Async.AwaitTask
    |> Async.bind Async.AwaitTask
