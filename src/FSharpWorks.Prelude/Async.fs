[<RequireQualifiedAccess>]
module FSharpWorks.Prelude.Async

open System.Threading.Tasks
open FSharpx

let awaitAny xs =
    xs
    |> Seq.map Async.StartAsTask
    |> Seq.toArray
    |> Task.WhenAny
    |> Async.AwaitTask
    |> Async.bind Async.AwaitTask

let inline ofOption z f = Option.option (Async.returnM z) f

let inline ofOptionF z f = Option.option ( async { return z () }) f
