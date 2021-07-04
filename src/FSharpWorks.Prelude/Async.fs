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

let zip a1 a2 =
    async {
        let! r1 = a1
        let! r2 = a2
        return r1, r2
    }

let inline ofOption z f = Option.option (Async.returnM z) f

let inline ofOptionF z f = Option.option (async { return z () }) f
