[<RequireQualifiedAccess>]
module FSharpWorks.Prelude.AsyncSeq

open FSharp.Control
open FSharpx

/// For each element of the sequence, executes a side-effect function then returns the original input value.
/// Consider this as 'tapping into' a stream.
let tapAsync f source =
    source
    |> AsyncSeq.mapAsync (fun x -> f x |> Async.map (fun _ -> x))

/// For each value in a sequence runs a computation
/// and returns a sequence of both original value and a result of that computation
let aside f source =
    source
    |> AsyncSeq.map (fun x -> (x, f x))

/// For each value in a sequence runs an async computation
/// and return a sequence of both original value and a result of that computation
let asideAsync (f: 'a -> Async<'b>) source =
    source
    |> AsyncSeq.mapAsync (fun x -> f x |> Async.map (tuple2 x))
