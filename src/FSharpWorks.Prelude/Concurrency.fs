module FSharpWorks.Prelude.Concurrency

open System

let sleepMillis (n: int) = Async.Sleep(n) |> IO.ofAsync
let sleep (n: TimeSpan) = Async.Sleep(n) |> IO.ofAsync
