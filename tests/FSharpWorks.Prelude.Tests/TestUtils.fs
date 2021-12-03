[<RequireQualifiedAccess>]
module TestUtils

open System

type TestDisposable() =
    let mutable disposed = false
    member this.IsDisposed = disposed

    interface IDisposable with
        member this.Dispose() =
            disposed <- true
            ()


