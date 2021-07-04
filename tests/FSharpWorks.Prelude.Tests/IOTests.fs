module IOTests

open System
open Hedgehog.Xunit
open FsUnit.CustomMatchers
open FsUnit.Xunit
open FSharpWorks.Prelude

let (>>=) m f = IO.bind f m

let runIO<'err> z m: Result<_, 'err> =
    m |> IO.RunSynchronously z

[<Property>]
let ``Left identity`` (a: int, b: int) =
    let k x = IO.ask |> IO.map (fun z -> z + x)

    io {
        let! res1 = IO.returnM a >>= k

        let! res2 = k a |> IO.mapError (fun () -> ())
        return res1 |> should equal res2
    } |> runIO b


[<Property>]
let ``Right identity`` (a: int) =
    io {
        let! res1 = IO.ask >>= IO.returnM
        let! res2 = IO.ask
        res1 |> should equal res2
    } |> IO.RunSynchronously a |> ignore

[<Property>]
let ``Monadic let`` (a: int, b: int) =
    io {
        let! x = IO.returnM a
        let y = b

        (x + y) |> should equal (a + b)
    } |> IO.RunSynchronously() |> ignore

[<Property>]
let ``Applicative IO`` (a: int, b: int) =
    let x = IO.returnM a
    let y = IO.returnM b
    let res = x
              |> IO.map (+)
              |> IO.ap y
              |> IO.RunSynchronously ()
    res |> should equal (Ok (a + b))

[<Property>]
let ``Reader ask`` (a: int, b: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.ask
        (x + y) |> should equal (a + b)
    } |> IO.RunSynchronously b |> ignore

[<Property>]
let ``Reader asks`` (a: int, b: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.asks (fun z -> z * 2)
        (x + y) |> should equal (a + (b * 2))
    } |> IO.RunSynchronously b |> ignore


[<Property>]
let ``Reader local`` (a: int, b: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.local (fun z -> z * 2) IO.ask
        (x + y) |> should equal (a + (b * 2))
    } |> IO.RunSynchronously b |> ignore

