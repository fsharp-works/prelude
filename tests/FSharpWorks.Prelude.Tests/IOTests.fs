module IOTests

open System
open Hedgehog
open Hedgehog.Xunit
open FsUnit.Xunit
open FSharpWorks.Prelude
open FsUnitTyped

open Console
open Concurrency

let (>>=) m f = IO.bind f m

[<Property(2<tests>)>]
let ``Example test``(greet: string) =
    // Assuming that we do have some IO actions
    let getTime = IO.liftUnit (fun () -> DateTime.Now)

    // We could write programs like
    io {
        // get an environment that is going to be provided when the program runs
        let! env = IO.ask
        do! putStrLn $"Hello, {env}"

        let! t1 = getTime
        do! sleepMillis 100
        let! t2 = getTime
        do! putStrLn $"I slept from {t1} till {t2}"
    } |> IO.RunSynchronously greet


[<Property>]
let ``Left identity`` (env: int, a: int) =
    let k x = IO.ask |> IO.map ((+) x)

    io {
        let! res1 = IO.returnM a >>= k
        let! res2 = k a
        return res1 |> should equal res2
    } |> IO.RunSynchronously env


[<Property>]
let ``Right identity`` (env: int) =
    io {
        let! res1 = IO.ask >>= IO.returnM
        let! res2 = IO.ask
        return res1 |> should equal res2
    } |> IO.RunSynchronously env

[<Property>]
let ``Monadic let`` (env: int, a: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.ask

        return (x + y) |> should equal (env + a)
    } |> IO.RunSynchronously env

[<Property>]
let ``Applicative IO`` (env: int, a: int) =
    let x = IO.returnM a
    let y = IO.returnM env
    let res = x
              |> IO.map (+)
              |> IO.ap y
              |> IO.RunSynchronously ()
    res |> should equal (Ok (env + a))

[<Property>]
let ``Applicative Syntax`` (env: int, a: int, b: int) =
    io {
        let! x = IO.ask
        and! y = IO.returnM a
        and! z = IO.returnM b
        return (x + y * z) |> should equal (env + a * b)
    } |> IO.RunSynchronously env

[<Property>]
let ``Reader ask`` (env: int, a: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.ask
        return (x + y) |> should equal (env + a)
    } |> IO.RunSynchronously env

[<Property>]
let ``Reader asks`` (env: int, a: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.asks ((*) 2)
        return (x + y) |> should equal (a + (env * 2))
    } |> IO.RunSynchronously env


[<Property>]
let ``Reader local`` (env: int, a: int) =
    io {
        let! x = IO.returnM a
        let! y = IO.local ((*) 2) IO.ask
        return (x + y) |> should equal (a + (env * 2))
    } |> IO.RunSynchronously env


[<Property>]
let ``Using`` (env: int, a: int) =
    io {
        use value = new TestUtils.TestDisposable()
        return value
    }
    |> IO.RunSynchronously env
    |> Result.map (fun r -> r.IsDisposed)
    |> shouldEqual (Ok true)

[<Property>]
let ``For`` (env: int16 list) =
    let mutable sum = 0
    let res =
        io {
            let! values = IO.ask
            for i in values do
                sum <- sum + int32 i
            return ()
        } |> IO.RunSynchronously env
    sum |> shouldEqual (env |> Seq.map int32 |> Seq.sum)
