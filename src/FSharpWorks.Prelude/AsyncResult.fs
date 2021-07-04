namespace FSharpWorks.Prelude

open System
open FSharpx
open FSharpWorks.Prelude

type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =

    let inline returnM x : AsyncResult<_, _> = x |> Result.Ok |> Async.returnM

    let inline swap (x: AsyncResult<_, _>) : AsyncResult<_, _> = x |> Async.map Result.swap

    let inline map f (x: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.map f) x

    let inline bimap onSuccess onError (a: AsyncResult<_, _>) : AsyncResult<_, _> =
        Async.map (Result.bimap onSuccess onError) a

    let inline mapError f (a: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.mapError f) a

    let inline ignore a = a |> map ignore

    let ap (a: AsyncResult<_, _>) (f: AsyncResult<_, _>) : AsyncResult<_, _> =
        a
        |> Async.bind (fun res -> f |> Async.map (Result.ap res))

    let inline apply f a = ap a f

    let bind (f: 'a -> AsyncResult<'b, 'c>) (a: AsyncResult<_, _>) : AsyncResult<_, _> =
        async {
            match! a with
            | Ok x -> return! f x
            | Error err -> return (Error err)
        }

    let catch f (x: AsyncResult<_, _>) : AsyncResult<_, _> =
        x
        |> Async.Catch
        |> Async.map
            (function
            | Choice1Of2 (Ok v) -> Ok v
            | Choice1Of2 (Error err) -> Result.Error err
            | Choice2Of2 ex -> Error(f ex))

    /// Convert a list of AsyncResult into a AsyncResult<list> using monadic style.
    /// Only the first error is returned. The error type need not be a list.
    let sequence xs =
        let f x xs' = ap xs' (map List.cons x)
        List.foldBack f xs (returnM [])

    let zip x1 x2 =
        Async.zip x1 x2
        |> Async.map (fun (r1, r2) -> Result.zip r1 r2)


    let inline ofOk x : AsyncResult<_, _> = Ok x |> Async.returnM

    let inline ofError x : AsyncResult<_, _> = Error x |> Async.returnM

    let inline ofResult x : AsyncResult<_, _> = Async.returnM x

    let inline ofAsync x : AsyncResult<_, _> = x |> Async.map Ok

    let inline ofOption e x = x |> Result.ofOption e |> ofResult

    let inline ofOptionF e x = x |> Result.ofOptionF e |> ofResult

    let inline ofOptionWith e f x = x |> Option.map f |> ofOption e

    let inline ofOptionWithF e f x = x |> Option.map f |> ofOptionF e

    let inline hoist f x = f x |> ofResult

    let inline hoist2 f x y = f x y |> ofResult

    let inline either f g (x: AsyncResult<_, _>) =
        x |> Async.map (fun r -> r |> Result.either f g)


[<AutoOpen>]
module AsyncResultComputationExpression =

    type AsyncResultBuilder() =
        member inline self.Return(x) = AsyncResult.returnM x

        member inline self.Bind(x, f) = AsyncResult.bind f x

        member self.Combine(a, b) = self.Bind(a, (fun () -> b ()))

        member inline self.ReturnFrom(x: AsyncResult<_, _>) = async.ReturnFrom(x)

        member self.Zero() = self.Return()

        member self.Yield(x) = AsyncResult.returnM x

        member self.Delay(f: unit -> AsyncResult<_, _>) : AsyncResult<_, _> = async.Delay f

        member self.While(guard, body) : AsyncResult<_, _> =
            if not (guard ()) then
                self.Zero()
            else
                self.Bind(body, (fun () -> self.While(guard, body)))

        member inline self.TryWith(body, handler) : AsyncResult<_, _> = async.TryWith(body, handler)

        member inline self.TryFinally(body, compensation) : AsyncResult<_, _> = async.TryFinally(body, compensation)

        member self.Using(disposable: 'T :> IDisposable, body: 'T -> AsyncResult<_, _>) : AsyncResult<_, _> = async.Using(disposable, body)

        member self.For(sequence: #seq<'T>, body: 'T -> AsyncResult<unit, 'TError>) : AsyncResult<unit, 'TError> =
            self.Using(sequence.GetEnumerator(), (fun enum -> self.While(enum.MoveNext, self.Delay(fun () -> body enum.Current))))

        member inline self.BindReturn(x: AsyncResult<_, _>, f) = AsyncResult.map f x

        member inline self.MergeSources(t1: AsyncResult<_, _>, t2: AsyncResult<_, _>) = AsyncResult.zip t1 t2

    let asyncResult = AsyncResultBuilder()
