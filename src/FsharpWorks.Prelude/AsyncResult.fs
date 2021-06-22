namespace FSharpCore.Prelude

open FSharpx
open FsharpWorks.Prelude

type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =

    let inline returnM x: AsyncResult<_, _> = x |> Result.Ok |> Async.returnM

    let inline swap (x: AsyncResult<_, _>): AsyncResult<_, _> = x |> Async.map Result.swap

    let inline map f (x: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.map f) x

    let inline bimap onSuccess onError (a: AsyncResult<_, _>): AsyncResult<_, _> =
        Async.map (Result.bimap onSuccess onError) a

    let inline mapError f (a: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.mapError f) a

    let inline ignore a = a |> map ignore

    let ap (a: AsyncResult<_, _>) (f: AsyncResult<_, _>) : AsyncResult<_, _> =
        a
        |> Async.bind (fun res -> f |> Async.map (Result.ap res))

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
    let sequence resultList =
        let (<*>) = flip ap
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR

        List.foldBack consR resultList (returnM [])

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
        member self.Return(x) = AsyncResult.returnM x

        member self.Bind(x, f) = AsyncResult.bind f x

        member self.ReturnFrom(x) = x

        member self.Zero() = self.Return()

        member self.Delay(f) = f

        member self.Run(f) = f ()

        member self.While(guard, body) =
            if not (guard ()) then
                self.Zero()
            else
                self.Bind(body (), (fun () -> self.While(guard, body)))

        member self.TryWith(body, handler) =
            try
                self.ReturnFrom(body ())
            with e -> handler e

        member self.TryFinally(body, compensation) =
            try
                self.ReturnFrom(body ())
            finally
                compensation ()

        member self.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable

            self.TryFinally(
                body',
                fun () ->
                    match disposable with
                    | null -> ()
                    | disposable -> disposable.Dispose()
            )

        member self.For(sequence: seq<_>, body) =
            self.Using(sequence.GetEnumerator(), (fun enum -> self.While(enum.MoveNext, self.Delay(fun () -> body enum.Current))))


    let asyncResult = AsyncResultBuilder()
