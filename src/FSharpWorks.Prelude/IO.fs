namespace FSharpWorks.Prelude

open System

[<NoComparison; NoEquality; Struct>]
type IO<'Env, 'Res, 'Err> =
    internal IO of ('Env -> AsyncResult<'Res, 'Err>)

[<RequireQualifiedAccess>]
module IO =

    let unsafePerformIO env (IO f) = f env

    let RunAsync env (IO f) = f env

    let RunSynchronously env (IO f) = f env |> Async.RunSynchronously

    let returnM a = IO(fun _ -> AsyncResult.returnM a)

    let ignore (IO f) = IO (f >> AsyncResult.ignore)


    let map f (IO g) = IO (g >> AsyncResult.map f)

    let bimap f g (IO h) = IO (h >> AsyncResult.bimap f g)

    let mapError f (IO g) = IO (g >> AsyncResult.mapError f)

    let bind f m =
        fun env ->
                unsafePerformIO env m
                |> AsyncResult.bind (f >> unsafePerformIO env)
        |> IO

    let zip (IO f1) (IO f2) =
        fun env ->
            asyncResult {
                let! res1 = f1 env
                and! res2 = f2 env
                return (res1, res2)
            }
        |> IO


    let inline join m = bind id m

    let ap m f = f |> bind (fun g -> map g m)

    let inline apply f a = ap a f

    let ask = IO AsyncResult.returnM

    let asks f = IO(f >> AsyncResult.returnM)

    let local f (IO g) = IO(f >> g)

    let inline ofSuccess a = returnM a
    let ofError a = IO(fun _ -> AsyncResult.ofError a)
    let ofAsync a = IO(fun _ -> AsyncResult.ofAsync a)
    let ofAsyncResult a = IO(fun _ -> a)
    let ofOption e a = IO(fun _ -> AsyncResult.ofOption e a)
    let ofOptionF e a = IO(fun _ -> AsyncResult.ofOptionF e a)

    let ofOptionWith e f a =
        IO(fun _ -> AsyncResult.ofOptionWith e f a)

    let ofOptionWithF e f a =
        IO(fun _ -> AsyncResult.ofOptionWithF e f a)

    let sequence xs =
        let inline f x xs' = ap xs' (map List.cons x)
        List.foldBack f xs (returnM [])

    let either f g (IO h) = IO(h >> AsyncResult.either f g)


[<AutoOpen>]
module IOComputationExpression =

    type IOBuilder() =
        member inline self.Return(x: 'a): IO<'r,'a,'e> = IO.returnM x
        member inline self.Bind(x, f) = IO.bind f x
        member inline self.Combine(a, b) = self.Bind(a, (fun () -> b ()))
        member inline self.ReturnFrom(x: IO<_, _, _>) = x

        member inline self.Yield(x) = IO.returnM x

        member self.Delay(f: unit -> IO<'env, 'res, 'err>): IO<'env, 'res, 'err> =
            IO(fun env -> asyncResult.Delay(f >> IO.unsafePerformIO env))

        member inline self.Zero() = IO.returnM ()

        member self.While(guard, body): IO<'env, _, 'err> =
            if not (guard ()) then
                self.Zero()
            else
                self.Bind(body, (fun () -> self.While(guard, body)))

        member self.TryWith(f, handler) =
            fun env ->
                try
                    IO.unsafePerformIO env (self.ReturnFrom(f ()))
                with e -> IO.unsafePerformIO env (handler e)
            |> IO

        member self.TryFinally(f, compensation) =
            fun env ->
                try
                    IO.unsafePerformIO env (self.ReturnFrom(f ()))
                finally
                    compensation ()
            |> IO

        member self.Using(disposable: 'T :> IDisposable, body: 'T -> IO<_, _, _>) =
            self.TryFinally(
                (fun () -> body disposable),
                fun () ->
                    match disposable with
                    | null -> ()
                    | disp -> disp.Dispose()
            )

        member inline self.BindReturn(m, f) = IO.map f m

        member inline self.MergeSources(m1, m2) = IO.zip m1 m2

        member self.For(sequence: #seq<'T>, body: 'T -> IO<_, unit, 'TError>) : IO<_, unit, 'TError> =
            self.Using(sequence.GetEnumerator(), (fun enum -> self.While(enum.MoveNext, self.Delay(fun () -> body enum.Current))))

    let io = IOBuilder()
