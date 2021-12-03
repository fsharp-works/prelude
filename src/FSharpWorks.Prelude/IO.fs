namespace FSharpWorks.Prelude

open System

/// A type that describes an effectful program.
///
/// The program requires some environment R,
/// and may fail with an error of type E,
/// or succeed with a value of type A
[<NoComparison; NoEquality; Struct>]
type IO<'R, 'A, 'E> = internal IO of ('R -> AsyncResult<'A, 'E>)

[<RequireQualifiedAccess>]
module IO =

    let unsafePerformIO env (IO f) = f env

    let RunAsync env (IO f) = f env

    let RunSynchronously env (IO f) = f env |> Async.RunSynchronously

    /// Wrap a given value into IO
    let returnM a = IO(fun _ -> AsyncResult.returnM a)

    let ofError a : IO<'r, 'a, 'e> = IO(fun _ -> AsyncResult.ofError a)
    let ofAsync a : IO<'r, 'a, 'e> = IO(fun _ -> AsyncResult.ofAsync a)
    let ofAsyncResult a : IO<'r, 'a, 'e> = IO(fun _ -> a)
    let ofOption e a : IO<'r, 'a, 'e> = IO(fun _ -> AsyncResult.ofOption e a)
    let ofOptionF e a : IO<'r, 'a, 'e> = IO(fun _ -> AsyncResult.ofOptionF e a)


    /// Lift a function into an IO action
    let lift (f: 'r -> 'a): IO<'r, 'a, 'e> = IO (f >> AsyncResult.returnM)

    /// Lift an effectful function into an IO action
    let liftUnit f = IO (fun _ -> f () |> AsyncResult.returnM)

    /// Lift a function that produces Async into an IO action
    let liftAsync f : IO<'r, 'a, 'e> = IO(f >> AsyncResult.ofAsync)

    /// Lift a function that produces AsyncResult into and IO action
    let liftAsyncResult f : IO<'r, 'a, 'e> = IO f

    /// Ignores the result and replaces it with unit
    let ignore (IO f) : IO<'r, unit, 'e> =
        IO(f >> AsyncResult.ignore)

    /// Transforms the result within IO
    let map (f: 'a -> 'b) (IO g) : IO<'r, 'b, 'e> =
        IO(g >> AsyncResult.map f)

    /// Transforms both result and error within IO
    let bimap (f: 'a -> 'b) (g: 'e -> 'e) (IO h: IO<'r, 'a, 'e>) : IO<'r, 'b, 'e> =
        IO(h >> AsyncResult.bimap f g)

    /// Transforms the error within IO
    let mapError f (IO g) : IO<'r, 'a, 'e> =
        IO(g >> AsyncResult.mapError f)

    let bind f m : IO<'r, 'b, 'e> =
        fun env ->
            unsafePerformIO env m
            |> AsyncResult.bind (f >> unsafePerformIO env)
        |> IO

    let delay f : IO<'r, 'a, 'e> =
        IO(fun env -> asyncResult.Delay(f >> unsafePerformIO env))

    /// Combines two actions by sequencing them and ignoring the first result
    let inline combine ma mb : IO<'r, 'b, 'e> =
        bind (fun _ -> mb) ma

    let foreach f (s: #seq<'T>) : IO<'r, unit, 'e> =
        Seq.fold (fun state t -> combine state (delay (fun () -> f t))) (returnM ()) s

    /// Repeat an IO action until 'guard' predicate allows
    let rec doWhile guard (body: IO<'r, unit, 'e>) =
        if not (guard ()) then
            returnM ()
        else
            bind (fun () -> doWhile guard body) body

    /// Zip two actions together by returning a function of both results
    let zipWith (f : 'a -> 'b -> 'c) (IO f1) (IO f2) : IO<'r, 'c, 'e> =
        fun env ->
            asyncResult {
                let! res1 = f1 env
                and! res2 = f2 env
                return f res1 res2
            }
        |> IO

    /// Zip two actions together by returning a tuple of both results
    let inline zip a b : IO<'r, 'a * 'b, 'e> =
        zipWith (fun x y -> (x, y)) a b

    /// Flattens nested IO actions
    let inline join m : IO<'r, 'a, 'e> = bind id m

    /// Applicative: given a function within IO and a value within IO
    /// apply that function to a value
    let inline ap m f: IO<'r, 'a, 'e>  = f |> bind (fun g -> map g m)

    /// Applicative: given a function within IO and a value within IO
    /// apply that function to a value
    let inline apply f a : IO<'r, 'b, 'e> = ap a f

    /// Returns environment value
    let ask : IO<'r, 'r, 'e> = IO AsyncResult.returnM

    /// Returns a function of an environment value
    let asks f = IO(f >> AsyncResult.returnM)

    /// Executes a computation in a modified environment
    let local f (IO g) = IO(f >> g)

    let inline ofSuccess a = returnM a

    let tryWith handler (a: IO<'r, 'a, 'e>) =
        fun env ->
            try
                unsafePerformIO env a
            with e -> unsafePerformIO env (handler e)
        |> IO

    let tryFinally cleanup (a: IO<'r, 'a, 'e>) =
        fun env ->
            try
                unsafePerformIO env a
            finally
                cleanup ()
        |> IO

    let using (f: _ -> IO<'r, 'b, 'e>) (resource: #IDisposable) =
        tryFinally (fun () -> resource.Dispose()) (f resource)

    let ofOptionWith e f a : IO<'r, 'b, 'e> =
        IO(fun _ -> AsyncResult.ofOptionWith e f a)

    let ofOptionWithF e f a : IO<'r, 'b, 'e> =
        IO(fun _ -> AsyncResult.ofOptionWithF e f a)

    /// Transforms list of actions into an action that returns list of all results
    let sequence (xs : IO<'r, 'a, 'e> list) : IO<'r, 'a list, 'e> =
        let inline f x xs' = ap xs' (map List.cons x)
        List.foldBack f xs (returnM [])

    /// Apply handlers to both result and error
    let either (f : 'a -> Result<'b,'e1>)
               (g : 'e -> Result<'b,'e1>)
               (IO h : IO<'r,'a,'e>) : IO<'r,'b,'e1> =
        IO(h >> AsyncResult.either f g)

[<AutoOpen>]
module IOComputationExpression =

    type IOBuilder() =
        member inline self.Return(x: 'a) : IO<'r, 'a, 'e> = IO.returnM x
        member inline self.Bind(x: IO<'r, _, 'e>, f) = IO.bind f x
        member inline self.Combine(a, b) = IO.combine a b
        member inline self.ReturnFrom(x: IO<'r, 'a, 'e>) = x

        member inline self.Yield(x: 'a) = IO.returnM x

        member inline self.Delay(f) : IO<'r, 'a, 'e> = IO.delay f

        member inline self.Zero() = IO.returnM ()

        member inline self.While(guard, body) : IO<'r, _, 'e> = IO.doWhile guard body

        member inline self.TryWith((a: IO<'r, 'a, 'e>), handler) = IO.tryWith handler a

        member inline self.TryFinally(f: IO<'r, 'a, 'e>, compensation) = IO.tryFinally compensation f

        member inline self.Using(disposable: 'T :> IDisposable, body: 'T -> IO<'r, 'a, 'e>) = IO.using body disposable

        member inline self.BindReturn(m: IO<'r, 'a, 'e>, f) = IO.map f m

        member inline self.MergeSources(m1: IO<'r, 'a, 'e>, m2: IO<'r, 'b, 'e>) = IO.zip m1 m2

        member inline self.For(sequence: #seq<'T>, body: 'T -> IO<'r, unit, 'e>) = IO.foreach body sequence

    let io = IOBuilder()
