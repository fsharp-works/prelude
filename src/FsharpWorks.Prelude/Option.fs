[<RequireQualifiedAccess>]
module FsharpWorks.Prelude.Option

let traverseResult f x =
    match x |> Option.map f with
    | None -> Ok None
    | Some (Ok v) -> Ok(Some v)
    | Some (Error e) -> Error e

let inline sequenceResult x = traverseResult id x
