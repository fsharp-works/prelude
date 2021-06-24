[<RequireQualifiedAccess>]
module FSharpWorks.Prelude.Result

/// Swaps Ok and Error values.
let inline swap source =
    match source with
    | Ok v -> Error v
    | Error v -> Ok v
