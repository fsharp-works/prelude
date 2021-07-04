[<RequireQualifiedAccess>]
module FSharpWorks.Prelude.Result

/// Swaps Ok and Error values.
let inline swap source =
    match source with
    | Ok v -> Error v
    | Error v -> Ok v

let zip x1 x2 =
    match x1, x2 with
    | Ok x1res, Ok x2res -> Ok(x1res, x2res)
    | Error e, _ -> Error e
    | _, Error e -> Error e
