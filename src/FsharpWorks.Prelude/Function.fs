[<RequireQualifiedAccess>]
module FsharpWorks.Prelude.Function

let map f g = g >> f
let map2 f g = fun a b -> f (g a b)
let map3 f g = fun a b c -> f (g a b c)
