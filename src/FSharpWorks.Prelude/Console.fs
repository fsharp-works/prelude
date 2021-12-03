module FSharpWorks.Prelude.Console

let putStrLn format = IO.liftUnit (fun () -> printfn format)
let getStrLn () = IO.liftUnit (fun () -> System.Console.ReadLine())
