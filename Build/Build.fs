open Fake.Api
open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.BuildServer

open Fake.Tools
open Helpers
open Newtonsoft.Json

initializeContext()

BuildServer.install [ GitHubActions.Installer ]

let publishDir = "./.publish"
let srcDir = System.IO.DirectoryInfo("./src")

Target.create "Format" (fun _ ->
    run dotnet "fantomas . -r" "src"
)

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs

    [ ".publish" ]
    |> Shell.deleteDirs
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Tests" (fun _ ->
    !! "tests/**/*.*proj"
    |> Seq.iter (DotNet.test id)
)

Target.create "Test" ignore
"Tests" ==> "Test" |> ignore

let publishProjectPath prj =
    prj
    |> Path.toRelativeFrom (srcDir.FullName)
    |> Path.getDirectory
    |> Path.combine publishDir

Target.create "Publish" (fun _ ->
    Shell.cleanDir publishDir
    !! "src/**/*.App.*proj"
    |> Seq.iter (fun src ->
        DotNet.publish(fun x ->
            { x with
                NoLogo        = true
                NoRestore     = true
                OutputPath    = Some (publishProjectPath src)
                Configuration = DotNet.BuildConfiguration.Release
            }
          ) src
        )
)

Target.create "GitHubRelease" (fun _ ->
    if not (GitHubActions.detect()) then failwith "Github release can only be called by GitHub Action"

    let token = Environment.environVarOrFail("GITHUB_TOKEN")
    let [owner; repo] = GitHubActions.Environment.Repository |> String.split '/'

    let result = dotnet "gitversion" "."
                |> CreateProcess.redirectOutput
                |> CreateProcess.mapResult (fun x -> x.Output)
                |> Proc.run

    let version = JsonConvert.DeserializeObject<GitVersion.GitVersionProperties>(result.Result)

    GitHub.createClientWithToken token
       |> GitHub.draftNewRelease owner repo version.MajorMinorPatch false []
       // |> GitHub.uploadFiles files
       |> GitHub.publishDraft
       |> Async.RunSynchronously
)

Target.create "All" ignore

"Clean" ==> "Build" ==> "All" |> ignore

[<EntryPoint>]
let main args = runOrDefault args
