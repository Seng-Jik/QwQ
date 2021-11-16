#r "nuget: FSharp.Control.AsyncSeq"

open System
open System.IO
open System.Diagnostics
open FSharp.Control


if not <| File.Exists "./QwQ/bin/Release/netstandard2.0/publish/QwQ.dll" 
then Process.Start("dotnet", "publish ./QwQ/QwQ.fsproj -c Release -f netstandard2.0").WaitForExit()


#r "./QwQ/bin/Release/netstandard2.0/publish/QwQ.dll"


let sourceName = 
    Environment.GetCommandLineArgs () 
    |> Array.tryItem 2
    |> function
        | None -> 
            printfn "Usage:"
            printfn "    dotnet fsi FullTest.fsx <sourceName> [--list-posts | --list-tags] "
            printfn ""
            exit -1
        | Some x -> x


let testTarget =
    Environment.GetCommandLineArgs ()
    |> Array.tryItem 3
    |> Option.defaultValue "--list-posts"


open QwQ
open QwQ.Utils


let source =
    Sources.Sources.sources
    |> List.tryFind (Source.name >> (=) sourceName)
    |> function
        | Some x -> x
        | None ->
            printfn "Can not find source named %A" sourceName
            exit -1


match testTarget with
| "--list-tags" ->
    match source with
    | :? ITags as source ->
        Tags.allTags source
        |> AsyncSeq.iter (Result.unwrap >> printfn "%s")
        |> Async.RunSynchronously
    | _ -> printfn "%A source is not an ITags." <| Source.name source
| "--list-posts" ->
    Source.allPosts source
    |> AsyncSeq.map Result.unwrap
    |> AsyncSeq.iteriAsync (fun i x -> async {
        printfn $"{Source.name source}: Page {i} has {Seq.length x} posts" })
    |> Async.RunSynchronously
| x -> printfn "Here is no test target %A." x





