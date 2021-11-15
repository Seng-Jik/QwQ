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
            printfn "    dotnet fsi FullTest.fsx <sourceName>"
            printfn ""
            exit -1
        | Some x -> x


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


Source.allPosts source
|> AsyncSeq.map Result.unwrap
|> AsyncSeq.iteriAsync (fun i x -> async {
    printfn $"{Source.name source}: Page {i} has {Seq.length x} posts" })
|> Async.RunSynchronously




