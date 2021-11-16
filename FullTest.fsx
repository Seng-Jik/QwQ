#r "nuget: FSharp.Control.AsyncSeq"
#r "nuget: FSharp.Data"

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
            printfn "    dotnet fsi FullTest.fsx <sourceName> [test] "
            printfn ""
            printfn "Tests:"
            printfn "    --list-tags"
            printfn "    --list-posts"
            printfn "    --download-preview"
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


let download =
    function
    | Https (url, opt) -> 
        async {
            let! stream =
                FSharp.Data.Http.AsyncRequestStream 
                    (url, headers = ["User-Agent", opt.UserAgent |> Option.defaultValue ""])

            if stream.StatusCode = 200
            then return Ok ()
            else return Error stream
        }


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
| "--download-preview" -> 
    Source.allPosts source
    |> AsyncSeq.map Result.unwrap
    |> AsyncSeq.mapi (fun pageId page -> 
        asyncSeq {
            for post in page -> 
                match post.PreviewImage with
                | Some x -> 
                    asyncSeq {
                        let! down = download x.DownloadMethod
                        match down with
                        | Ok () -> printfn $"{Source.name source} (Page {pageId}, Post {post.Id}) downloaded."
                        | Error response -> 
                            printfn $"{Source.name source} (Page {pageId}, Post {post}, Response {response.StatusCode}) failed:"
                            printfn "    %A" x.DownloadMethod
                            yield post
                    }
                | None -> 
                    asyncSeq { 
                        printfn $"{Source.name source} (Page {pageId}, Post {post}) has no preview image."
                    }
        }
    )
    |> AsyncSeq.concat
    |> AsyncSeq.concat
    |> AsyncSeq.toListAsync
    |> Async.RunSynchronously
    |> List.map (fun errPosts -> 
        printfn ""
        printfn "===="
        printfn "%A" errPosts
        printfn "===="
        printfn "")
    |> function 
        | [] -> ()
        | _ -> failwith "↑↑↑ Here is some errors ↑↑↑"

| x -> printfn "Here is no test target %A." x





