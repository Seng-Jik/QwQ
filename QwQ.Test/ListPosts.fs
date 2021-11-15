module QwQ.Test.ListPosts

open NUnit.Framework
open QwQ
open FSharp.Control


let show10Pages =
    AsyncSeq.take 10
    >> AsyncSeq.iteriAsync (fun i x -> 
        async {
            printfn ""
            printfn $"===== Page: {i}"
            printfn ""

            match x with
            | Error e -> 
                printfn "%A" e
                Assert.Fail ()
            | Ok x -> 
                x
                |> Seq.iter (fun post ->
                    printfn $"{post.Id} {AsyncSeq.toListSynchronously post.Tags}"
                    printfn $"{post.PreviewImage}"
                    printfn $"Contents: {AsyncSeq.toListSynchronously post.Content}"
                    printfn ""
                )
        })
    >> Async.RunSynchronously


let list10Pages source = Source.allPosts source |> show10Pages


let [<Test>] listKonachan () = list10Pages Sources.Moebooru.konachan

