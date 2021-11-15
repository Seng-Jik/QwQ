module QwQ.Test.ListPosts

open NUnit.Framework
open QwQ
open FSharp.Control


let showList3Pages =
    AsyncSeq.take 3
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


let list3Pages source = Source.allPosts source |> showList3Pages


let [<Test>] konachan () = list3Pages Sources.Moebooru.konachan

