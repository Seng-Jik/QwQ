module QwQ.Test.ListPosts

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show10Pages =
    AsyncSeq.take 10
    >> AsyncSeq.iteriAsync (fun i x -> 
        async {
            printfn ""
            printfn $"===== Page: {i}"
            printfn ""

            Result.unwrap x
            |> Seq.iter (fun post ->
                printfn $"{post.Id} {AsyncSeq.toListSynchronously post.Tags}"
                printfn $"{post.PreviewImage}"
                printfn $"Contents: {AsyncSeq.toListSynchronously post.Content}"
                printfn ""
            )
        })
    >> Async.RunSynchronously


let list10Pages source = Source.allPosts source |> show10Pages


let [<Test>] ``list: Konachan`` () = list10Pages Sources.Moebooru.konachan
let [<Test>] ``list: Yandere`` () = list10Pages Sources.Moebooru.yandere
let [<Test>] ``list: Lolibooru`` () = list10Pages Sources.Moebooru.lolibooru
let [<Test>] ``list: HypnoHub`` () = list10Pages Sources.Moebooru.hypnohub

