module QwQ.Test.ListPosts

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show10Pages =
    AsyncSeq.truncate 10
    >> AsyncSeq.iteriAsync (fun i x -> 
        async {
            printfn ""
            printfn $"===== Page: {i}"
            printfn ""

            Result.unwrap x
            |> Seq.iter (fun post ->
                printfn $"{post.Id} {post.Tags}"
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

let [<Test>] ``list: Danbooru`` () = list10Pages Sources.Danbooru.danbooru
let [<Test>] ``list: ATFBooru`` () = list10Pages Sources.Danbooru.atfbooru
let [<Test>] ``list: Sonohara`` () = list10Pages Sources.Danbooru.sonohara
let [<Test>] ``list: Hijiribe`` () = list10Pages Sources.Danbooru.hijiribe

let [<Test>] ``list: Gelbooru`` () = list10Pages Sources.Gelbooru.gelbooru
let [<Test>] ``list: TBIB`` () = list10Pages Sources.Gelbooru.tbib
let [<Test>] ``list: Safebooru`` () = list10Pages Sources.Gelbooru.safebooru
let [<Test>] ``list: XBooru`` () = list10Pages Sources.Gelbooru.xbooru
let [<Test>] ``list: Rule34`` () = list10Pages Sources.Gelbooru.rule34

let [<Test>] ``list: Sankaku Channel`` () = list10Pages Sources.SankakuComplex.sankakuChannel
let [<Test>] ``list: Idol Complex`` () = list10Pages Sources.SankakuComplex.idolComplex
