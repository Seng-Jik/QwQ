module QwQ.Test.ListPosts

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show10Pages x =
    let items = ref 0
    AsyncSeq.truncate 10 x
    |> AsyncSeq.iteriAsync (fun i x -> 
        async {
            printfn ""
            printfn $"===== Page: {i}"
            printfn ""

            Result.unwrap x
            |> Seq.iter (fun post ->
                printfn $"{post.Id} {post.Tags}"
                printfn $"{post.PreviewImage}"
                printfn ""
                items.Value <- items.Value + 1
            )
        })
    |> Async.RunSynchronously

    if items.Value <= 0 then
        failwith "Nothing to show."


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

let [<Test>] ``list: All Girl`` () = list10Pages Sources.TheBooruProject.allgirl
let [<Test>] ``list: Foot Fetish Booru`` () = list10Pages Sources.TheBooruProject.footfetishbooru
let [<Test>] ``list: CGBooru`` () = list10Pages Sources.TheBooruProject.cgbooru
let [<Test>] ``list: Touhou`` () = list10Pages Sources.TheBooruProject.touhou
let [<Test>] ``list: Anime Girls 2020`` () = list10Pages Sources.TheBooruProject.animegirls2020
let [<Test>] ``list: Character Library`` () = list10Pages Sources.TheBooruProject.characterlib
let [<Test>] ``list: Ecchi Booru`` () = list10Pages Sources.TheBooruProject.ecchibooru
let [<Test>] ``list: Hina`` () = list10Pages Sources.TheBooruProject.hina
let [<Test>] ``list: RuleXXX`` () = list10Pages Sources.TheBooruProject.rulexxx

let [<Test>] ``list: Nekobooru`` () = list10Pages Sources.Shimmie.nekobooru
let [<Test>] ``list: Tentacle Rape`` () = list10Pages Sources.Shimmie.tentacleRape
let [<Test>] ``list: Fan Service`` () = list10Pages Sources.Shimmie.fanservice

let [<Test>] ``list: NHentai`` () = list10Pages Sources.NHentaiSharp.nhentai