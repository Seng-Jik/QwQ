module QwQ.Test.SearchTags

open NUnit.Framework
open QwQ


let search100Tags' (tagName: string) (source: ISource) =
    match source with
    | :? ISearchTag as x -> Tags.searchTag tagName x |> ListTags.show100Tags
    | _ -> failwith "Do not support tags."


let search100Tags = search100Tags' "touhou"


let [<Test>] ``searchTag: Konachan`` () = search100Tags Sources.Moebooru.konachan
let [<Test>] ``searchTag: Yandere`` () = search100Tags Sources.Moebooru.yandere
let [<Test>] ``searchTag: Lolibooru`` () = search100Tags Sources.Moebooru.lolibooru
let [<Test>] ``searchTag: HypnoHub`` () = search100Tags Sources.Moebooru.hypnohub

let [<Test>] ``searchTag: Danbooru`` () = search100Tags Sources.Danbooru.danbooru
let [<Test>] ``searchTag: ATFBooru`` () = search100Tags Sources.Danbooru.atfbooru
let [<Test>] ``searchTag: Sonohara`` () = search100Tags Sources.Danbooru.sonohara
let [<Test>] ``searchTag: Hijiribe`` () = search100Tags Sources.Danbooru.hijiribe
let [<Test>] ``searchTag: Safebooru Donmai`` () = search100Tags Sources.Danbooru.safebooruDonmai

let [<Test>] ``searchTag: Gelbooru`` () = search100Tags Sources.Gelbooru.gelbooru
let [<Test>] ``searchTag: TBIB`` () = search100Tags Sources.Gelbooru.tbib
let [<Test>] ``searchTag: Safebooru`` () = search100Tags Sources.Gelbooru.safebooru
let [<Test>] ``searchTag: XBooru`` () = search100Tags Sources.Gelbooru.xbooru
let [<Test>] ``searchTag: Rule34`` () = search100Tags Sources.Gelbooru.rule34

let [<Test>] ``searchTag: Sankaku Channel`` () = search100Tags Sources.SankakuComplex.sankakuChannel
let [<Test>] ``searchTag: Tentacle Rape`` () = search100Tags Sources.Shimmie.tentacleRape

let [<Test>] ``searchTag: Nekobooru`` () = search100Tags Sources.Shimmie.nekobooru
let [<Test>] ``searchTag: Fan Service`` () = search100Tags' "Series:" Sources.Shimmie.fanservice
let [<Test>] ``searchTag: Rule34 Paheal`` () = search100Tags' "Touhou" Sources.Shimmie.rule34paheal

let [<Test>] ``searchTag: Booru io`` () = search100Tags Sources.BooruIO.booruio