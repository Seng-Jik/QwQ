module QwQ.Test.SearchTags

open NUnit.Framework
open QwQ


let search100Tags (source: ISource) =
    match source with
    | :? ISearchTag as x -> Tags.searchTag "touhou" x |> ListTags.show100Tags
    | _ -> failwith "Do not support tags."


let [<Test>] ``searchTag: Konachan`` () = search100Tags Sources.Moebooru.konachan
let [<Test>] ``searchTag: Yandere`` () = search100Tags Sources.Moebooru.yandere
let [<Test>] ``searchTag: Lolibooru`` () = search100Tags Sources.Moebooru.lolibooru
let [<Test>] ``searchTag: HypnoHub`` () = search100Tags Sources.Moebooru.hypnohub

let [<Test>] ``searchTag: Danbooru`` () = search100Tags Sources.Danbooru.danbooru
let [<Test>] ``searchTag: ATFBooru`` () = search100Tags Sources.Danbooru.atfbooru

let [<Test>] ``searchTag: Gelbooru`` () = search100Tags Sources.Gelbooru.gelbooru
let [<Test>] ``searchTag: TBIB`` () = search100Tags Sources.Gelbooru.tbib
let [<Test>] ``searchTag: Safebooru`` () = search100Tags Sources.Gelbooru.safebooru
let [<Test>] ``searchTag: XBooru`` () = search100Tags Sources.Gelbooru.xbooru
let [<Test>] ``searchTag: Rule34`` () = search100Tags Sources.Gelbooru.rule34