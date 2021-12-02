module QwQ.Test.ListTags

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show100Tags (x: AsyncSeq<Result<Tag, exn>>) =
    let s =
        x
        |> AsyncSeq.truncate 100
        |> AsyncSeq.cache
        |> AsyncSeq.toBlockingSeq

    if Seq.isEmpty s then failwith "No tags found!"

    s |> Seq.iter (Result.unwrap >> printfn "%s")


let list100Tags (source: ISource) =
    match source with
    | :? ITags as x -> Tags.allTags x |> show100Tags
    | _ -> failwith "Do not support tags."


let [<Test>] ``tags: Konachan`` () = list100Tags Sources.Moebooru.konachan
let [<Test>] ``tags: Yandere`` () = list100Tags Sources.Moebooru.yandere
let [<Test>] ``tags: Lolibooru`` () = list100Tags Sources.Moebooru.lolibooru
let [<Test>] ``tags: HypnoHub`` () = list100Tags Sources.Moebooru.hypnohub

let [<Test>] ``tags: Danbooru`` () = list100Tags Sources.Danbooru.danbooru
let [<Test>] ``tags: ATFBooru`` () = list100Tags Sources.Danbooru.atfbooru
let [<Test>] ``tags: Sonohara`` () = list100Tags Sources.Danbooru.sonohara
let [<Test>] ``tags: Hijiribe`` () = list100Tags Sources.Danbooru.hijiribe
let [<Test>] ``tags: Safebooru Donmai`` () = list100Tags Sources.Danbooru.safebooruDonmai

let [<Test>] ``tags: Gelbooru`` () = list100Tags Sources.Gelbooru.gelbooru
let [<Test>] ``tags: TBIB`` () = list100Tags Sources.Gelbooru.tbib
let [<Test>] ``tags: Safebooru`` () = list100Tags Sources.Gelbooru.safebooru
let [<Test>] ``tags: XBooru`` () = list100Tags Sources.Gelbooru.xbooru
let [<Test>] ``tags: Rule34`` () = list100Tags Sources.Gelbooru.rule34

let [<Test>] ``tags: Sankaku Channel`` () = list100Tags Sources.SankakuComplex.sankakuChannel

let [<Test>] ``tags: Nekobooru`` () = list100Tags Sources.Shimmie.nekobooru
let [<Test>] ``tags: Tentacle Rape`` () = list100Tags Sources.Shimmie.tentacleRape
let [<Test>] ``tags: Fan Service`` () = list100Tags Sources.Shimmie.fanservice
let [<Test>] ``tags: Rule34 Paheal`` () = list100Tags Sources.Shimmie.rule34paheal

let [<Test>] ``tags: Nozomi`` () = list100Tags Sources.Nozomi.nozomi
let [<Test>] ``tags: Hitomi`` () = list100Tags Sources.Hitomi.hitomi