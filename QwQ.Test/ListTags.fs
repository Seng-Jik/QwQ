module QwQ.Test.ListTags

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show100Tags (x: AsyncSeq<Result<Tag, exn>>) =
    x
    |> AsyncSeq.take 100
    |> AsyncSeq.iter (Result.unwrap >> printfn "%s")
    |> Async.RunSynchronously


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

let [<Test>] ``tags: Gelbooru`` () = list100Tags Sources.Gelbooru.gelbooru
let [<Test>] ``tags: TBIB`` () = list100Tags Sources.Gelbooru.tbib
let [<Test>] ``tags: Safebooru`` () = list100Tags Sources.Gelbooru.safebooru
let [<Test>] ``tags: XBooru`` () = list100Tags Sources.Gelbooru.xbooru
let [<Test>] ``tags: Rule34`` () = list100Tags Sources.Gelbooru.rule34