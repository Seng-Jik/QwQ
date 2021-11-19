module QwQ.Test.GetPostById

open NUnit.Framework
open QwQ
open QwQ.Utils


let getPostById id (source: ISource) =
    match source with
    | :? IGetPostById as source ->
        Source.getPostById source id
        |> Async.RunSynchronously
        |> Result.unwrap
        |> printfn "%A"
    | _ -> failwith "Not supported."


let [<Test>] ``getPostById: Konachan`` () = getPostById 334447UL Sources.Moebooru.konachan 
let [<Test>] ``getPostById: Yandere`` () = getPostById 884012UL Sources.Moebooru.yandere
let [<Test>] ``getPostById: Lolibooru`` () = getPostById 381980UL Sources.Moebooru.lolibooru
let [<Test>] ``getPostById: HypnoHub`` () = getPostById 130894UL Sources.Moebooru.hypnohub

let [<Test>] ``getPostById: Danbooru`` () = getPostById 4917687UL Sources.Danbooru.danbooru
let [<Test>] ``getPostById: Sonohara`` () = getPostById 4918795UL Sources.Danbooru.sonohara
let [<Test>] ``getPostById: ATFBooru`` () = getPostById 366626UL Sources.Danbooru.atfbooru
let [<Test>] ``getPostById: Hijiribe`` () = getPostById 4918788UL Sources.Danbooru.hijiribe

let [<Test>] ``getPostById: Gelbooru`` () = getPostById 6653855UL Sources.Gelbooru.gelbooru
let [<Test>] ``getPostById: TBIB`` () = getPostById 9867580UL Sources.Gelbooru.tbib
let [<Test>] ``getPostById: Safebooru`` () = getPostById 3722213UL Sources.Gelbooru.safebooru
let [<Test>] ``getPostById: XBooru`` () = getPostById 885166UL Sources.Gelbooru.xbooru
let [<Test>] ``getPostById: Rule34`` () = getPostById 5289582UL Sources.Gelbooru.rule34

let [<Test>] ``getPostById: Sankaku Channel`` () = 
    getPostById 28595183UL Sources.SankakuComplex.sankakuChannel

let [<Test>] ``getPostById: Nekobooru`` () =
    getPostById 1796UL Sources.Nekobooru.nekobooru