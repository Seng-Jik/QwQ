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