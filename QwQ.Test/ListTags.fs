module QwQ.Test.ListTags

open NUnit.Framework
open QwQ
open QwQ.Utils
open FSharp.Control


let show100Tags (x: AsyncSeq<Result<TagDetails, exn>>) =
    x
    |> AsyncSeq.take 100
    |> AsyncSeq.iter (Result.unwrap >> printfn "%A")
    |> Async.RunSynchronously


let list100Tags (source: ISource) =
    match source with
    | :? ITags as x -> Tags.allTags x |> show100Tags
    | _ -> failwith "Do not support tags."


let [<Test>] ``tags: Konachan`` () = list100Tags Sources.Moebooru.konachan
