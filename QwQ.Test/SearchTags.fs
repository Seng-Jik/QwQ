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