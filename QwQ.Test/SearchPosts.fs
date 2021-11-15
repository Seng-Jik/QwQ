module QwQ.Test.SearchPosts

open NUnit.Framework
open QwQ


let search10Pages source searchOpt =
    match source: ISource with
    | :? ISearch as x -> Search.search x searchOpt |> ListPosts.show10Pages
    | _ -> failwith "Do not support search."


// booru tags: order:score rating:e ke-ta uncensored -sex -nipples
let searchOpt =
    { Tags = ["ke-ta"; "uncensored"]
      NonTags = ["sex"; "nipples"]
      Rating = [Explicit]
      Order = Popular }


let [<Test>] ``search: Konachan`` () = search10Pages Sources.Moebooru.konachan searchOpt
let [<Test>] ``search: Yandere`` () = search10Pages Sources.Moebooru.yandere searchOpt
let [<Test>] ``search: Lolibooru`` () = search10Pages Sources.Moebooru.lolibooru searchOpt
let [<Test>] ``search: HypnoHub`` () = 
    search10Pages Sources.Moebooru.hypnohub 
        { searchOpt with Tags = ["ke-ta"]; NonTags = ["ass"] }