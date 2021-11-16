module QwQ.Test.SearchPosts

open NUnit.Framework
open QwQ


let search10Pages searchOpt source =
    match source: ISource with
    | :? ISearch as x -> Search.search x searchOpt |> ListPosts.show10Pages
    | _ -> failwith "Do not support search."


// booru tags: order:score rating:e ke-ta uncensored -sex -nipples
let searchOpt =
    { Tags = ["ke-ta"; "uncensored"]
      NonTags = ["sex"; "nipples"]
      Rating = [Explicit]
      Order = Popular }


let [<Test>] ``search: Konachan`` () = search10Pages searchOpt Sources.Moebooru.konachan
let [<Test>] ``search: Yandere`` () = search10Pages searchOpt Sources.Moebooru.yandere 
let [<Test>] ``search: Lolibooru`` () = search10Pages searchOpt Sources.Moebooru.lolibooru 
let [<Test>] ``search: HypnoHub`` () = 
    search10Pages
        { searchOpt with Tags = ["ke-ta"]; NonTags = ["ass"] }
        Sources.Moebooru.hypnohub 
        
let [<Test>] ``search: Danbooru`` () = 
    search10Pages 
        { searchOpt with Tags = ["ke-ta"] } 
        Sources.Danbooru.danbooru 

let [<Test>] ``search: ATFBooru`` () = search10Pages searchOpt Sources.Danbooru.atfbooru 