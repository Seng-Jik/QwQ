module QwQ.Test.SearchPosts

open NUnit.Framework
open QwQ


let search10Pages searchOpt source =
    match source: ISource with
    | :? ISearch as x -> Search.search x searchOpt |> ListPosts.show10Pages
    | _ -> failwith "Do not support search."

    
let searchOpt =
    { Tags = ["ke-ta"; "uncensored"]
      NonTags = ["sex"; "nipples"]
      Rating = [Explicit]
      Order = Default }


let [<Test>] ``search: Konachan`` () = search10Pages searchOpt Sources.Moebooru.konachan