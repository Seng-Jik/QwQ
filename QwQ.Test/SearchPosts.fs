module QwQ.Test.SearchPosts

open NUnit.Framework
open QwQ


let search10Pages searchOpt source =
    match source: ISource with
    | :? ISearch as x -> Search.search x searchOpt |> ListPosts.show10Pages
    | _ -> failwith "Do not support search."


// booru tags: order:popular rating:e ke-ta uncensored -sex -nipples
let searchOpt =
    { Tags = ["ke-ta"; "uncensored"]
      NonTags = ["sex"; "nipples"]
      Rating = set [Explicit]
      Order = Popular }


let [<Test>] ``search: Konachan`` () = search10Pages searchOpt Sources.Moebooru.konachan
let [<Test>] ``search: Yandere`` () = search10Pages searchOpt Sources.Moebooru.yandere 
let [<Test>] ``search: Lolibooru`` () = search10Pages searchOpt Sources.Moebooru.lolibooru 
let [<Test>] ``search: HypnoHub`` () = 
    search10Pages
        { searchOpt with Tags = ["ke-ta"]; NonTags = ["ass"] }
        Sources.Moebooru.hypnohub 
        
let danbooruSearch = { searchOpt with Tags = ["ke-ta"] } 
let [<Test>] ``search: Danbooru`` () = search10Pages danbooruSearch Sources.Danbooru.danbooru 
let [<Test>] ``search: Sonohara`` () = search10Pages danbooruSearch Sources.Danbooru.sonohara 
let [<Test>] ``search: ATFBooru`` () = search10Pages searchOpt Sources.Danbooru.atfbooru 
let [<Test>] ``search: Hijiribe`` () = search10Pages danbooruSearch Sources.Danbooru.hijiribe 

let [<Test>] ``search: Gelbooru`` () = search10Pages searchOpt Sources.Gelbooru.gelbooru
let [<Test>] ``search: TBIB`` () = search10Pages searchOpt Sources.Gelbooru.tbib
let [<Test>] ``search: Safebooru`` () = 
    search10Pages 
        { searchOpt with Tags = ["ke-ta"]; Rating = set [Safe] } 
        Sources.Gelbooru.safebooru

let [<Test>] ``search: XBooru`` () = search10Pages searchOpt Sources.Gelbooru.xbooru
let [<Test>] ``search: Rule34`` () = search10Pages searchOpt Sources.Gelbooru.rule34

let [<Test>] ``search: Sankaku Channel`` () = search10Pages searchOpt Sources.SankakuComplex.sankakuChannel
let [<Test>] ``search: Idol Complex`` () = 
    search10Pages
        { Tags = ["cosplay"; "asian"]
          NonTags = ["touhou"]
          Rating = set [Explicit]
          Order = Popular }
        Sources.SankakuComplex.idolComplex
