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
      ExludeTags = ["sex"; "rape"]
      Rating = Explicit
      Order = Popular }


let [<Test>] ``search: Konachan`` () = search10Pages searchOpt Sources.Moebooru.konachan
let [<Test>] ``search: Yandere`` () = search10Pages searchOpt Sources.Moebooru.yandere 
let [<Test>] ``search: Lolibooru`` () = search10Pages searchOpt Sources.Moebooru.lolibooru 
let [<Test>] ``search: HypnoHub`` () = 
    search10Pages
        { searchOpt with Tags = ["ke-ta"]; ExludeTags = ["ass"] }
        Sources.Moebooru.hypnohub 
        
let danbooruSearch = { searchOpt with Tags = ["ke-ta"] } 
let [<Test>] ``search: Danbooru`` () = search10Pages danbooruSearch Sources.Danbooru.danbooru 
let [<Test>] ``search: Sonohara`` () = search10Pages danbooruSearch Sources.Danbooru.sonohara 
let [<Test>] ``search: ATFBooru`` () = search10Pages searchOpt Sources.Danbooru.atfbooru 
let [<Test>] ``search: Hijiribe`` () = search10Pages danbooruSearch Sources.Danbooru.hijiribe 
let [<Test>] ``search: Safebooru Donmai`` () = 
    search10Pages 
        { danbooruSearch with Rating = Unrated; Order = Default }
        Sources.Danbooru.safebooruDonmai

let [<Test>] ``search: Gelbooru`` () = search10Pages searchOpt Sources.Gelbooru.gelbooru
let [<Test>] ``search: TBIB`` () = search10Pages searchOpt Sources.Gelbooru.tbib
let [<Test>] ``search: Safebooru`` () = 
    search10Pages 
        { searchOpt with Tags = ["ke-ta"]; Rating = Safe } 
        Sources.Gelbooru.safebooru

let [<Test>] ``search: XBooru`` () = search10Pages searchOpt Sources.Gelbooru.xbooru
let [<Test>] ``search: Rule34`` () = search10Pages searchOpt Sources.Gelbooru.rule34

let [<Test>] ``search: Sankaku Channel`` () = search10Pages searchOpt Sources.SankakuComplex.sankakuChannel
let [<Test>] ``search: Idol Complex`` () = 
    search10Pages
        { Tags = ["cosplay"; "asian"]
          ExludeTags = ["touhou"]
          Rating = Explicit
          Order = Popular }
        Sources.SankakuComplex.idolComplex


let searchOpt2 = { Tags = ["touhou"]; ExludeTags = []; Rating = Unrated; Order = Default }


let [<Test>] ``search: All Girl`` () = search10Pages searchOpt2 Sources.TheBooruProject.allgirl
let [<Test>] ``search: Foot Fetish Booru`` () = search10Pages searchOpt2 Sources.TheBooruProject.footfetishbooru
let [<Test>] ``search: CGBooru`` () = search10Pages { searchOpt2 with Tags = ["happy"] } Sources.TheBooruProject.cgbooru
let [<Test>] ``search: Touhou`` () = search10Pages { searchOpt2 with Tags = ["daiyousei"] } Sources.TheBooruProject.touhou
let [<Test>] ``search: Anime Girls 2020`` () = search10Pages searchOpt2 Sources.TheBooruProject.animegirls2020
let [<Test>] ``search: Character Library`` () = search10Pages searchOpt2 Sources.TheBooruProject.characterlib
let [<Test>] ``search: Ecchi Booru`` () = search10Pages searchOpt2 Sources.TheBooruProject.ecchibooru
let [<Test>] ``search: Hina`` () = search10Pages { searchOpt2 with Tags = ["nsfw"] } Sources.TheBooruProject.hina
let [<Test>] ``search: RuleXXX`` () = search10Pages { searchOpt2 with Tags = ["tagme"] } Sources.TheBooruProject.rulexxx

let [<Test>] ``search: Nekobooru`` () = 
    search10Pages 
        { searchOpt2 with Tags = ["touhou"]; Rating = Unrated } 
        Sources.Shimmie.nekobooru

let [<Test>] ``search: Tentacle Rape`` () = search10Pages { searchOpt2 with Tags = ["touhou"] } Sources.Shimmie.tentacleRape
let [<Test>] ``search: Fan Service`` () = 
    search10Pages { searchOpt2 with Tags = ["Series:Yumeria"] } Sources.Shimmie.fanservice

let [<Test>] ``search: Rule34 Paheal`` () = search10Pages searchOpt2 Sources.Shimmie.rule34paheal

let [<Test>] ``search: NHentai`` () = search10Pages { searchOpt2 with Tags = ["touhou"] } Sources.NHentaiSharp.nhentai

let [<Test>] ``search: Nozomi`` () = search10Pages searchOpt Sources.Nozomi.nozomi
let [<Test>] ``search: Hitomi`` () = search10Pages searchOpt2 Sources.Hitomi.hitomi

let [<Test>] ``search: Lolibaka`` () = 
    search10Pages 
        { searchOpt2 with 
            Tags = [ "flandre_scarlet"; "touhou" ] } 
        Sources.Lolibaka.lolibaka

let [<Test>] ``search: Booru io`` () = search10Pages searchOpt2 Sources.BooruIO.booruio

let [<Test>] ``search: The Hentai World`` () = search10Pages searchOpt2 Sources.TheHentaiWorld.thehentaiworld
