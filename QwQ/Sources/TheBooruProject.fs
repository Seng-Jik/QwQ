module QwQ.Sources.TheBooruProject

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


let processViewPage (url: string) =
    async {
        let! html = HtmlDocument.AsyncLoad url
        let image =
            html.CssSelect "#note-container img#image"
            |> List.tryExactlyOne
            |> Option.bind (fun x -> x.TryGetAttribute "src")
            |> Option.map (fun x -> x.Value ())

        let source =
            html.CssSelect "#tag_list"
            |> List.tryExactlyOne
            |> Option.map (string >> fun x -> 
                let openBracket = "Source:"
                let closeBracket = "Rating"
                let a = x.IndexOf openBracket + String.length openBracket
                let b = x.IndexOf closeBracket - 1
                x.[a .. b].Trim().Trim('\r', '\n').Trim())
            |> Option.bind String.nullOrWhitespace
            |> Option.toList

        return Option.map (mapHttpsContent HttpsOptions.Default) image, (url :: source)
    }


let processData (x: string) =
    let id = x.[x.IndexOf "posts[" + 6 .. x.IndexOf ']' - 1] |> uint64
    let body = x.[x.IndexOf '{'..]
    let tags = 
        let openBracket = "\'tags\':\'"
        let closeBracket = "\'.split"
        let a = body.IndexOf openBracket + String.length openBracket 
        let b = body.LastIndexOf closeBracket - 1
        body.[a .. b].Split ' ' |> List.ofArray

    let rating =
        let openBracket = "\'rating\':\'"
        let a = body.IndexOf "\'rating\':\'"
        let b = body.IndexOf('\'', a + String.length openBracket)

        match body.[a + String.length openBracket .. b - 1] with
        | "Safe" -> Safe
        | "Questionable" -> Questionable
        | "Explicit" -> Explicit
        | x -> Rating' x

    id, rating, tags


let mapPostPage' (page: HtmlDocument) =
    page.CssSelect ".thumb"
    |> List.choose (fun x -> 
        let previewUrl = 
            Option.protect (fun () ->
                x.CssSelect "img"
                |> List.tryExactlyOne
                |> Option.bind (fun x -> x.TryGetAttribute "src")
                |> Option.map (fun x -> x.Value ()))
            |> Option.flatten
        
        let script =
            Option.protect (fun () ->
                x.CssSelect "script"
                |> List.tryExactlyOne
                |> Option.map (string >> processData))
            |> Option.flatten
            
        script |> Option.map (fun script -> previewUrl, script))


let mapPostPage baseUrl source (page: HtmlDocument) =
    mapPostPage' page
    |> Seq.map (fun (preview, (id, rating, tags)) ->
        let details = processViewPage $"{baseUrl}/index.php?page=post&s=view&id={id}"

        { Id = id
          Rating = rating
          Tags = tags
          Title = None
          Source = source
          SourceUrl = asyncSeq { let! _, x = details in yield! AsyncSeq.ofSeq x}
          PreviewImage = Option.map (mapHttpsContent HttpsOptions.Default) preview
          Content = asyncSeq { let! x, _ = details in yield AsyncSeq.ofSeq <| Option.toList x} })
    


type TheBooruProjectSource (name, baseUrl) =

    let requestPostUrl' this postFix =
        enumAllPages <| fun pageId ->
            async {
                let! doc = 
                    HtmlDocument.AsyncLoad $"{baseUrl}/index.php?page=post&s=list&pid={20 * pageId}{postFix}"
                    |> Async.protect
                    |> Async.retryResult 3 1500
                
                return doc |> Result.map (mapPostPage baseUrl this)
            }

    interface ISource with
        member _.Name = name
        member x.AllPosts = requestPostUrl' x "&tags=all"

    interface ISearch with
        member x.Search s = 
            requestPostUrl' x $"&tags={mapSearchOptions { s with Order = Default; NonTags =[] } }"
            |> AntiGuro.antiThat s.NonTags

     
let allgirl = TheBooruProjectSource ("All Girl", "https://allgirl.booru.org") :> ISource
let footfetishbooru = TheBooruProjectSource("Foot Fetish Booru", "https://footfetishbooru.booru.org") :> ISource
let cgbooru = TheBooruProjectSource("CGBooru", "https://cg.booru.org") :> ISource
let touhou = TheBooruProjectSource("Touhou", "https://hakurei.booru.org") :> ISource
let animegirls2020 = TheBooruProjectSource("Anime Girls 2020", "https://animegirls2020.booru.org") :> ISource
let characterlib = TheBooruProjectSource("Character Library", "https://characterlibrary.booru.org") :> ISource
let ecchibooru = TheBooruProjectSource("Ecchi Booru", "https://ecchi.booru.org") :> ISource
let hina = TheBooruProjectSource("Hina", "https://hina.booru.org") :> ISource
let rulexxx = TheBooruProjectSource("RuleXXX", "https://rulexxx.booru.org") :> ISource


let sources = 
    [ allgirl
      footfetishbooru
      cgbooru
      touhou
      animegirls2020
      characterlib
      ecchibooru
      hina
      rulexxx ]

