module QwQ.Sources.Shimmie

open QwQ
open QwQ.Utils
open FSharp.Control
open FSharp.Data
open QwQ.Sources.Moebooru


let getContentAndSourceUrl name baseUrl fullUrl postId (page: HtmlDocument) =
    let img = 
        page.CssSelect "#main_image"
        |> List.tryExactlyOne
        |> Option.bind (fun x -> x.TryGetAttribute "src")
        |> Option.map (fun x -> 
            let x = x.Value ()
            if x.ToLower().StartsWith("https://")
            then x
            else baseUrl + x)
        |> Option.bind String.nullOrWhitespace
        |> Option.map (
            mapHttpsContent HttpsOptions.Empty 
            >> fun x -> { x with FileName = $"{name} {postId}{System.IO.Path.GetExtension x.FileName}"})

    let source =
        Option.protect (fun () ->
            page.CssSelect ".image_info tr"
            |> List.filter (fun x -> 
                x.CssSelect "th"
                |> List.exactlyOne
                |> HtmlNodeExtensions.InnerText
                |> (=) "Source")
            |> List.exactlyOne
            |> fun x -> CssSelectorExtensions.CssSelect (x, "a")
            |> List.tryExactlyOne
            |> Option.bind (HtmlNode.tryGetAttribute "href")
            |> Option.map (fun x -> x.Value())
            |> Option.bind String.nullOrWhitespace)
        |> Option.flatten

    AsyncSeq.ofSeq <| Option.toList img, [fullUrl] @ Option.toList source


let getPostByViewPage source name baseUrl (page: HtmlDocument) ratingIfNotSupportRating =
    Option.protect (fun () ->
        let postId =
            page.CssSelect "input"
            |> List.find (fun x -> x.AttributeValue "name" = "image_id")
            |> fun x -> x.TryGetAttribute "value"
            |> Option.unwrap
            |> HtmlAttributeExtensions.Value
            |> uint64

        let fullUrl = $"{baseUrl}/post/view/{id}"

        let content, sourceUrls = getContentAndSourceUrl name baseUrl fullUrl postId page

        let infoTable =
            page.CssSelect ".image_info tr"
            |> List.map (fun x -> 
                x.CssSelect("th").Head.InnerText().Trim(), x.CssSelect("td"))
            |> dict

        let tags = 
            Option.protect (fun () ->
                let table = infoTable.["Tags"]
                if table.[0].Name() <> "td"
                then List.map (fun (x: HtmlNode) -> x.InnerText ()) table
                else
                    match List.tryExactlyOne table with
                    | Some input when List.tryExactlyOne (input.Elements()) |> Option.map (fun x -> x.HasName "input") = Some true ->
                        input.Elements ()
                        |> List.tryExactlyOne 
                        |> Option.bind (fun x -> x.TryGetAttribute "value")
                        |> Option.map (fun x -> x.Value().Split ' ')
                        |> Option.defaultValue [||]
                        |> Array.toList
                    | _ -> 
                        table 
                        |> List.collect (fun x -> x.Elements ())
                        |> List.map (fun x -> x.InnerText ()))
            |> Option.defaultValue []

        let rating =
            Option.protect (fun () ->
                match infoTable.["Rating"].Head.InnerText().Trim() with
                | "Safe" -> Safe
                | "Questionable" -> Questionable
                | "Explicit" -> Explicit
                | _ -> Unrated)
            |> Option.orElse ratingIfNotSupportRating
            |> Option.defaultValue Unrated

        let preview = 
            page.Html().CssSelect "head meta"
            |> List.tryFind (fun x -> 
                match x.TryGetAttribute "property" with
                | Some x -> x.Value () = "og:image"
                | _ -> false)
            |> Option.bind (fun x -> x.TryGetAttribute "content")
            |> Option.map HtmlAttribute.value

        { Id = postId 
          Title = None 
          Source = source
          Rating = rating
          SourceUrl = AsyncSeq.ofSeq sourceUrls
          Tags = tags
          PreviewImage = preview |> Option.map (mapHttpsContent HttpsOptions.Empty)
          Content = AsyncSeq.singleton content } )


let mapViewPage name baseUrl id =
    async {
        let src = $"{baseUrl}/post/view/{id}"
        let! page = 
            HtmlDocument.AsyncLoad src
            |> Async.protect

        match page with
        | Ok x -> return getContentAndSourceUrl name baseUrl src id x
        | _ -> return AsyncSeq.empty, []
    }


let mapPage source baseUrl (page: HtmlDocument) =
    page.CssSelect ".shm-image-list"
    |> List.tryHead
    |> Option.map (fun x -> x.Elements())
    |> Option.defaultValue []
    |> List.choose (fun node -> 
        let tags = 
            node.TryGetAttribute "data-tags"
            |> Option.map (fun x -> x.Value().Split(' '))

        let id = 
            node.TryGetAttribute "data-post-id"
            |> Option.map (fun x -> x.Value() |> uint64)
        
        let img = 
            node.CssSelect "img"
            |> List.tryExactlyOne
            |> Option.bind (fun x -> x.TryGetAttribute "src")
            |> Option.map (fun x -> 
                let x = x.Value ()
                if x.ToLower().StartsWith("https://")
                then x
                else baseUrl + x)
            
        id |> Option.map (fun id ->
            { Id = id 
              Title = None 
          
              Source = source
              Rating = Unrated 
              SourceUrl = asyncSeq { let! _, s = mapViewPage source.Name baseUrl id in yield! AsyncSeq.ofSeq s }
              Tags = tags |> Option.map Array.toList |> Option.defaultValue []
              PreviewImage = img |> Option.map (mapHttpsContent HttpsOptions.Empty)
              Content = asyncSeq { let! a, _ = mapViewPage source.Name baseUrl id in yield a } } ))


type ShimmieSource (name, baseUrl: string, rule34PahealTags) =

    let requestPostList this search =
        enumAllPages <| fun pageId ->
            async {
                let! doc = 
                    HtmlDocument.AsyncLoad $"{baseUrl}/post/list{search}/{1 + pageId}"
                    |> Async.protect
                    |> Async.retryResult 3 1500

                match doc with
                | Ok doc -> 
                    return
                        match getPostByViewPage this name baseUrl doc <| Some Unrated with
                        | Some x -> [x]
                        | None -> mapPage this baseUrl doc
                        |> Ok
                | Error x when x.Message.Contains "Search limit hit" -> return Ok []
                | Error x when x.Message.Contains "No Images Found" -> return Ok []
                | Error x when x.Message.Contains "No posts Found" -> return Ok []
                | Error x -> return Error x
            }

    let tags =
        asyncSeq {
            match!
                HtmlDocument.AsyncLoad ($"{baseUrl}/tags" + if rule34PahealTags then "?starts_with" else "")
                |> Async.protect
                |> Async.retryResult 3 1500
            with
            | Ok html ->
                yield! 
                    html.CssSelect "#Tagsmain .blockbody a"
                    |> Seq.choose (fun x -> String.nullOrWhitespace <| x.InnerText())
                    |> Seq.map Ok
                    |> AsyncSeq.ofSeq
            | Error x -> yield Error x
        }
    
    interface ISource with
        member _.Name = name
        member this.AllPosts =
            requestPostList this ""

    interface ISearch with
        member this.Search search =
            mapSearchOptions { search with Rating = Unrated; Order = Default }
            |> (+) "/"
            |> requestPostList this
            |> AntiGuro.antiThat search.NonTags

    interface ITags with
        member _.Tags = tags

    interface ISearchTag with
        member this.SearchTag s = Tags.slowSearchTag s this

    interface IGetPostById with
        member x.GetPostById id =
            async {
                let fullUrl = $"{baseUrl}/post/view/{id}"
                let! html = HtmlDocument.AsyncLoad fullUrl
                return getPostByViewPage x name baseUrl html (Some Unrated)
            }
            |> Async.protect


let nekobooru = ShimmieSource ("Nekobooru", "https://neko-booru.com", false) :> ISource
let tentacleRape = ShimmieSource ("Tentacle Rape", "https://tentaclerape.net", false) :> ISource
let fanservice = ShimmieSource ("Fan Service", "https://fanservice.fan", false) :> ISource
let rule34paheal = ShimmieSource("Rule34 Paheal", "https://rule34.paheal.net", true) :> ISource


let sources = 
    [ nekobooru
      tentacleRape
      fanservice
      rule34paheal ]
