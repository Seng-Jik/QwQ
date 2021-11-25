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
        |> Option.map (fun x -> x.Value())
        |> Option.bind String.nullOrWhitespace
        |> Option.map (
            (+) baseUrl 
            >> mapHttpsContent HttpsOptions.Default 
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
                then table
                else table |> List.collect (fun x -> x.Elements ())
                |> List.map (fun x -> x.InnerText ()))
            |> Option.toList
            |> List.concat

        let rating =
            Option.protect (fun () ->
                match infoTable.["Rating"].Head.InnerText().Trim() with
                | "Safe" -> Safe
                | "Questionable" -> Questionable
                | "Explicit" -> Explicit
                | x -> Rating' x)
            |> Option.orElse ratingIfNotSupportRating
            |> Option.defaultValue (Rating' "Unknown")

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
          PreviewImage = preview |> Option.map (mapHttpsContent HttpsOptions.Default)
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


let mapPage source rating baseUrl (page: HtmlDocument) =
    page.CssSelect ".shm-image-list a"
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
            |> Option.map (fun x -> baseUrl + x.Value())
            
        id |> Option.map (fun id ->
            { Id = id 
              Title = None 
          
              Source = source
              Rating = rating 
              SourceUrl = asyncSeq { let! _, s = mapViewPage source.Name baseUrl id in yield! AsyncSeq.ofSeq s }
              Tags = tags |> Option.map Array.toList |> Option.defaultValue []
              PreviewImage = img |> Option.map (mapHttpsContent HttpsOptions.Default)
              Content = asyncSeq { let! a, _ = mapViewPage source.Name baseUrl id in yield a } } ))


type ShimmieSource (name, baseUrl, ratingIfNotSupportRating) =

    let requestPostList this rating search =
        enumAllPages <| fun pageId ->
            async {
                let! doc = 
                    HtmlDocument.AsyncLoad $"{baseUrl}/post/list{search}/{1 + pageId}"
                    |> Async.protect
                    |> Async.retryResult 3 1500

                match doc with
                | Ok doc -> 
                    return
                        match getPostByViewPage this name baseUrl doc ratingIfNotSupportRating with
                        | Some x -> [x]
                        | None -> mapPage this rating baseUrl doc
                        |> Ok
                | Error x when x.Message.Contains "No Images Found" -> return Ok []
                | Error x when x.Message.Contains "No posts Found" -> return Ok []
                | Error x -> return Error x
            }

    let tags =
        asyncSeq {
            match!
                HtmlDocument.AsyncLoad $"{baseUrl}/tags"
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
            match ratingIfNotSupportRating with
            | None ->
                asyncSeq {
                    yield! requestPostList this Explicit "/rating:explicit"
                    yield! requestPostList this (Rating' "unknown") "/rating:unknown"
                    yield! requestPostList this Questionable "/rating:questionable"
                    yield! requestPostList this Safe "/rating:safe"
                }
            | Some r -> requestPostList this r ""

    interface ISearch with
        member this.Search search =
            match ratingIfNotSupportRating with
            | None ->
                match search.Rating with
                | x when x = Set.empty -> set [ Safe; Questionable; Explicit; Rating' "unknown" ]
                | x -> x
                |> AsyncSeq.ofSeq
                |> AsyncSeq.collect (fun rating ->
                    mapSearchOptions 
                        { search with 
                            Rating = set [rating]
                            Order = Default
                            NonTags = [] }
                    |> (+) "/"
                    |> requestPostList this rating)
            | Some rating ->
                mapSearchOptions { search with Rating = Set.empty }
                |> (+) "/"
                |>requestPostList this rating
            |> AntiGuro.antiThat search.NonTags

    interface ITags with
        member _.Tags = tags

    interface ISearchTag with
        member _.SearchTag s =
            tags 
            |> AsyncSeq.filter (function
                | Ok x when not <| x.Contains s -> false
                | _ -> true)

    interface IGetPostById with
        member x.GetPostById id =
            async {
                let fullUrl = $"{baseUrl}/post/view/{id}"
                let! html = HtmlDocument.AsyncLoad fullUrl
                return getPostByViewPage x name baseUrl html ratingIfNotSupportRating
            }
            |> Async.protect


let nekobooru = ShimmieSource ("Nekobooru", "https://neko-booru.com", None) :> ISource
let tentacleRape = ShimmieSource ("Tentacle Rape", "https://tentaclerape.net", Some Explicit) :> ISource
let fanservice = ShimmieSource ("Fan Service", "https://fanservice.fan", Some <| Rating' "Unknown") :> ISource


let sources = 
    [ nekobooru
      tentacleRape
      fanservice ]
