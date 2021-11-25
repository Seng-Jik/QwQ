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
            |> List.exactlyOne
            |> HtmlNode.tryGetAttribute "href"
            |> Option.unwrap
            |> HtmlAttributeExtensions.Value)
        |> Option.bind String.nullOrWhitespace


    AsyncSeq.ofSeq <| Option.toList img, [fullUrl] @ Option.toList source


let getPostByViewPage source name baseUrl (page: HtmlDocument) =
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
            infoTable.["Tags"]
            |> List.map (fun x -> x.InnerText ())

        let rating =
            match infoTable.["Rating"].Head.InnerText().Trim() with
            | "Safe" -> Safe
            | "Questionable" -> Questionable
            | "Explicit" -> Explicit
            | x -> Rating' x

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
    |> Seq.choose (fun node -> 
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
              Tags = tags |> Option.defaultValue [||]
              PreviewImage = img |> Option.map (mapHttpsContent HttpsOptions.Default)
              Content = asyncSeq { let! a, _ = mapViewPage source.Name baseUrl id in yield a } } ))


type ShimmieSource (name, baseUrl) =

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
                        match getPostByViewPage this name baseUrl doc with
                        | Some x -> Seq.singleton x
                        | None -> mapPage this rating baseUrl doc
                        |> Ok
                | Error x when x.Message.Contains "No Images Found" -> return Ok Seq.empty
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
            asyncSeq {
                yield! requestPostList this Safe ""
                yield! requestPostList this Questionable ""
                yield! requestPostList this Explicit ""
                yield! requestPostList this (Rating' "unknown") ""
            }

    interface ISearch with
        member this.Search search =
            search.Rating
            |> AsyncSeq.ofSeq
            |> AsyncSeq.collect (fun rating ->
                mapSearchOptions 
                    { search with 
                        Rating = set [rating]
                        Order = Default
                        NonTags = [] }
                |> (+) "/"
                |> requestPostList this rating)
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
                return getPostByViewPage x name baseUrl html
            }
            |> Async.protect


let nekobooru = ShimmieSource ("Nekobooru", "https://neko-booru.com") :> ISource


let sources = 
    [ nekobooru ]
