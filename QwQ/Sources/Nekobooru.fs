module QwQ.Sources.Nekobooru

open QwQ
open QwQ.Utils
open FSharp.Control
open FSharp.Data
open QwQ.Sources.Moebooru


// TODO: 此源在搜索结果只有一个页面时会直接跳转到ViewPage，需要从ViewPage解析。
// TODO: GetPostById可以通过硬爬ViewPage实现


let mapViewPage name baseUrl id =
    async {
        let src = $"{baseUrl}/post/view/{id}"
        let! page = HtmlDocument.AsyncLoad src
        
        let img = 
            page.CssSelect "#main_image"
            |> List.tryExactlyOne
            |> Option.bind (fun x -> x.TryGetAttribute "src")
            |> Option.map (fun x -> x.Value())
            |> Option.bind String.nullOrWhitespace
            |> Option.map (
                (+) baseUrl 
                >> mapHttpsContent HttpsOptions.Default 
                >> fun x -> { x with FileName = $"{name} {id}{System.IO.Path.GetExtension x.FileName}"})

        return (AsyncSeq.ofSeq <| Option.toList img), [src]
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


type Nekobooru (name, baseUrl) =

    let requestPostList this search rating =
        enumAllPages <| fun pageId ->
            async {
                let! doc = 
                    HtmlDocument.AsyncLoad $"{baseUrl}/post/list{search}/{1 + pageId}"
                    |> Async.protect
                    |> Async.retryResult 3 1500

                match doc with
                | Ok doc -> return Ok <| mapPage this rating baseUrl doc
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
                yield! requestPostList this "" Safe
                yield! requestPostList this "" Questionable
                yield! requestPostList this "" Explicit
            }

    interface ISearch with
        member this.Search search =
            let searchStr =
                mapSearchOptions 
                    { search with 
                        Rating = set [Safe; Questionable; Explicit]
                        Order = Default
                        NonTags = [] }
                |> (+) "/"

            search.Rating
            |> AsyncSeq.ofSeq
            |> AsyncSeq.collect (requestPostList this searchStr)
            |> AntiGuro.antiThat search.NonTags

    interface ITags with
        member _.Tags = tags

    interface ISearchTag with
        member _.SearchTag s =
            tags 
            |> AsyncSeq.filter (function
                | Ok x when not <| x.Contains s -> false
                | _ -> true)


let nekobooru = Nekobooru ("Nekobooru", "https://neko-booru.com") :> ISource


let sources = 
    [ nekobooru ]
