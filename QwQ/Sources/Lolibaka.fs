module QwQ.Sources.Lolibaka

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


let getContentFromViewPage (viewPage: HtmlDocument) =
    option {
        let! img =
            viewPage.CssSelect "#gallery img"
            |> List.tryHead

        let! attr = img.TryGetAttribute "src"
        let url = attr.Value ()
        return mapHttpsContent HttpsOptions.Empty url
    }


let getViewPageUrlFromPostId (postId: PostId) =
    $"https://www.lolibaka.com/post/show/{postId}"


let mapViewPageToPost this srcUrl =
    async {
        let! viewPage = 
            HtmlDocument.AsyncLoad srcUrl
            |> Async.protect
            |> Async.map Option.ofResult

        return option {
            let! viewPage = viewPage

            let tags = 
                viewPage.CssSelect ".sidebar-content li.nav-item a span"
                |> List.filter (fun x -> not <| x.HasClass "badge")
                |> List.choose (fun x -> 
                    x.InnerText()
                    |> String.nullOrWhitespace)
                |> List.map (fun x -> x.Replace(' ', '_'))

            let infos =
                viewPage.CssSelect "ul.list-group li"
                |> List.choose (fun x -> x.InnerText () |> String.nullOrWhitespace)
                |> List.choose (fun x -> 
                    let x = x.Split ':'
                    option {
                        let! key = Array.tryItem 0 x
                        let! value = Array.tryItem 1 x
                        return key.Trim(), value.Trim()
                    })
                |> Map.ofList

            let! postId = Map.tryFind "Id" infos
            let! postId = Option.protect (fun () -> uint64 postId)
            return 
                { Id = postId
                  Title = None
                  Source = this
                  Rating = 
                      Map.tryFind "Rating" infos
                      |> Option.map (function
                          | "Safe" -> Safe
                          | "Questionable" -> Questionable
                          | "Explicit" -> Explicit
                          | _ -> Unrated)
                      |> Option.defaultValue Unrated
                  SourceUrl = AsyncSeq.singleton srcUrl
                  Tags = tags
                  PreviewImage = None
                  Content = 
                      getContentFromViewPage viewPage
                      |> Option.toList
                      |> AsyncSeq.ofSeq
                      |> AsyncSeq.singleton }
        }
            
    }


let mapPostList this (page: HtmlDocument) =
    page.CssSelect ".lolipostlistbox"
    |> List.choose (fun li ->
        option {
            let! postId = li.TryGetAttribute "id"
            let! postId = Option.protect (postId.Value >> uint64 >> (+) 1UL)
            let! a = 
                li.Elements () 
                |> List.tryHead 
                |> Option.filter (fun x -> x.HasName "a")

            let sourceUrl =
                li.TryGetAttribute "href"
                |> Option.map (fun x -> x.Value ())

            let! img = 
                a.Elements ()
                |> List.tryHead
                |> Option.filter (fun x -> x.HasName "img")

            let previewImgUrl =
                img.TryGetAttribute "src"
                |> Option.map (fun x -> x.Value ())

            let tags = 
                img.TryGetAttribute "alt"
                |> Option.map (fun x -> 
                    let alt = x.Value ()
                    let pos = alt.IndexOf "//"
                    let alt = 
                        if pos < 0
                        then alt
                        else alt.[..pos]
                    
                    alt.Trim().Split ' '
                    |> Seq.choose String.nullOrWhitespace
                    |> Seq.map String.trim)
                |> Option.defaultValue Seq.empty

            return 
                { Id = postId
                  SourceUrl = AsyncSeq.ofSeq <| Option.toList sourceUrl
                  Title = None
                  Rating = Unrated
                  Source = this
                  Tags = List.ofSeq tags
                  PreviewImage = 
                      previewImgUrl
                      |> Option.map (mapHttpsContent HttpsOptions.Empty)

                  Content = 
                      asyncSeq {
                          let url =
                              sourceUrl
                              |> Option.defaultValue (getViewPageUrlFromPostId postId)

                          let! page = HtmlDocument.AsyncLoad url

                          page
                          |> getContentFromViewPage
                          |> Option.toList
                          |> AsyncSeq.ofSeq
                      } 
                }
        })


let requestPostList this tags =
    enumAllPages <| fun pageId ->
        async {
            let url = $"https://www.lolibaka.com/post/list/{tags}/{pageId + 1}"
            let! page =
                HtmlDocument.AsyncLoad url
                |> Async.protect
            
            return Result.map (mapPostList this) page
        }


type LolibakaSource () =

    interface ISource with
        member _.Name = "Lolibaka"
        member x.AllPosts = requestPostList x ""

    interface ISearchTag with
        member _.SearchTag term =
            asyncSeq {
                match!
                    JsonValue.AsyncLoad $"https://www.lolibaka.com/search.php?term={term}"
                    |> Async.protect
                with
                | Ok json ->
                    yield!
                        json.AsArray ()
                        |> Seq.choose (fun x -> 
                            x.TryGetProperty "value"
                            |> Option.orElseWith (fun () -> x.TryGetProperty "label"))
                        |> Seq.choose (fun x -> String.nullOrWhitespace <| x.AsString())
                        |> Seq.map Ok
                        |> AsyncSeq.ofSeq
                | Error e -> yield Error e
            }

    interface ISearch with
        member x.Search opt =
            let firstTag = Seq.tryHead opt.Tags |> Option.defaultValue ""
            let nextTags = 
                if Seq.length opt.Tags > 0
                then Seq.skip 1 opt.Tags
                else Seq.empty

            requestPostList x firstTag
            |> AntiGuro.antiThat opt.NonTags
            |> AsyncSeq.map (
                Result.map (
                    List.filter <| fun post -> 
                        Seq.forall (fun t -> List.contains t post.Tags) nextTags))
            
    interface IGetPostById with
        member x.GetPostById id =
            mapViewPageToPost x $"https://www.lolibaka.com/post/show/{id}"
            |> Async.map Ok


let lolibaka = LolibakaSource () :> ISource
