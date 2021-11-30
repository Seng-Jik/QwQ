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
        return mapHttpsContent HttpsOptions.Default url
    }


let getViewPageUrlFromPostId (postId: PostId) =
    $"https://www.lolibaka.com/post/show/{postId}"


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
                      |> Option.map (mapHttpsContent HttpsOptions.Default)

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
            let! page =
                HtmlDocument.AsyncLoad    
                    $"https://www.lolibaka.com/post/list#post/list/{tags}/{pageId + 1}"
                |> Async.protect
            
            return Result.map (mapPostList this) page
        }


type LolibakaSource () =

    interface ISource with
        member _.Name = "Lolibaka"
        member x.AllPosts = requestPostList x ""

    // interface ISearch with
    // interface IGetPostById with


let lolibaka = LolibakaSource () :> ISource
