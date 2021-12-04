module QwQ.Sources.TheHentaiWorld

open FSharp.Data
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open FSharp.Control


let tags = 
    asyncSeq {
        match!
            HtmlDocument.AsyncLoad "https://thehentaiworld.com/tags/"
            |> Async.protect
        with
        | Ok tags ->
            yield!
                tags.CssSelect "#tag-list a"
                |> Seq.choose (fun x -> x.InnerText().Trim().Replace(" ", "_") |> String.nullOrWhitespace)
                |> Seq.map Ok
                |> AsyncSeq.ofSeq
        | Error e -> yield Error e
    }


let getContentFromDetailsPage (html: HtmlDocument) : Mipmaps =
    let a =
        html.CssSelect "#doujin a"
        |> List.tryLast

    let bigImage = 
        a 
        |> Option.bind (fun x -> x.TryGetAttribute "href")

    let smallImage =
        a
        |> Option.bind (fun a -> a.CssSelect "img" |> List.tryHead)
        |> Option.bind (fun img -> img.TryGetAttribute "src")

    let video =
        html.CssSelect "#video source"
        |> List.choose (fun x -> x.TryGetAttribute "src")
        |> List.map Some

    [ smallImage; bigImage; yield! video ] 
    |> List.choose id
    |> List.choose (fun x -> x.Value () |> String.nullOrWhitespace)
    |> Seq.map (mapHttpsContent HttpsOptions.Empty)
    |> AsyncSeq.ofSeq


let parseDetailsPage (url: string) =    
    async {
        let! html = HtmlDocument.AsyncLoad url

        let tags = 
            html.CssSelect "#tags a" 
            |> List.choose (fun x -> 
                x.InnerText().Replace(" ", "_") 
                |> String.trim 
                |> String.nullOrWhitespace)

        let thisPageMipmaps = getContentFromDetailsPage html

        let moreContents =
            html.CssSelect "#miniThumbContainer a"
            |> List.choose (fun x -> x.TryGetAttribute "href")
            |> List.choose (fun x -> x.Value () |> String.nullOrWhitespace)
            |> function
                | [] -> AsyncSeq.empty
                | _ :: ls ->
                    ls
                    |> AsyncSeq.ofSeq
                    |> AsyncSeq.mapAsyncParallel (fun url -> 
                        HtmlDocument.AsyncLoad url
                        |> Async.map getContentFromDetailsPage)

        let allContents = AsyncSeq.append (AsyncSeq.singleton thisPageMipmaps) moreContents

        return {| Tags = tags; Contents = allContents |}
    }
    |> Async.protect


let parsePostListPage (url: string) =
    async {
        let! html = HtmlDocument.AsyncLoad url

        return
            html.CssSelect "#thumbContainer div.thumb a"
            |> List.choose (fun a -> 
                let thumbnail = 
                    a.CssSelect "img"
                    |> List.tryHead
                    |> Option.bind (fun x -> x.TryGetAttribute "src")
                    |> Option.bind (fun x -> x.Value () |> String.nullOrWhitespace)
                    |> Option.map (mapHttpsContent HttpsOptions.Empty)
                    
                let title = 
                    a.TryGetAttribute "title"
                    |> Option.bind (fun x -> x.Value () |> String.nullOrWhitespace)

                let detailsPage =
                    a.TryGetAttribute "href"
                    |> Option.bind (fun x -> x.Value () |> String.nullOrWhitespace)

                detailsPage |> Option.map (fun detailsPage ->
                    {| Title = title; Thumbnail = thumbnail; DetailsPage = detailsPage |}))
    }
    |> Async.protect


let getPostsFromPostListPage this (url: string) =
    asyncSeq {
        let! postPageList = parsePostListPage url

        match
            result {
                let! postPageList = postPageList
                return
                    postPageList
                    |> AsyncSeq.ofSeq
                    |> AsyncSeq.mapAsyncParallel (fun x -> 
                        async {
                            let! post = parseDetailsPage x.DetailsPage
                        
                            return result {
                                let! post = post
                                return 
                                    { Id = uint64 <| x.Title.GetHashCode()
                                      Title = x.Title
                                      Source = this 
                                      Rating = Unrated
                                      SourceUrl = AsyncSeq.singleton x.DetailsPage
                                      Tags = post.Tags
                                      PreviewImage = x.Thumbnail
                                      Content = post.Contents }
                            }
                        })
            }
        with 
        | Error e -> yield Error e
        | Ok x -> 
            yield! 
                x |> AsyncSeq.map (Result.map List.singleton)
    }


let enumAllPages this postfix =
    enumAllPagesWithState (AsyncSeq.empty.GetEnumerator (), 0) <| fun (gen, pageId) _ ->
        async {
            match! gen.MoveNext () with
            | Some cur -> return cur |> Result.map (fun cur -> (gen, pageId), cur)
            | None -> 
                let pageId = pageId + 1
                let url = $"https://thehentaiworld.com/page/{pageId}/{postfix}"
                let gen = (getPostsFromPostListPage this url).GetEnumerator ()
                match! gen.MoveNext () with
                | Some a -> return a |> Result.map (fun a -> (gen, pageId), a)
                | None -> return Ok ((AsyncSeq.empty.GetEnumerator (), pageId), [])
        }


let thehentaiworld = 
    { new ISource with
          member _.Name = "The Hentai World"
          member this.AllPosts = enumAllPages this "?new"
              
      interface ISearch with
          member this.Search opt =
              if Seq.isEmpty opt.Tags
              then this.AllPosts
              else
                  opt.Tags
                  |> Seq.reduce (fun a b -> a + "+" + b)
                  |> (+) "?s="
                  |> enumAllPages this
              |> AntiGuro.antiThat opt.ExludeTags
                  
      interface ITags with
          member _.Tags = tags

      interface ISearchTag with
          member t.SearchTag term = Tags.slowSearchTag term (t :?> ITags)
    }