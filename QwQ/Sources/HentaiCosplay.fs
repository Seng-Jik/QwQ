module QwQ.Sources.HentaiCosplay

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


let baseUrl = "https://hentai-cosplays.com"


let httpsOption = HttpsOptions.Default


let parseSingleAttachmentPage (html: HtmlDocument) : Mipmaps =
    let a = html.CssSelect "#post #display_image_detail a" |> List.tryHead

    let small = 
        a
        |> Option.bind (fun a -> 
            a.CssSelect "img"
            |> List.tryHead)
        |> Option.bind (fun x -> x.TryGetAttribute "src")

    let big = 
        a
        |> Option.bind (fun a ->
            a.TryGetAttribute "href")

    [ small; big ] 
    |> List.choose id
    |> List.choose (fun x -> x.Value () |> String.nullOrWhitespace)
    |> List.map (mapHttpsContent httpsOption)
    |> AsyncSeq.ofSeq


let parseAttachmentPages (url: string) =
    async {
        let! html = HtmlDocument.AsyncLoad url

        let contents =
            Option.protect (fun () ->
                let ls = html.CssSelect "#main_contents > div > span > a"
                ls.[1 .. ^1])
            |> Option.defaultValue []
            |> Seq.choose (fun x -> x.TryGetAttribute "href")
            |> Seq.choose (fun x -> x.Value () |> String.nullOrWhitespace)
            |> Seq.map ((+) baseUrl)
            |> AsyncSeq.ofSeq
            |> AsyncSeq.mapAsyncParallel HtmlDocument.AsyncLoad
            |> AsyncSeq.append (AsyncSeq.singleton html)
            |> AsyncSeq.map parseSingleAttachmentPage

        return contents
    }

let parseDetailsPage (url: string) =
    async {
        let! html = HtmlDocument.AsyncLoad url

        let contentPage =
            html.CssSelect "#display_image_detail .icon-overlay a"
            |> Seq.choose (fun x -> x.TryGetAttribute "href")
            |> Seq.choose (fun x -> x.Value () |> String.nullOrWhitespace)
            |> Seq.filter (fun x -> x.StartsWith "/image/")
            |> Seq.tryHead
            |> Option.map ((+) baseUrl >> parseAttachmentPages)
            |> Option.defaultValue (async { return AsyncSeq.empty })

        let tags =
            html.CssSelect "#main_contents #detail_tag a"
            |> List.choose (fun x -> 
                x.InnerText()
                |> String.nullOrWhitespace)
            |> List.distinct


        return {| ContentPages = contentPage; Tags = tags |}
    }


let parsePostList (url: string) =
    async {
        let! html = HtmlDocument.AsyncLoad url

        return
            html.CssSelect "#center > #display_area_image #image-list .image-list-item"
            |> List.choose (fun div -> 
                let a = 
                    div.CssSelect ".image-list-item-title a"
                    |> List.tryHead

                let title =
                    a
                    |> Option.bind (fun x -> 
                        x.InnerText () 
                        |> String.nullOrWhitespace)
                    
                let detailsPage =
                    a
                    |> Option.bind (fun x -> x.TryGetAttribute "href")
                    |> Option.map (fun x -> x.Value())
                    |> Option.bind String.nullOrWhitespace
                    |> Option.map ((+) baseUrl)
                    
                let previewImg =
                    div.CssSelect ".image-list-item-image img"
                    |> List.tryHead
                    |> Option.bind (fun img -> img.TryGetAttribute "src")
                    |> Option.map (fun x -> x.Value ())
                    |> Option.bind String.nullOrWhitespace
                    
                detailsPage 
                |> Option.map (fun detailsPage ->
                    {| Title = title; Preview = previewImg; Details = detailsPage |}))
    }


let parsePostList' this (url: string) =
    asyncSeq {
        let! postList = parsePostList url

        yield!
            AsyncSeq.ofSeq postList
            |> AsyncSeq.mapAsyncParallel (fun post -> 
                async {
                    let! details = parseDetailsPage post.Details
                    let! content = details.ContentPages
                    return
                        { Id = post.Title.GetHashCode () |> uint64 
                          Title = post.Title
                          PreviewImage = post.Preview |> Option.map (mapHttpsContent httpsOption)
                          Source = this
                          Rating = Unrated
                          SourceUrl = AsyncSeq.singleton post.Details
                          Tags = details.Tags
                          Content = content }
                }
                |> Async.map List.singleton
                |> Async.protect)
    }


let allTags = 
    enumAllPages <| fun pageId ->
        async {
            let! html = HtmlDocument.AsyncLoad $"https://hentai-cosplays.com/tag/page/{pageId + 1}/"
            let tags =
                html.CssSelect "ul#tags li a"
                |> List.choose (fun x -> 
                    let x = x.InnerText()
                    let a = x.IndexOf '('
                    if a < 0
                    then x 
                    else x.[.. a - 1]
                    |> fun x -> x.Trim()
                    |> String.nullOrWhitespace)

            return tags
        }
        |> Async.protect
    |> AsyncSeq.collect (function
        | Ok x -> AsyncSeq.ofSeq x |> AsyncSeq.map Ok
        | Error e -> AsyncSeq.singleton <| Error e)


let enumAllPosts this (getUrlByPageId: int -> string) =
    enumAllPagesWithState (AsyncSeq.empty.GetEnumerator (), 0) <| fun (gen, pageId) _ ->
        async {
            match! gen.MoveNext () with
            | None ->
                let pageId = pageId + 1
                let gen = (parsePostList' this <| getUrlByPageId pageId).GetEnumerator ()
                match! gen.MoveNext () with
                | Some x -> return x |> Result.map (fun x -> (gen, pageId), x)
                | None -> return Ok ((AsyncSeq.empty.GetEnumerator (), pageId), [])
            | Some x -> return x |> Result.map (fun x -> (gen, pageId), x)
        }


let hentaicosplay =
    { new ISource with
          member _.Name = "Hentai Cosplay"
          member x.AllPosts = 
              enumAllPosts x <| fun pageId -> 
                  $"https://hentai-cosplays.com/search/page/{pageId + 1}"
          
      interface ITags with
          member _.Tags = allTags
          
      interface ISearch with
          member x.Search opt =
              if Seq.isEmpty opt.Tags
              then x.AllPosts
              else 
                  let term = Seq.reduce (fun a b -> a + "+" + b) opt.Tags
                  enumAllPosts x <| fun pageId ->
                      $"https://hentai-cosplays.com/search/keyword/{term}/page/{pageId + 1}/"
              |> AntiGuro.antiThat opt.ExludeTags }

