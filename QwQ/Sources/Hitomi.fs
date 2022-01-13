module QwQ.Sources.Hitomi

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open QwQ.Sources.Nozomi


type Json = JsonProvider<"./Sources/HitomiJsonSample.json">


let referer = "https://hitomi.la/"


module private MagicJS =
    open System.Text.RegularExpressions
    // Magic JS Code
    // Maybe throw some Exception!!!!!

    let subdomainFromUrlRegex =
        Regex @"\/[0-9a-f]\/([0-9a-f]{2})\/"


    let subdomainFromUrl url =
//  function subdomain_from_url(url, base) {
//      var retval = 'b';
        let mutable retval = "b"
//      if (base) {
//              retval = base;
//      }
            
//      var number_of_frontends = 2;
        let number_of_frontends = 2
//      var b = 16;
            
//      var r = /\/[0-9a-f]\/([0-9a-f]{2})\//;
//      var m = r.exec(url);
//      if (!m) {
//              return 'a';
//      }
        let m = 
            Option.protect (fun () -> 
                subdomainFromUrlRegex.Match(url).Groups.[1].Value)
            
//      var g = parseInt(m[1], b);
        let g = 
            m
            |> Option.bind (fun m -> Option.protect(fun () -> 
                System.Int32.Parse(m, System.Globalization.NumberStyles.HexNumber)))

//      if (!isNaN(g)) {
//              var o = 0;
//              if (g < 0x7c) {
//                      o = 1;
//              }
//              retval = String.fromCharCode(97 + o) + retval;
//      }

        match g with
        | None -> ()
        | Some g ->
            let mutable o = 0
            if g < 0x7c then
                o <- 1
            retval <- $"{char (97 + o)}{retval}"
            
//      return retval;
        retval
//  }


    let urlFromUrlRegex = Regex @"\/\/..?\.hitomi\.la\/"

    
    let urlFromUrl url = 
//  function url_from_url(url, base) {
//      return url.replace(/\/\/..?\.hitomi\.la\//, '//'+subdomain_from_url(url, base)+'.hitomi.la/');
        urlFromUrlRegex.Replace(url,  "//" + subdomainFromUrl url + ".hitomi.la/")
//  }


    let fullPathFromHash hash = 
//  function full_path_from_hash(hash) {
//      if (hash.length < 3) {
//          return hash;
//      }
//      return hash.replace(/^.*(..)(.)$/, '$2/$1/'+hash);
//  }
        if String.length hash < 3
        then hash
        else $"{hash.[^0]}/{hash.[^2..^1]}/{hash}"


    let urlFromHash (image: Json.File) = 
//  function url_from_hash(galleryid, image, dir, ext) {
//      ext = ext || dir || image.name.split('.').pop();
        let ext = image.Name.Split '.' |> Seq.skip 1 |> Seq.head
//      dir = dir || 'images';
        let dir = "images"
        
//      return 'https://a.hitomi.la/'+dir+'/'+full_path_from_hash(image.hash)+'.'+ext;
        "https://a.hitomi.la/" + dir + "/" + fullPathFromHash image.Hash + "." + ext
//  }
        

    let urlFromUrlFromHash (postId: uint32) (image: Json.File) : string =
//  function url_from_url_from_hash(galleryid, image, dir, ext, base) {
//      return url_from_url(url_from_hash(galleryid, image, dir, ext), base);
        urlFromUrl (urlFromHash image)
//  }

//  url_from_url_from_hash(galleryid, image);


let hitomiHttpOption = { Headers = [ "referer", referer ]}


let getContentsFromJson (postJson: Json.Root) =
    Result.protect (fun () ->
        let postId = uint32 postJson.Id
        postJson.Files
        |> Seq.map (fun x -> 
            let url = MagicJS.urlFromUrlFromHash postId x
            { FileName = x.Name
              DownloadMethod = 
                Https (url, hitomiHttpOption) }))


let requestDetailsJson (postId: uint32) =
    async {
        let! json =
            Http.AsyncRequestString 
                ($"https://ltn.hitomi.la/galleries/{postId}.js", headers = hitomiHttpOption.Headers)

        let json = json.Replace("var galleryinfo =", "") |> Json.Parse

        (*let title = 
            json.JsonValue.TryGetProperty "title"
            |> Option.orElseWith (fun () -> json.JsonValue.TryGetProperty "japanese_title")
            |> Option.map (fun x -> x.AsString ())

        let tags = 
            json.Tags
            |> Seq.map (fun x -> x.Tag.Replace(' ', '_'))
            |> Seq.toList*)

        return json
    }


let parseDetailsHtml this (postId: uint32) : Async<Result<Option<Post>, exn>> =
    async {
        let! htmlStr = 
            Http.AsyncRequestString 
                ($"https://ltn.hitomi.la/galleryblock/{postId}.html", 
                 headers = [ "referer", "https://hitomi.la"])

        let html = 
            HtmlDocument.Parse htmlStr

        let title = 
            html.CssSelect "h1 a"
            |> Seq.tryHead
            |> Option.bind (fun x -> String.nullOrWhitespace <| x.InnerText ())

        let artist =
            html.CssSelect ".artist-list a"

        let tags =
            html.CssSelect ".dj-desc td ul li a"
            |> List.append artist
            |> List.choose (fun x -> 
                x
                    .InnerText()
                    .Replace("♀", "")
                    .Replace("♂", "")
                    .Trim()
                    .Replace(' ', '_')
                |> String.nullOrWhitespace)

        let preview =
            html.CssSelect ".dj-img-cont img"
            |> Seq.choose (fun x -> x.TryGetAttribute "src")
            |> Seq.map (fun x -> x.Value())
            |> Seq.choose String.nullOrWhitespace
            |> Seq.tryHead
            |> Option.map ((+) "https:" >> Moebooru.mapHttpsContent hitomiHttpOption)

        let hitomiViewPage = 
            html.CssSelect "a" 
            |> Seq.choose (fun x -> x.TryGetAttribute "href")
            |> Seq.choose (fun x -> String.nullOrWhitespace <| x.Value())
            |> Seq.head
            |> (+) "https://hitomi.la"

        return 
            { Id = uint64 postId 
              Title = title
              Source = this
              Rating = Unrated
              SourceUrl = AsyncSeq.singleton hitomiViewPage
              Tags = tags
              PreviewImage = preview
              Content = 
                  asyncSeq {
                      let! json = 
                          requestDetailsJson postId
                          |> Async.protect

                      let json = Result.unwrap json

                      yield!
                          getContentsFromJson json
                          |> Result.unwrap
                          |> Seq.map AsyncSeq.singleton
                          |> AsyncSeq.ofSeq
                  } 
            }
    }
    |> Async.protect
    |> Async.map (function
        | Ok x -> Ok <| Some x
        | Error x when x.Message.Contains "404" -> Ok None
        | Error e -> Error e)


let mapOrdering order =
    match order with
    | Popular | Score -> "popular/week/"
    | _ -> ""


let allPosts this (nozomiCache: AsyncCache<string, uint32 seq>) order =
    asyncSeq {
        let! nozomiBin = 
            nozomiCache.GetAsyncProtected <| 
                match order with
                | Popular | Score -> "https://ltn.hitomi.la/popular/week-all.nozomi"
                | _ -> "https://ltn.hitomi.la/index-all.nozomi"
            
        match nozomiBin with
        | Error x -> yield Error x
        | Ok nozomiBin -> 
            yield!
                nozomiBin 
                |> AsyncSeq.ofSeq
                |> AsyncSeq.mapAsync (parseDetailsHtml this)
                |> AsyncSeq.choose (function
                    | Ok (Some x) -> Some <| Ok [x]
                    | Ok None -> None
                    | Error x -> Some <| Error x)
    }


let requestTags' (cat: string) (tagPageName: string) =
    async {
        let! html =
            $"https://hitomi.la/{cat}-{tagPageName}.html"
            |> HtmlDocument.AsyncLoad

        return
            html.CssSelect "ul.posts li a"
            |> List.map (fun x -> 
                x
                    .InnerText()
                    .Replace("♀", "")
                    .Replace("♂", "")
                    .Trim()
                    .Replace(' ', '_'))
    }
    |> Async.protect


let requestTags tagPageName =
    seq { "allseries"; "allcharacters"; "alltags"; "allartists" }
    |> Seq.map (fun cat -> requestTags' cat tagPageName |> Async.map (Result.map (List.map (fun x -> cat, x))))
    |> AsyncSeq.ofSeqAsync


let convertTags: AsyncSeq<Result<(string * string) list, exn>> seq -> AsyncSeq<Result<string * Tag, exn>> =
    AsyncSeq.ofSeq
    >> AsyncSeq.concat
    >> AsyncSeq.map (function
        | Ok x -> Seq.map Ok x
        | Error x -> Seq.singleton <| Error x)
    >> AsyncSeq.concatSeq


let allTags  =
    seq { yield "123"; yield! seq { 'a'..'z' } |> Seq.map string }
    |> Seq.map requestTags
    |> convertTags


let searchTag (tagTerm: string) =
    match String.nullOrWhitespace <| String.trim tagTerm with
    | None -> allTags
    | Some x -> 
        match System.Char.ToLower <| x.[0] with
        | x when Seq.exists ((=) x) (seq { '0' .. '9' }) -> requestTags "123"
        | x when Seq.exists ((=) x) (seq { 'a' .. 'z' }) -> requestTags <| string x
        | _ -> AsyncSeq.singleton <| Ok []
        |> Seq.singleton
        |> convertTags
        |> AsyncSeq.filter (function
            | Ok (_, x) -> x.StartsWith tagTerm
            | Error _ -> true)


let hitomi =
    let nozomiCache = newNozomiCache "https://hitomi.la/"
    { new ISource with
          member _.Name = "Hitomi"
          member this.AllPosts = allPosts this nozomiCache Date              

      interface IGetPostById with
          member this.GetPostById id =
              parseDetailsHtml (this :?> ISource) (id |> uint32)

      interface ISearch with
          member this.Search opt = 
              let requestNozomiFromTag (cat, tag: string) =
                  let cat = 
                      match cat with
                      | "alltags" -> "tag"
                      | "allartists" -> "artist"
                      | "allseries" -> "series"
                      | "allcharacters" -> "character"
                      | x -> failwith $"What is cat of {x}???"

                  $"https://ltn.hitomi.la/{cat}/{mapOrder opt.Order}{tag.Replace('_', ' ')}-all.nozomi"
                  |> nozomiCache.GetAsyncProtected
                  |> Async.map (function
                      | Ok x -> Ok x
                      | Error e when e.Message.Contains "404" -> Ok Seq.empty
                      | Error e -> Error e)

              if Seq.length opt.Tags = 0 
              then this.AllPosts
              else
                  let tags = opt.Tags |> Seq.map searchTag |> AsyncSeq.ofSeq

                  let processOrGroup (orGroup: AsyncSeq<Result<string * Tag, exn>>) : Async<Result<uint32 seq, exn>> =
                      async {
                          let! postIds =
                              orGroup
                              |> AsyncSeq.map (Result.map (requestNozomiFromTag))
                              |> AsyncSeq.toArrayAsync
                          
                          let postIdOks, errs1 = Result.eitherArray postIds
                          let! postIdOks = postIdOks |> Async.Parallel
                          let postIdOks, errs2 = Result.eitherArray postIdOks
                          
                          if Array.forall (Seq.isEmpty >> not) postIdOks
                          then return Ok <| Seq.concat postIdOks
                          else 
                              if errs1 |> Array.isEmpty |> not then return Error errs1.[0]
                              elif errs2 |> Array.isEmpty |> not then return Error errs2.[0]
                              else return Ok Seq.empty
                      }

                  asyncSeq {
                        let! tags = AsyncSeq.map processOrGroup tags |> AsyncSeq.toArrayAsync
                        let! tags = tags |> Async.Parallel

                        let tagOks, tagErrs = Result.eitherArray tags

                        if Array.isEmpty tagErrs |> not
                        then yield! tagErrs |> Array.map Error |> AsyncSeq.ofSeq
                        else 
                            let first = Array.head tagOks
                            let tail = Seq.tail tagOks 
                                  
                            yield!
                                first
                                |> Seq.filter (fun postId ->
                                    Seq.forall (fun x -> Seq.exists ((=) postId) x) tail)
                                |> Seq.map (parseDetailsHtml this)
                                |> AsyncSeq.ofSeqAsync
                                |> AsyncSeq.choose (function
                                    | Ok None -> None
                                    | Ok (Some x) -> Some <| Ok [x]
                                    | Error x -> Some <| Error x)
                  }
              |> AntiGuro.antiThat opt.ExludeTags


      interface ITags with
          member _.Tags =
              allTags
              |> AsyncSeq.map (Result.map snd)

      interface ISearchTag with
          member _.SearchTag s = 
              searchTag s
              |> AsyncSeq.map (Result.map snd)

    }
