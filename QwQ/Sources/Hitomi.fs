module QwQ.Sources.Hitomi

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


type Json = JsonProvider<"./Sources/HitomiJsonSample.json">


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


let getContentsFromJson (postJson: Json.Root) =
    Result.protect (fun () ->
        let postId = uint32 postJson.Id
        postJson.Files
        |> Seq.map (fun x -> 
            let url = MagicJS.urlFromUrlFromHash postId x
            { FileName = x.Name
              DownloadMethod = 
                Https (url, { Headers = [ "referer", "https://hitomi.la/" ] }) }))


let parseDetailsJson (postId: uint32) =
    async {
        let! json =
            Http.AsyncRequestString $"https://ltn.hitomi.la/galleries/{postId}.js"

        let json = json.Replace("var galleryinfo =", "") |> Json.Parse

        let title = 
            json.JsonValue.TryGetProperty "title"
            |> Option.orElseWith (fun () -> json.JsonValue.TryGetProperty "japanese_title")
            |> Option.map (fun x -> x.AsString ())

        let tags = 
            json.Tags
            |> Seq.map (fun x -> x.Tag.Replace(' ', '_'))
            |> Seq.toList

        return title, tags, json
    }


let parseDetailsHtml this (postId: uint32) : Async<Result<Option<Post>, exn>> =
    async {
        let! html = 
            HtmlDocument.AsyncLoad $"https://ltn.hitomi.la/galleryblock/{postId}.html"

        let! json = 
            parseDetailsJson postId
            |> Async.protect 

        let title, tags, json = Result.unwrap json

        let preview =
            html.CssSelect ".dj-img-cont img"
            |> Seq.choose (fun x -> x.TryGetAttribute "src")
            |> Seq.map (fun x -> x.Value())
            |> Seq.choose String.nullOrWhitespace
            |> Seq.tryHead
            |> Option.map ((+) "https:" >> Moebooru.mapHttpsContent HttpsOptions.Empty)

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
                  getContentsFromJson json
                  |> Result.unwrap
                  |> Seq.map AsyncSeq.singleton
                  |> AsyncSeq.ofSeq }
    }
    |> Async.protect
    |> Async.map (function
        | Ok x -> Ok <| Some x
        | Error x when x.Message.Contains "404" -> Ok None
        | Error e -> Error e)


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
    seq { "alltags"; "allartists"; "allseries"; "allcharacters" }
    |> Seq.map (fun cat -> requestTags' cat tagPageName)
    |> AsyncSeq.ofSeqAsync


let convertTags: AsyncSeq<Result<string list, exn>> seq -> AsyncSeq<Result<Tag, exn>> =
    AsyncSeq.ofSeq
    >> AsyncSeq.concat
    >> AsyncSeq.map (function
        | Ok x -> Seq.map Ok x
        | Error x -> Seq.singleton <| Error x)
    >> AsyncSeq.concatSeq


let hitomi =
    { new ISource with
          member _.Name = "Hitomi"
          member this.AllPosts = 
              asyncSeq {
                  let! nozomiBin = 
                      Http.AsyncRequestStream "https://ltn.hitomi.la/index-all.nozomi"
                      |> Async.protect

                  match nozomiBin with
                  | Error x -> yield Error x
                  | Ok nozomiBin -> 
                      use bs = new System.IO.MemoryStream ()
                      nozomiBin.ResponseStream.CopyTo bs
                      let ids = bs.ToArray() |> Nozomi.parseNozomiBin

                      yield!
                          ids 
                          |> AsyncSeq.ofSeq
                          |> AsyncSeq.mapAsync (parseDetailsHtml this)
                          |> AsyncSeq.choose (function
                              | Ok (Some x) -> Some <| Ok [x]
                              | Ok None -> None
                              | Error x -> Some <| Error x)
              }

      interface IGetPostById with
          member this.GetPostById id =
              parseDetailsHtml (this :?> ISource) (id |> uint32)

      interface ITags with
          member _.Tags =
              seq { yield "123"; yield! seq { 'a'..'z' } |> Seq.map string }
              |> Seq.map requestTags
              |> convertTags

      interface ISearchTag with
          member x.SearchTag s = 
              match String.nullOrWhitespace <| String.trim s with
              | None -> (x :?> ITags).Tags
              | Some x -> 
                  match System.Char.ToLower <| x.[0] with
                  | x when Seq.exists ((=) x) (seq { '0' .. '9' }) -> requestTags "123"
                  | x when Seq.exists ((=) x) (seq { 'a' .. 'z' }) -> requestTags <| string x
                  | _ -> AsyncSeq.singleton <| Ok []
                  |> Seq.singleton
                  |> convertTags
                  |> AsyncSeq.filter (function
                      | Ok x -> x.StartsWith s
                      | Error _ -> true)
    }
