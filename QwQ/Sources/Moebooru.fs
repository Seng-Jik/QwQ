module QwQ.Sources.Moebooru

open FSharp.Control
open FSharp.Data
open QwQ
open QwQ.Utils


type PostListJson = JsonProvider<"https://konachan.net/post.json">


let mapTag (json: JsonValue) =
    json.["name"].AsString()


let mapRating =
    function
    | "s" -> Safe
    | "q" -> Questionable
    | "e" -> Explicit
    | x -> Rating' x


let normalizeFileName (x: string) = 
    [":";"*";"!";"#";"?";"%";"<";">";"|";"\"";"\\";"/";"\"";"\'"]
    |> List.fold (fun (s: string) (c: string) -> s.Replace (c,"")) x
    |> fun x -> x.Trim()


let getFileNameFromUrl (url: string) =
    let nameWithParam =
        let url = url.Replace('\\', '/')
        url.[1 + url.LastIndexOf '/'..] 
        |> System.Web.HttpUtility.UrlDecode
    let paramStart = nameWithParam.IndexOf '?'
    if paramStart < 0 then nameWithParam
    else nameWithParam.[..paramStart-1]
    |> normalizeFileName


let mapHttpsContent httpsOpts url =
    { DownloadMethod = Https (url, httpsOpts)
      FileName = getFileNameFromUrl url }


let mapPost httpsOpts source sourceUrlGen (post: PostListJson.Root) =
    { Id = uint64 post.Id
      Source = source 

      Rating = async { return mapRating post.Rating }
      SourceUrl = 
        [ yield (sourceUrlGen <| uint64 post.Id)
          yield! (
              match post.Source with
              | Some x when not <| System.String.IsNullOrWhiteSpace x ->
                [x]
              | _ -> []) ] 
        |> AsyncSeq.ofSeq

      Tags = post.Tags.Split(' ') |> AsyncSeq.ofSeq
      
      PreviewImage = 
        if System.String.IsNullOrWhiteSpace post.PreviewUrl
        then None
        else Some <| mapHttpsContent httpsOpts post.PreviewUrl

      Content = 
        [ post.SampleUrl
          post.JpegUrl
          post.FileUrl ]
        |> AsyncSeq.ofSeq
        |> AsyncSeq.filter (not << System.String.IsNullOrWhiteSpace)
        |> AsyncSeq.map (mapHttpsContent httpsOpts) }


let requestPosts httpsOpts source sourceUrlGen url =
    async {
        let! json = 
            Async.protect (PostListJson.AsyncLoad(url))
            |> Async.retryResult 3 1500

        return
            json
            |> Result.map (
                Seq.map 
                    (mapPost 
                        httpsOpts 
                        source 
                        sourceUrlGen))
    } : Async<Result<PostPage, exn>>


type MoebooruSourceOptions =
  { Name: string
    BaseUrl: string
    PostListJson: string
    SourceUrlGen: string -> PostId -> string
    HttpsOpts: HttpsOptions
    StartPageIndex: int }


let enumAllPages getPage =
    let rec enumPages errors curPage =
        asyncSeq {
            match! getPage curPage with
            | Ok x when Seq.isEmpty x && errors < 5 -> 
                yield Ok (x: PostPage)
                yield! enumPages (errors + 1) (curPage)
            | Ok x when Seq.isEmpty x -> yield Ok x
            | Ok x -> 
                yield Ok x
                yield! enumPages 0 (curPage + 1)
            | Error x when errors < 3 -> 
                yield Error x
                yield! enumPages (errors + 1) (curPage + 1)
            | Error x -> yield Error x
        }
    
    enumPages 0 0


let mapRatingToMetaTag =
    function
    | Explicit -> "e"
    | Safe -> "s"
    | Questionable -> "q"
    | Rating' x -> x
    >> (+) "rating:"


let nonTag = (+) "-"


exception DoNotSupportRatingException of string


let mapSearchRating r =
    if Seq.length r = 1
    then Some <| mapRatingToMetaTag (Seq.exactlyOne r)
    elif Seq.isEmpty r
    then None
    elif Seq.exists (function Rating' x -> true | _ -> false) r
    then Seq.pick (function Rating' x -> Some x | _ -> None) r
         |> DoNotSupportRatingException
         |> raise
    elif Seq.exists ((=) Explicit) r
      && Seq.exists ((=) Questionable) r
      && Seq.exists ((=) Safe) r
    then None
    elif Seq.exists ((=) Safe) r
      && Seq.exists ((=) Questionable) r
    then mapRatingToMetaTag Explicit |> nonTag |> Some
    elif Seq.exists ((=) Safe) r
      && Seq.exists ((=) Explicit) r
    then mapRatingToMetaTag Questionable |> nonTag |> Some
    elif Seq.exists ((=) Questionable) r
      && Seq.exists ((=) Explicit) r
    then mapRatingToMetaTag Safe |> nonTag |> Some
    else raise <| exn $"Can not process {r}"


let mapOrder =
    function
    | Default -> None
    | Popular -> Some "order:score"


let mapSearchOptions searchOpt =
    searchOpt.NonTags
    |> Seq.map nonTag
    |> Seq.append searchOpt.Tags
    |> Seq.append (mapSearchRating searchOpt.Rating |> Option.toList)
    |> Seq.append (mapOrder searchOpt.Order |> Option.toList)
    |> Seq.fold (fun a b -> a + " " + b) ""


type MoebooruSource (opts) =
    let sourceUrlGen = opts.SourceUrlGen opts.BaseUrl
    let requestPosts' this f = 
        enumAllPages <| fun pageId -> 
            requestPosts 
                opts.HttpsOpts
                this
                sourceUrlGen
                (f pageId)

    let requestTags this (urlPostfix: string) =
        asyncSeq {
                match!
                    JsonValue.AsyncLoad($"{opts.BaseUrl}/tag.json?limit=0{urlPostfix}")
                    |> Async.protect
                    |> Async.retryResult 3 500
                with
                | Error x -> yield Error x
                | Ok x -> 
                    yield! 
                        x.AsArray ()
                        |> AsyncSeq.ofSeq 
                        |> AsyncSeq.map (mapTag >> Ok)
            }

    interface ISource with
        member _.Name = opts.Name
        member this.AllPosts = 
            requestPosts' this <| fun pageId ->
                $"{opts.BaseUrl}{opts.PostListJson}?page={pageId + opts.StartPageIndex}"

    interface IGetPostById with
        member this.GetPostById x =
            async {
                match! 
                    requestPosts 
                        opts.HttpsOpts
                        this
                        sourceUrlGen
                        $"{opts.BaseUrl}{opts.PostListJson}?tags=id:{x}"
                with
                | Ok x -> return Ok <| Seq.tryHead x
                | Error e -> return Error e
            }
            

    interface ISearch with
        member this.Search search =
            let tagString = mapSearchOptions search
            requestPosts' this <| fun pageId ->
                $"{opts.BaseUrl}{opts.PostListJson}?page={pageId + opts.StartPageIndex}&tags={tagString}"

    interface ITags with
        member this.Tags = requestTags this ""

    interface ISearchTag with
        member this.SearchTag name = requestTags this $"&name={name}"
            

let create x = MoebooruSource (x) :> ISource


let konachanLike name baseUrl = 
    { Name = name
      BaseUrl = baseUrl
      PostListJson = "/post.json"
      SourceUrlGen = sprintf "%s/post/show/%d"
      HttpsOpts = HttpsOptions.Default
      StartPageIndex = 1 }


let konachan = create <| konachanLike "Konachan" "https://konachan.com"
let yandere = create <| konachanLike "Yandere" "https://yande.re"
let lolibooru = create <| konachanLike "Lolibooru" "https://lolibooru.moe"
let hypnohub = create { konachanLike "HypnoHub" "https://hypnohub.net" with PostListJson = "/post/index.json" }


let sources =
    [ konachan
      yandere
      lolibooru
      hypnohub ]
    
