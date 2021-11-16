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

      Rating = mapRating post.Rating
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
          String.nullOrWhitespace post.PreviewUrl
          |> Option.map (mapHttpsContent httpsOpts)

      Content = 
          [ post.SampleUrl
            post.JpegUrl
            post.FileUrl ]
          |> AsyncSeq.ofSeq
          |> AsyncSeq.choose String.nullOrWhitespace
          |> AsyncSeq.map (mapHttpsContent httpsOpts >> AsyncSeq.singleton) }


let requestPosts loadJson map mapPost =
    async {
        let! json = 
            Async.protect loadJson
            |> Async.retryResult 3 1500

        return 
            json
            |> map
            |> Result.map (
                Seq.choose (fun json -> 
                    Option.protect (fun () -> mapPost json)))
    }


type MoebooruSourceOptions =
  { Name: string
    BaseUrl: string
    PostListJson: string
    SourceUrlGen: string -> PostId -> string
    HttpsOpts: HttpsOptions
    StartPageIndex: int
    OrderPopular: string
    OrderDate: string
    OrderScore: string }


let enumAllPages getPageByIndex =
    let rec enumPages errors curPage =
        asyncSeq {
            match! getPageByIndex curPage with
            | Ok x when Seq.isEmpty x && errors < 5 -> 
                yield Ok x
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


let mapOrder opts =
    function
    | Default -> None
    | Popular -> Some opts.OrderPopular
    | Date -> Some opts.OrderDate
    | Score -> Some opts.OrderScore


let mapSearchOptions opts searchOpt =
    searchOpt.NonTags
    |> Seq.map nonTag
    |> Seq.append searchOpt.Tags
    |> Seq.append (mapSearchRating searchOpt.Rating |> Option.toList)
    |> Seq.append (mapOrder opts searchOpt.Order |> Option.toList)
    |> Seq.fold (fun a b -> a + " " + b) ""


let limit = 500


type MoebooruSource (opts) =
    let sourceUrlGen = opts.SourceUrlGen opts.BaseUrl
    let mapPost this = mapPost opts.HttpsOpts this sourceUrlGen
    let requestPosts' this f = 
        enumAllPages <| fun pageId -> 
            requestPosts 
                (PostListJson.AsyncLoad(f pageId))
                id
                (mapPost this)

    let requestTags (urlPostfix: string) =
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

    let requestPostsWithPostfix this p =
        requestPosts' this <| fun pageId ->
            $"{opts.BaseUrl}{opts.PostListJson}?page={pageId + opts.StartPageIndex}&limit={limit}{p}"

    interface ISource with
        member _.Name = opts.Name
        member this.AllPosts = requestPostsWithPostfix this ""
            

    interface IGetPostById with
        member this.GetPostById x =
            async {
                match! 
                    requestPosts
                        (PostListJson.AsyncLoad(
                            $"{opts.BaseUrl}{opts.PostListJson}?tags=id:{x}"))
                        id
                        (mapPost this)
                with
                | Ok x -> return Ok <| Seq.tryHead x
                | Error e -> return Error e
            }
            
    interface ISearch with
        member this.Search search =
            requestPostsWithPostfix this $"&tags={mapSearchOptions opts search}"

    interface ITags with
        member _.Tags = requestTags ""

    interface ISearchTag with
        member _.SearchTag name = requestTags $"&name={name}"
            

let create x = MoebooruSource (x) :> ISource


let konachanLike name baseUrl = 
    { Name = name
      BaseUrl = baseUrl
      PostListJson = "/post.json"
      SourceUrlGen = sprintf "%s/post/show/%d"
      HttpsOpts = HttpsOptions.Default
      StartPageIndex = 1
      OrderPopular = "order:popular"
      OrderDate = "order:date"
      OrderScore = "order:score" }


let konachan = create <| konachanLike "Konachan" "https://konachan.com"
let yandere = create <| konachanLike "Yandere" "https://yande.re"
let lolibooru = create <| konachanLike "Lolibooru" "https://lolibooru.moe"
let hypnohub = create { konachanLike "HypnoHub" "https://hypnohub.net" with PostListJson = "/post/index.json" }


let sources =
    [ konachan
      yandere
      lolibooru
      hypnohub ]
    
