module QwQ.Sources.Moebooru

open FSharp.Control
open FSharp.Data
open QwQ
open QwQ.Utils


type PostListJson = JsonProvider<"./Sources/MoebooruSample.json">


let mapRating =
    function
    | "s" -> Safe
    | "q" -> Questionable
    | "e" -> Explicit
    | _ -> Unrated


let normalizeFileName (x: string) = 
    [";";":";"*";"!";"#";"?";"%";"<";">";"|";"\"";"\\";"/";"\"";"\'";"^"]
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


let parseTags (x: string) = 
    x.Split(' ') |> Array.toList


let mapPost httpsOpts source sourceUrlGen (post: PostListJson.Root) =
    { Id = uint64 post.Id
      Source = source 
      Title = None

      Rating = mapRating post.Rating
      SourceUrl = 
          [ yield (sourceUrlGen <| uint64 post.Id)
            yield! (
                match post.Source with
                | Some x when not <| System.String.IsNullOrWhiteSpace x ->
                  [x]
                | _ -> []) ] 
          |> AsyncSeq.ofSeq

      Tags = parseTags post.Tags
      
      PreviewImage = 
          String.nullOrWhitespace post.PreviewUrl
          |> Option.map (mapHttpsContent httpsOpts)

      Content = 
          asyncSeq {
              post.SampleUrl
              post.JpegUrl
              post.FileUrl 
          }
          |> AsyncSeq.choose String.nullOrWhitespace
          |> AsyncSeq.map (mapHttpsContent httpsOpts)
          |> AsyncSeq.singleton }


let requestPosts loadJson map mapPost =
    async {
        let! json = 
            Async.protect loadJson
            |> Async.retryResult 3 1500

        return 
            json
            |> map
            |> Result.map (
                List.choose (fun json -> 
                    Option.protect (fun () -> mapPost json)))
    }


type MoebooruSourceOptions =
  { Name: string
    BaseUrl: string
    PostListJson: string
    SourceUrlGen: string -> PostId -> string
    HttpsOpts: HttpsOptions
    StartPageIndex: int }


let enumAllPagesWithState initState getPageByIndex =
    let rec enumPages prevState errors curPage =
        asyncSeq {
            match! getPageByIndex prevState curPage with
            | Ok (nextState, x) when List.isEmpty x && errors < 5 -> 
                yield Ok x
                yield! enumPages nextState (errors + 1) (curPage + 1)
            | Ok (_, x) when List.isEmpty x -> yield Ok x
            | Ok (nextState, x) -> 
                yield Ok x
                yield! enumPages nextState 0 (curPage + 1)
            | Error x when errors < 3 -> 
                yield Error x
                yield! enumPages prevState (errors + 1) curPage
            | Error x -> yield Error x
        }
    
    enumPages initState 0 0


let enumAllPages getPageByIndex =
    enumAllPagesWithState 
        () 
        (fun () -> 
            getPageByIndex 
            >> Async.map (Result.map (fun x -> (), x)))


let mapRatingToMetaTag =
    function
    | Explicit -> Some "rating:explicit"
    | Safe -> Some "rating:safe"
    | Questionable -> Some "rating:questionable"
    | Unrated -> None


let nonTag = (+) "-"


exception DoNotSupportRatingException of string


let mapOrder =
    function
    | Default -> None
    | Popular -> Some "order:popular"
    | Date -> Some "order:date"
    | Score -> Some "order:score"


let mapSearchOptions searchOpt =
    searchOpt.ExludeTags
    |> Seq.map nonTag
    |> Seq.append searchOpt.Tags
    |> Seq.append (mapRatingToMetaTag searchOpt.Rating |> Option.toList)
    |> Seq.append (mapOrder searchOpt.Order |> Option.toList)
    |> Seq.fold (fun a b -> a + " " + b) ""
    |> String.trim


let limit = 500


type MoebooruSource (opts) =
    let sourceUrlGen = opts.SourceUrlGen opts.BaseUrl
    let mapPost this = mapPost opts.HttpsOpts this sourceUrlGen
    let requestPosts' this f = 
        enumAllPages <| fun pageId -> 
            requestPosts 
                (PostListJson.AsyncLoad(f pageId))
                (Result.map Array.toList)
                (mapPost this)

    let requestTags (urlPostfix: string) =
        asyncSeq {
            match! 
                requestPosts 
                    (JsonValue.AsyncLoad($"{opts.BaseUrl}/tag.json?limit=0{urlPostfix}"))
                    (Result.map (JsonExtensions.AsArray >> Array.toList))
                    (fun json -> Result.protect <| json.["name"].AsString)
            with
            | Ok x -> yield! AsyncSeq.ofSeq x
            | Error e -> yield Error e
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
                        (Result.map Array.toList)
                        (mapPost this)
                with
                | Ok x -> return Ok <| Seq.tryHead x
                | Error e -> return Error e
            }
            
    interface ISearch with
        member this.Search search =
            requestPostsWithPostfix this $"&tags={mapSearchOptions search}"

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
      HttpsOpts = HttpsOptions.Empty
      StartPageIndex = 1 }


let konachan = create <| konachanLike "Konachan" "https://konachan.com"
let yandere = create <| konachanLike "Yandere" "https://yande.re"
let lolibooru' = create <| konachanLike "Lolibooru" "https://lolibooru.moe"
let hypnohub = create { konachanLike "HypnoHub" "https://hypnohub.net" with PostListJson = "/post/index.json" }

let lolibooru = 
    let inner = create <| konachanLike "Lolibooru" "https://lolibooru.moe"
    { new ISource with
        member _.Name = inner.Name
        member _.AllPosts = inner.AllPosts
      interface IGetPostById with
        member _.GetPostById x = (inner :?> IGetPostById).GetPostById x
      interface ISearch with
        member _.Search x = (inner :?> ISearch).Search x }

let sources =
    [ konachan
      yandere
      lolibooru
      hypnohub ]
    
