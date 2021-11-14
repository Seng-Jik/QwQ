module QwQ.Sources.Konachan

open FSharp.Control
open FSharp.Data
open QwQ
open QwQ.Utils


type PostListJson = JsonProvider<"https://konachan.net/post.json">


let mapRating =
    function
    | "s" -> Safe
    | "q" -> Questionable
    | "e" -> Explicit
    | x -> Rating' x


let getFileNameFromUrl (url: string) =
    let url = url.[..url.IndexOf '?' - 1]
    url.[url.LastIndexOf '/' + 1 ..]


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


type KonachanSourceOptions =
  { Name: string
    BaseUrl: string
    PostListJson: string
    SourceUrlGen: string -> PostId -> string
    HttpsOpts: HttpsOptions }


let enumAllPages getPage =
    let rec enumPages errors curPage =
        asyncSeq {
            match! getPage curPage with
            | Ok x when Seq.isEmpty x -> yield Ok (x: PostPage)
            | Ok x -> 
                yield Ok x
                yield! enumPages 0 (curPage + 1)
            | Error x when errors < 3 -> 
                yield Error x
                yield! enumPages (errors + 1) (curPage + 1)
            | Error x -> yield Error x
        }
    
    enumPages 0 0


type KonachanSource (opts) =
    let sourceUrlGen = opts.SourceUrlGen opts.BaseUrl

    interface ISource with
        member _.Name = opts.Name
        member this.AllPosts = 
            enumAllPages <| fun pageId ->
                requestPosts 
                    opts.HttpsOpts
                    this
                    sourceUrlGen
                    $"{opts.BaseUrl}{opts.PostListJson}"


let create x = KonachanSource (x) :> ISource


let konachan =
    create 
        { Name = "Konachan"
          BaseUrl = "https://konachan.net"
          PostListJson = "/post.json"
          SourceUrlGen = sprintf "%s/post/show/%d"
          HttpsOpts = HttpsOptions.Default }


let sources =
    [ konachan ]
    
