module QwQ.Sources.SankakuComplex

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open QwQ.Sources.Danbooru

type PostListJson = JsonProvider<"./Sources/SankakuComplexSample.json">


let userAgent = """Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36 Edg/88.0.705.63"""


let httpsOption =
    { UserAgent = Some userAgent }

let fixUrlPrefix (x: string) =
    if x.StartsWith "//" then "https:" + x
    else x


let mapPost siteUrl src (json: PostListJson.Root) =
    { Id = uint64 json.Id
      Source = src
      Rating = mapRating json.Rating
      SourceUrl = 
          asyncSeq {
              $"{siteUrl}/post/show/{json.Id}"
              yield! 
                  json.Source
                  |> Option.bind String.nullOrWhitespace
                  |> Option.toList
                  |> AsyncSeq.ofSeq
          }

      Tags = 
          json.Tags 
          |> Array.choose (fun x -> 
              x.Name
              |> Option.bind String.nullOrWhitespace
              |> Option.orElse x.NameEn
              |> Option.bind String.nullOrWhitespace
              |> Option.orElse x.NameJa
              |> Option.bind String.nullOrWhitespace)
          |> AsyncSeq.ofSeq
            
      PreviewImage = 
          json.PreviewUrl 
          |> Option.bind String.nullOrWhitespace
          |> Option.map (fixUrlPrefix >> mapHttpsContent httpsOption)
          
      Content =
          asyncSeq {
              json.SampleUrl
              json.FileUrl
          }
          |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
          |> AsyncSeq.map (fixUrlPrefix >> mapHttpsContent httpsOption)
          |> AsyncSeq.singleton }


type SankakuComplexSource (name, siteUrl, apiUrl, limit) =

    let requestPostList this urlPostfix =
        enumAllPages <| fun pageId ->
            requestPosts 
                (PostListJson.AsyncLoad($"{apiUrl}?limit={limit}&page={pageId + 1}{urlPostfix}"))
                (function
                    | Error (:? System.Net.WebException as e) 
                        when e.Message.Contains "sign in to view more!" -> Ok [||]
                    | x -> x)
                (mapPost siteUrl this)
    
    interface ISource with
        member _.Name = name
        member x.AllPosts = requestPostList x ""

    interface ISearch with
        member x.Search search = 
            "&tags=" + mapSearchOptions { search with NonTags = [] }
            |> requestPostList x
            |> AsyncSeq.map (Result.map (
                Seq.filter (fun x -> 
                    let postTags = AsyncSeq.toBlockingSeq x.Tags
                    Seq.forall (fun nonTag -> Seq.forall ((<>) nonTag) postTags) search.NonTags)))


type SankakuChannelSource () =
    inherit SankakuComplexSource (
        "Sankaku Channel", 
        "https://chan.sankakucomplex.com",
        "https://capi-v2.sankakucomplex.com/posts",
        500)

    let requestTagList urlPostfix =
        requestTags <| fun p ->
            requestPosts
                (JsonValue.AsyncLoad($"https://capi-v2.sankakucomplex.com/tags?limit=500&page={p + 1}{urlPostfix}"))
                (Result.map JsonExtensions.AsArray)
                (fun json -> 
                    json.TryGetProperty "name"
                    |> Option.map JsonExtensions.AsString
                    |> Option.bind String.nullOrWhitespace
                    |> Option.orElse (json.TryGetProperty "name_en" |> Option.map JsonExtensions.AsString)
                    |> Option.bind String.nullOrWhitespace
                    |> Option.orElse (json.TryGetProperty "name_jp" |> Option.map JsonExtensions.AsString)
                    |> Ok)
        |> AsyncSeq.choose (function
            | Ok None -> None
            | Ok (Some x) -> Some <| Ok x
            | Error e -> Some <| Error e)
            

    interface IGetPostById with
        member x.GetPostById id =
            async {
                match!
                    (x :> ISearch).Search 
                        { Tags = [$"id:{id}"]
                          NonTags = []
                          Order = Default
                          Rating = [ Safe; Questionable; Explicit ] }
                    |> AsyncSeq.tryFirst
                with
                | Some p -> return Result.map Seq.tryHead p
                | None -> return Ok None
            }

    interface ITags with
        member _.Tags = requestTagList ""

    interface ISearchTag with
        member _.SearchTag s = requestTagList $"&name={s}"
    

let sankakuChannel = SankakuChannelSource () : ISource
let idolComplex = 
    SankakuComplexSource (
        "Idol Complex", 
        "https://idol.sankakucomplex.com/",
        "https://iapi.sankakucomplex.com/post/index.json",
        60) :> ISource


let sources =
    [ sankakuChannel
      idolComplex ]