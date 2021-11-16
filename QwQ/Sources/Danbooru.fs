module QwQ.Sources.Danbooru

open FSharp.Control
open FSharp.Data
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


type PostListJson = JsonProvider<"./Sources/DanbooruSample.json">


let mapPost src baseUrl (json: PostListJson.Root) =
    { Id = json.Id |> Option.defaultValue 0 |> uint64
      Source = src

      Rating = mapRating json.Rating
      SourceUrl = 
          asyncSeq {
              yield $"{baseUrl}/posts/{json.Id}"
              if not <| System.String.IsNullOrWhiteSpace json.Source
              then yield json.Source
          }

      Tags = json.TagString.Split(' ') |> AsyncSeq.ofSeq
      PreviewImage = 
          json.PreviewFileUrl
          |> Option.bind String.nullOrWhitespace
          |> Option.map (mapHttpsContent HttpsOptions.Default)

      Content = 
          asyncSeq { 
              asyncSeq {
                  yield json.LargeFileUrl
                  yield json.FileUrl
              } 
              |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
              |> AsyncSeq.map (mapHttpsContent HttpsOptions.Default)
          }
    }


let danbooruMapError: Result<_, exn> -> Result<_, exn> =
    function
    | Ok x -> Ok x
    | Error (:? System.Net.WebException) -> Ok [||]
    | Error e -> Error e


let limit = 500


type DanbooruSource (name, baseUrl) =

    let mapJson =
        if name = "Danbooru" 
        then danbooruMapError
        else id

    let requestPosts' this url =
        requestPosts 
            (PostListJson.AsyncLoad(url))
            mapJson
            (mapPost this baseUrl)

    let requestPostsWithUrlPostfix this p =
        enumAllPages <| fun pageId ->
            requestPosts' this $"{baseUrl}/posts.json?limit={limit}&page={pageId + 1}{p}"

    let responseTagsWithUrlPostfix p =
        let response =
            enumAllPages <| fun pageId ->
                requestPosts 
                    (JsonValue.AsyncLoad($"{baseUrl}/tags.json?limit=1000&page={pageId + 1}{p}"))
                    (Result.map JsonExtensions.AsArray)
                    (fun x -> Option.protect (fun () -> x.["name"].AsString()))
            |> AsyncSeq.map (Result.map (Seq.choose id))

        let exns = 
            AsyncSeq.choose (function Ok _ -> None | Error e -> Some e) response
            |> AsyncSeq.map Error

        let oks = 
            AsyncSeq.choose (function Ok x -> Some x | Error e -> None) response
            |> AsyncSeq.concatSeq
            |> AsyncSeq.map Ok
        
        AsyncSeq.append oks exns

    let danbooruSearcher this searchOpts =
        let tags = 
            mapOrder searchOpts.Order 
            |> Option.toList
            |> Seq.append searchOpts.Tags

        Seq.fold (fun a b -> a + " " + b) "" tags
        |> (+) "?tags="
        |> requestPostsWithUrlPostfix this 
        |> AsyncSeq.map (
            Result.map (
                Seq.filter (fun x -> 
                    let postTags = AsyncSeq.toBlockingSeq x.Tags
                    Seq.exists ((=) x.Rating) searchOpts.Rating
                    && Seq.forall (fun nonTag -> Seq.forall ((<>) nonTag) postTags) searchOpts.NonTags)
            )
        )

    let normalSearcher this = 
        mapSearchOptions
        >> (+) "?tags="
        >> requestPostsWithUrlPostfix this

    let searcher =
        if name = "Danbooru"
        then danbooruSearcher
        else normalSearcher

    interface ISource with
        member _.Name = name
        member this.AllPosts = requestPostsWithUrlPostfix this ""      

    interface ISearch with
        member this.Search searchOpts = searcher this searchOpts
            
    interface ITags with 
        member _.Tags = responseTagsWithUrlPostfix ""
            
    interface IGetPostById with
        member this.GetPostById id = 
            async {
                match! requestPosts' this $"{baseUrl}/posts.json?tags=id:{id}" with
                | Ok x -> return Ok <| Seq.tryHead x
                | Error e -> return Error e
            }

    interface ISearchTag with
        member _.SearchTag p = responseTagsWithUrlPostfix $"&search[fuzzy_name_matches]={p}"


let danbooru = DanbooruSource ("Danbooru", "https://danbooru.donmai.us") :> ISource
let atfbooru = DanbooruSource ("ATFBooru", "https://booru.allthefallen.moe/") :> ISource


let sources = 
    [ danbooru
      atfbooru ]
