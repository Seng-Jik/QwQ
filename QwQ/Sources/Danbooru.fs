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
              $"{baseUrl}/posts/{json.Id}"
              if not <| System.String.IsNullOrWhiteSpace json.Source
              then json.Source
          }

      Tags = parseTags json.TagString
      PreviewImage = 
          json.PreviewFileUrl
          |> Option.bind String.nullOrWhitespace
          |> Option.map (mapHttpsContent HttpsOptions.Default)

      Content = 
          asyncSeq {
              json.LargeFileUrl
              json.FileUrl
          } 
          |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
          |> AsyncSeq.map (mapHttpsContent HttpsOptions.Default)
          |> AsyncSeq.singleton
    }


let danbooruMapError: Result<_, exn> -> Result<_, exn> =
    function
    | Ok x -> Ok x
    | Error (:? System.Net.WebException) -> Ok [||]
    | Error e -> Error e


let limit = 500


let requestTags tagKey jsonUrlFromPage =
    let response =
        enumAllPages <| fun pageId ->
            requestPosts 
                (JsonValue.AsyncLoad(jsonUrlFromPage pageId))
                (Result.map JsonExtensions.AsArray)
                (fun x -> Result.protect (fun () -> x.[tagKey: string].AsString()))

    let exns = 
        AsyncSeq.choose (function Ok _ -> None | Error e -> Some e) response
        |> AsyncSeq.map Error

    let oks = 
        AsyncSeq.choose (function Ok x -> Some x | Error e -> None) response
        |> AsyncSeq.concatSeq
        |> AsyncSeq.map Ok
    
    AsyncSeq.append oks exns
    |> AsyncSeq.map (Result.bind id)


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
        requestTags "name" (fun pageId -> $"{baseUrl}/tags.json?limit=1000&page={pageId + 1}{p}")

    let danbooruSearcher this searchOpts =
        let tags = 
            mapOrder searchOpts.Order 
            |> Option.toList
            |> Seq.append searchOpts.Tags

        Seq.fold (fun a b -> a + " " + b) "" tags
        |> (+) "&tags="
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
        >> (+) "&tags="
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
