module QwQ.Sources.Danbooru

open FSharp.Control
open FSharp.Data
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


type PostListJson = JsonProvider<"./Sources/DanbooruSample.json">


let mapPost src baseUrl (json: PostListJson.Root) =
    { Id = json.Id |> Option.defaultValue 0 |> uint64
      Title = None
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
          |> Option.map (mapHttpsContent HttpsOptions.Empty)

      Content = 
          asyncSeq {
              json.LargeFileUrl
              json.FileUrl
          } 
          |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
          |> AsyncSeq.map (mapHttpsContent HttpsOptions.Empty)
          |> AsyncSeq.singleton
    }


let danbooruMapError: Result<_, exn> -> Result<_, exn> =
    function
    | Ok x -> Ok x
    | Error (:? System.Net.WebException) -> Ok [||]
    | Error e -> Error e


let limit = 500


let requestTags pageLoader =
    let response =
        enumAllPages pageLoader
            
    let exns = 
        AsyncSeq.choose (function Ok _ -> None | Error e -> Some e) response
        |> AsyncSeq.map Error

    let oks = 
        AsyncSeq.choose (function Ok x -> Some x | Error e -> None) response
        |> AsyncSeq.concatSeq
        |> AsyncSeq.map Ok
    
    AsyncSeq.append oks exns
    |> AsyncSeq.map (Result.bind id)


let requestTagsJson tagKey jsonUrlFromPage = 
    requestTags <| fun pageId ->
        requestPosts 
            (async {
                let! str = Http.AsyncRequestString (jsonUrlFromPage pageId, headers = [])
                return JsonValue.Parse str })
            (function
                | Ok x -> Ok <| Array.toList (x.AsArray())
                | Error (:? System.Net.WebException) -> Ok []
                | Error e -> Error e)
            (fun x -> Result.protect (fun () -> x.[tagKey: string].AsString()))


type DanbooruSource (name, baseUrl, danbooruLimit) =

    let mapJson =
        if danbooruLimit
        then danbooruMapError
        else id
        >> Result.map List.ofArray

    let requestPosts' this url =
        requestPosts 
            (async {
                let! str = Http.AsyncRequestString (url, headers = [])
                return PostListJson.Parse str })
            mapJson
            (mapPost this baseUrl)

    let requestPostsWithUrlPostfix this p =
       enumAllPages <| fun pageId ->
            requestPosts' this $"{baseUrl}/posts.json?limit={limit}&page={pageId + 1}{p}"

    let responseTagsWithUrlPostfix p =
        requestTagsJson "name" (fun pageId -> $"{baseUrl}/tags.json?limit=1000&page={pageId + 1}{p}")

    let danbooruSearcher this searchOpts =
        let tags = 
            mapOrder searchOpts.Order 
            |> Option.toList
            |> fun x -> List.append x <| Seq.toList searchOpts.Tags

        let firstTwoTags = List.truncate 2 tags
        let otherTags = try List.skip 2 tags with _ -> []

        List.fold (fun a b -> a + " " + b) "" firstTwoTags
        |> (+) "&tags="
        |> requestPostsWithUrlPostfix this 
        |> AntiGuro.antiThat searchOpts.ExludeTags
        |> AsyncSeq.map (
            Result.map (
                List.filter (fun x -> 
                    if searchOpts.Rating = Unrated then true else x.Rating = searchOpts.Rating
                    && Seq.forall (fun tagNeedToHave -> List.contains tagNeedToHave x.Tags) otherTags)
            )
        )

    let normalSearcher this = 
        mapSearchOptions
        >> (+) "&tags="
        >> requestPostsWithUrlPostfix this

    let searcher =
        if danbooruLimit
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


let danbooru = DanbooruSource ("Danbooru", "https://danbooru.donmai.us", true) :> ISource
let atfbooru = DanbooruSource ("ATFBooru", "https://booru.allthefallen.moe", false) :> ISource
let sonohara = DanbooruSource ("Sonohara", "https://sonohara.donmai.us", true) :> ISource
let hijiribe = DanbooruSource ("Hijiribe", "https://hijiribe.donmai.us", true) :> ISource
let safebooruDonmai = DanbooruSource ("Safebooru Donmai", "https://safebooru.donmai.us", true) :> ISource


let sources = 
    [ danbooru
      atfbooru
      sonohara
      hijiribe
      safebooruDonmai ]
