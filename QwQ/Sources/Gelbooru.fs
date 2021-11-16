module QwQ.Sources.Gelbooru

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open QwQ.Sources.Danbooru


type PostListJson = JsonProvider<"./Sources/GelbooruSample.json">


let mapPost src baseUrl imgServerBaseUrl (json: PostListJson.Root) =
    { Id = uint64 json.Id
      Source = src
      Rating = mapRating json.Rating
      SourceUrl = 
          asyncSeq {
              $"{baseUrl}/index.php?page=post&s=view&id={json.Id}"
              yield!
                  json.Source
                  |> Option.bind String.nullOrWhitespace
                  |> Option.toList
                  |> AsyncSeq.ofSeq
          }
          
      Tags = parseTags json.Tags
      PreviewImage = 
          mapHttpsContent 
              HttpsOptions.Default 
              $"{imgServerBaseUrl}/thumbnails/{json.Directory}/thumbnail_{json.Hash}.jpg"
          |> Some
              
      Content = 
          asyncSeq {
              if json.Sample
              then $"{imgServerBaseUrl}/samples/{json.Directory}/sample_{json.Hash}.jpg"

              json.FileUrl
              |> Option.defaultValue $"{imgServerBaseUrl}/images/{json.Directory}/{json.Image}"
          }
          |> AsyncSeq.choose String.nullOrWhitespace
          |> AsyncSeq.map (mapHttpsContent HttpsOptions.Default)
          |> AsyncSeq.singleton }


let limit = 500

type GelbooruSource (name, baseUrl, imgSrvBaseUrl) =

    let requestPostListWithUrlPostfix this urlPostfix =
        enumAllPages <| fun pageId ->
            requestPosts 
                (PostListJson.AsyncLoad(
                    $"{baseUrl}/index.php?page=dapi&s=post&q=index&json=1&limit={limit}&pid={pageId}{urlPostfix}"))
                id
                (mapPost this baseUrl imgSrvBaseUrl)

    interface ISource with
        member _.Name = name
        member x.AllPosts = requestPostListWithUrlPostfix x ""

    interface ITags with
        member _.Tags =
            requestTags "tag" 
                (fun pid -> $"{baseUrl}/index.php?page=dapi&s=tag&q=index&json=1&limit={limit}&pid={pid}")


let gelbooru = GelbooruSource ("Gelbooru", "https://gelbooru.com", "https://img3.gelbooru.com") :> ISource
let tbib = GelbooruSource ("The Big ImageBoard (TBIB)", "https://tbib.org", "https://tbib.org") :> ISource
let safebooru = GelbooruSource ("Safebooru", "https://safebooru.org", "https://safebooru.org") :> ISource
let xbooru = GelbooruSource ("XBooru", "https://xbooru.com", "https://img.xbooru.com/") :> ISource
let rule34 = GelbooruSource ("Rule34", "https://rule34.xxx", "https://us.rule34.xxx/") :> ISource


let sources =
    [ gelbooru
      tbib
      safebooru
      xbooru
      rule34 ]

