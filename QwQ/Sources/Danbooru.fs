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


let requestPosts source baseUrl url =
    async {
        let! json = 
            Async.protect (PostListJson.AsyncLoad(url))
            |> Async.retryResult 3 1500

        return 
            json
            |> function
                | Ok x -> Ok x
                | Error (:? System.Net.WebException) -> Ok [||]
                | Error e -> Error e
            |> Result.map (
                Seq.choose (fun json ->
                    Option.protect (fun () ->
                        mapPost source baseUrl json)))
    } : Async<Result<PostPage, exn>>


let limit = 500


type DanbooruSource (name, url) =

    interface ISource with
        member _.Name = name
        member this.AllPosts =
            enumAllPages <| fun pageId ->
                requestPosts this url $"{url}/posts.json?limit={limit}&page={pageId + 1}"
    
    interface ITags with 
        member _.Tags = 
            let response =
                enumAllPages <| fun pageId ->
                    async {
                        let! json = 
                            JsonValue.AsyncLoad($"{url}/tags.json?limit=1000&page={pageId + 1}")
                            |> Async.protect
                            |> Async.retryResult 3 1500

                        return
                            json
                            |> Result.map (
                                JsonExtensions.AsArray
                                >> Seq.choose (fun x -> Option.protect (fun () -> x.["name"].AsString()))
                            )
                    }
            
            let exns = 
                AsyncSeq.choose (function Ok _ -> None | Error e -> Some e) response
                |> AsyncSeq.map Error

            let oks = 
                AsyncSeq.choose (function Ok x -> Some x | Error e -> None) response
                |> AsyncSeq.concatSeq
                |> AsyncSeq.map Ok
            
            AsyncSeq.append oks exns


let danbooru = DanbooruSource ("Danbooru", "https://danbooru.donmai.us") :> ISource
let atfbooru = DanbooruSource ("ATFBooru", "https://booru.allthefallen.moe/") :> ISource


let sources = 
    [ danbooru
      atfbooru ]
