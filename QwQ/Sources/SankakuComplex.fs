module QwQ.Sources.SankakuComplex

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


type PostListJson = JsonProvider<"./Sources/SankakuComplexSample.json">


let userAgent = """Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36 Edg/88.0.705.63"""


let httpsOption =
    { UserAgent = Some userAgent }


let mapPost baseUrl src (json: PostListJson.Root) =
    { Id = uint64 json.Id
      Source = src
      Rating = mapRating json.Rating
      SourceUrl = 
          asyncSeq {
              $"{baseUrl}/post/show/{json.Id}"
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
          |> Option.map (mapHttpsContent httpsOption)
          
      Content =
          asyncSeq {
              json.SampleUrl
              json.FileUrl
          }
          |> AsyncSeq.choose (Option.bind String.nullOrWhitespace)
          |> AsyncSeq.map (mapHttpsContent httpsOption)
          |> AsyncSeq.singleton }

