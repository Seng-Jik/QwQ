﻿module QwQ.Sources.NHentaiSharp

open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru
open NHentaiSharp.Core
open NHentaiSharp.Search
open FSharp.Control


let mapElement src (e: GalleryElement) =
    { Id = uint64 e.id
      Title = 
          String.nullOrWhitespace e.japaneseTitle
          |> Option.orElse (String.nullOrWhitespace e.prettyTitle)
          |> Option.orElse (String.nullOrWhitespace e.englishTitle)

      Source = src 
      Rating = Explicit
      SourceUrl = e.url.ToString() |> AsyncSeq.singleton
      Tags = e.tags |> Array.map (fun x -> x.name)
      PreviewImage = 
          e.thumbnail.imageUrl.ToString()
          |> String.nullOrWhitespace
          |> Option.map (mapHttpsContent HttpsOptions.Default)

      Content = 
          e.pages
          |> Array.map (fun x -> AsyncSeq.singleton <| mapHttpsContent HttpsOptions.Default (x.imageUrl.ToString()))
          |> AsyncSeq.ofSeq }


let mapResult src (result: SearchResult) =
    result.elements
    |> Array.map (mapElement src)
    |> Array.toSeq


let processErrs def : Result<'a, exn> -> Result<'a, exn> =
    function
    | Ok x -> Ok x
    | Error (:? NHentaiSharp.Exception.EmptySearchException) -> Ok def
    | Error x -> Error x


type NHentaiSharp () =  

    interface ISource with
        member _.Name = "NHentai"
        member this.AllPosts = 
            enumAllPages <| fun pageId ->
                async {
                    let! result = 
                        Async.ofDelayedTask (fun () -> SearchClient.SearchAsync (pageId + 1))
                        |> Async.protect

                    return result |> Result.map (mapResult this) |> processErrs []
                }

    interface IGetPostById with
        member this.GetPostById id =
            async {
                let! result = 
                    Async.ofDelayedTask (fun () -> SearchClient.SearchByIdAsync <| int id)
                    |> Async.protect

                return result |> Result.map (mapElement this >> Some) |> processErrs None
            }

    interface ISearch with
        member this.Search x =
            let tags =
                seq {
                    yield! x.Tags
                    yield! 
                        x.NonTags
                        |> Seq.map SearchClient.GetExcludeTag
                } |> Array.ofSeq

            enumAllPages <| fun pageId ->
                async {
                    let! result =
                        Async.ofDelayedTask (fun () -> SearchClient.SearchWithTagsAsync(tags, pageId + 1))
                        |> Async.protect
                    
                    return result |> Result.map (mapResult this) |> processErrs []
                }


let nhentai = NHentaiSharp () :> ISource


let sources = [ nhentai ]

