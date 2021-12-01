module QwQ.Sources.BooruIO

open FSharp.Data
open FSharp.Control
open QwQ
open QwQ.Utils
open QwQ.Sources.Moebooru


type PostListJson = JsonProvider<"https://booru.io/api/legacy/query/entity">


let parseTransformTitleToWidth (title: string) =
    Option.protect (fun () ->
        let mutable title = title
        let widthEquals = title.IndexOf "width="
        if widthEquals >= 0 then title <- title.[String.length "width=" ..]
        
        let ends = title.IndexOf ':'
        title <- title.[..ends - 1]

        int64 title)
    |> Option.defaultValue System.Int64.MaxValue


let mapPostListJson this (json: PostListJson.Root) =
    json.JsonValue.TryGetProperty "cursor" |> Option.map (fun x -> x.AsInteger()),
    [ for post in json.Data ->
        let transforms = 
            post.Transforms.JsonValue.Properties()
            |> Array.sortBy (fst >> parseTransformTitleToWidth)
            |> Array.map (
                snd >> 
                (fun x -> x.AsString()) 
                >> (+) "https://booru.io/api/legacy/data/" 
                >> mapHttpsContent HttpsOptions.Empty)

        let preview = Array.tryHead transforms

        let content = 
            if Array.length transforms >= 1 
            then Seq.skip 1 transforms
            else Seq.empty

        { Id = post.Key.GetHashCode() |> uint64
          Title = None
          Source = this
          Rating = Unrated
          SourceUrl = AsyncSeq.singleton <| $"https://booru.io/p/{post.Key}"
          Tags = post.Tags.JsonValue.Properties() |> Seq.map fst |> Seq.toList
          PreviewImage = preview
          Content = 
              content
              |> AsyncSeq.ofSeq
              |> AsyncSeq.singleton } ]


type BooruIOSource () =
    
    let enumAllPosts this postfix =
        enumAllPagesWithState (Some 0) <| fun cursor _ ->
            async {
                match cursor with
                | Some cursor ->
                    let! postJson = 
                        PostListJson.AsyncLoad 
                            $"https://booru.io/api/legacy/query/entity?cursor={cursor}{postfix}"
                        |> Async.protect

                    return Result.map (mapPostListJson this) postJson
                | None -> return Ok (None, [])
            }

    interface ISource with
        member _.Name = "Booru.io"
        member x.AllPosts = enumAllPosts x ""

    interface ISearch with
        member this.Search opt =
            opt.Tags 
            |> Seq.fold (fun a b -> a + " " + b) "" 
            |> String.trim
            |> (+) "&query="
            |> enumAllPosts this

    interface ISearchTag with
        member _.SearchTag tag =
            asyncSeq {
                let! results = 
                    JsonValue.AsyncLoad $"https://booru.io/api/legacy/query/tag?prefix={tag}"
                    |> Async.map (fun x -> 
                        x.TryGetProperty "data"
                        |> Option.map (fun x -> x.Properties())
                        |> Option.defaultValue [||]
                        |> Array.map fst)
                    |> Async.protect
                    |> Async.map (function
                        | Ok x -> Seq.map Ok x |> AsyncSeq.ofSeq
                        | Error x -> AsyncSeq.singleton <| Error x)

                yield! results
            }


let booruio = BooruIOSource () :> ISource

