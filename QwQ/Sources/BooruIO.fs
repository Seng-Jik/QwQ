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
    json.Cursor,
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
        enumAllPagesWithState 0 <| fun cursor _ ->
            async {
                let! postJson = 
                    PostListJson.AsyncLoad 
                        $"https://booru.io/api/legacy/query/entity?cursor={cursor}{postfix}"
                    |> Async.protect

                return Result.map (mapPostListJson this) postJson
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


let booruio = BooruIOSource () :> ISource

