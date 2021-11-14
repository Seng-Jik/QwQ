module QwQ.Sources.Konachan

open FSharp.Control
open FSharp.Data
open QwQ


type PostListJson = JsonProvider<"https://konachan.net/post.json">


let mapRating =
    function
    | "s" -> Safe
    | "q" -> Questionable
    | "e" -> Explicit
    | x -> Rating' x


let mapPost source sourceUrlGen (post: PostListJson.Root) =
    { Id = uint64 post.Id
      Source = source 

      Rating = async { return mapRating post.Rating }
      SourceUrl = async { return sourceUrlGen (uint64 post.Id) }
      Tags = post.Tags.Split(' ') |> AsyncSeq.ofSeq
      
      PreviewImage = post.Preview
      Images = AsyncSeq.empty }