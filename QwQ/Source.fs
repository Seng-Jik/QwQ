namespace QwQ

open FSharp.Control


type PostId = uint64


type Tag = string


type Rating =
    | Safe
    | Questionable
    | Explicit
    | Rating of string


type DownloadMethod =
    | Https of url: string * userAgent: string


type Content =
    { FileName: string
      DownloadMethod: DownloadMethod }


type Post =
    { Id: PostId
      Source: ISource
      
      Rating: Async<Rating>
      SourceUrl: Async<string list>
      Tags: Async<Tag list>
      
      PreviewImage: Content option
      Images: AsyncSeq<Content> }


and PostPage = AsyncSeq<Post>


and ISource = 
    abstract Name: string
    abstract AllPosts: AsyncSeq<PostPage>


type IGetByPostId =
    inherit ISource
    abstract GetPostById: PostId -> Async<Post option>

    