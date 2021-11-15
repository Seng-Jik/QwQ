namespace QwQ

open FSharp.Control


type PostId = uint64


type Tag = string


type Rating =
    | Safe
    | Questionable
    | Explicit
    | Rating' of string


type HttpsOptions =
    { UserAgent: string option }
    with 
        static member Default = 
            { UserAgent = None }


type DownloadMethod =
    | Https of url: string * opt: HttpsOptions


type Content =
    { FileName: string
      DownloadMethod: DownloadMethod }


type Post =
    { Id: PostId
      Source: ISource
      
      Rating: Async<Rating>
      SourceUrl: AsyncSeq<string>
      Tags: AsyncSeq<Tag>
      
      PreviewImage: Content option
      Content: AsyncSeq<Content> }


and PostPage = Post seq


and ISource = 
    abstract Name: string
    abstract AllPosts: AsyncSeq<Result<PostPage, exn>>


type IGetByPostId =
    inherit ISource
    abstract GetPostById: PostId -> Async<Result<Post option, exn>>


module Source =
    let name (source: ISource) = source.Name
    let allPosts (source: ISource) = source.AllPosts