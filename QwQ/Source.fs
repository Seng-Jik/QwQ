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
      
      Rating: Rating 
      SourceUrl: string list
      Tags: Tag list
      
      PreviewImage: Content option
      Images: Content list }


and PostPage = Post list


and ISource = 
    abstract Name: string
    abstract AllPosts: AsyncSeq<PostPage>



