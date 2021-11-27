namespace QwQ

open FSharp.Control


type PostId = uint64


type Tag = string


type Rating =
    | Safe
    | Questionable
    | Explicit
    | Unrated


type HttpsOptions =
    { Headers: (string * string) list }
    with 
        static member DefaultUserAgent = 
            """Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.55 Safari/537.36 Edg/96.0.1054.34"""
        static member Default = 
            { Headers = [ "User-Agent", HttpsOptions.DefaultUserAgent ] }


type DownloadMethod =
    | Https of url: string * opt: HttpsOptions


type Content =
    { FileName: string
      DownloadMethod: DownloadMethod }


type Mipmaps = AsyncSeq<Content>


type Post =
    { Id: PostId
      Title: string option
      Source: ISource
      
      Rating: Rating
      SourceUrl: AsyncSeq<string>
      Tags: Tag list
      
      PreviewImage: Content option
      Content: AsyncSeq<Mipmaps> }


and PostPage = Post list


and ISource = 
    abstract Name: string
    abstract AllPosts: AsyncSeq<Result<PostPage, exn>>


type IGetPostById =
    abstract GetPostById: PostId -> Async<Result<Post option, exn>>


module Source =
    let name (source: ISource) = source.Name
    let allPosts (source: ISource) = source.AllPosts
    let getPostById (source: IGetPostById) id = source.GetPostById id